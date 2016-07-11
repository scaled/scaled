//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.io.File
import scala.collection.mutable.{Map => MMap}
import scaled._
import scaled.util.Errors

// TODO
//
// - Buffer.Anchor which demarks a point in a buffer which is automatically adjusted as the buffer
// is edited (and likely reports to listeners when its location changes)

/** [BufferImpl] related types and utilities. */
object BufferImpl {

  /** Reads the contents of `store` info a buffer. */
  def apply (store :Store) :BufferImpl = {
    val buf = new BufferImpl(store)
    // TODO: remove tab hackery when we support tabs
    store.readLines { (ln, off) => buf.addLine(ln.replace('\t', ' ').toCharArray) }
    // TEMP: tack a blank line on the end to simulate a trailing line sep
    buf.addLine(MutableLine.NoChars)
    buf
  }

  /** Returns a blank buffer to be used by scratch views (e.g. the minibuffer). */
  def scratch (name :String) :BufferImpl = apply(Store.scratch(name, cwd))

  /** Used to track the view state for a buffer when it's not visible. */
  case class ViewState (point :Loc, scrollTop :Int, scrollLeft :Int)

  /** An empty line sequence used for edits that delete no lines. */
  private final val NoLines = Seq[Line]()
}

/** Implements [Buffer] and [RBuffer]. This is where all the excitement happens. */
class BufferImpl private (initStore :Store) extends RBuffer {
  import Buffer._
  import BufferImpl._

  // TODO: character encoding
  // TODO: line endings

  private[this] val _lines = SeqBuffer[MutableLine]()
  private[this] val _name = Value(initStore.name)
  private[this] val _store = Value(initStore)
  private[this] val _mark = Value(None :Option[Loc])
  private[this] val _editable = Value(true)
  private[this] val _dirty = Value(false)
  private[this] val _willSave = Signal[Buffer]()
  private[this] val _stale = Signal[Buffer]()
  private[this] val _killed = Signal[Buffer]()
  private[this] val _edited = Signal[Edit]()
  private[this] val _lineStyled = Signal[Loc]()

  /** The "expected" last modified time of this buffer's backing store (if any).
    * Used to detect files changed by external programs. */
  private[this] var _lastModified :Long = initStore.lastModified

  val undoStack = new UndoStack(this)

  /** Contains the state of the most recent view of this buffer. When a view goes away, it writes
    * its current state to the buffer, and if the buffer is loaded into a new view, it restores the
    * most recently saved state. */
  var viewState :ViewState = ViewState(Loc.Zero, 0, 0)

  /** Checks whether this buffer has become stale (i.e. the file it is editing has been modified
    * more recently than it was loaded into this buffer). Emits [[stale]] if so. */
  def checkStale () {
    if (store.lastModified > _lastModified) _stale.emit(this)
  }

  //
  // from Buffer and RBuffer API

  override def nameV = _name
  override def storeV = _store
  override def stale = _stale
  override def killed = _killed
  override def markV = _mark
  override def mark_= (loc :Loc) = _mark() = Some(bound(loc))
  override def clearMark () = _mark() = None
  override def undoer = undoStack
  override def edited = _edited
  override def lineStyled = _lineStyled
  override def lines = _lines
  // refine `dirtyV` return type to ease life for internal friends
  override def dirtyV :Value[Boolean] = _dirty
  override def editableV = _editable
  override def editable_= (editable :Boolean) = _editable() = editable
  override def willSave = _willSave
  override def line (idx :Int) :MutableLine = _lines(idx)
  override def line (loc :Loc) :MutableLine = _lines(loc.row)

  // TODO: run buffer kill hooks
  override def kill () = killed.emit(this)

  override def saveTo (store :Store) {
    if (store.readOnly) throw Errors.feedback(s"Cannot save to read-only file: $store")
    // run our on-save hooks, but don't let them abort the save if they choke
    val exn = try {
      _willSave.emit(this) ; null
    } catch {
      case e :Exception => e
    }
    // write our contents to the store
    store.write(lines)
    // update our last modified time
    _lastModified = store.lastModified
    // if the store changed, update our name to the new store's name
    if (this.store != store) _name() = store.name
    _dirty() = false
    // now run our post-save hooks, and if they also fail, then tack any on-save hooks thereon
    try _store.updateForce(store)
    catch {
      case re :ReactionException => if (exn != null) re.addSuppressed(exn) ; throw re
    }
    // rethrow any on-save hook failure
    if (exn != null) throw exn
  }

  override def markClean () :Unit = _dirty() = false

  override def insert (loc :Loc, c :Char, syntax :Syntax) = {
    _lines(loc.row).insert(loc, c, syntax)
    noteInsert(loc, loc.nextC)
  }
  override def insert (loc :Loc, line :LineV) = {
    val end = _lines(loc.row).insert(loc, line)
    noteInsert(loc, end)
  }
  override def insert (loc :Loc, region :Ordered[_ <: LineV]) = region.size match {
    case 0 => loc
    case 1 => insert(loc, region.head)
    case _ =>
      // split the first line in twain
      val tail = line(loc).split(loc)
      // merge the first half of the split line with the first line of the region
      line(loc).append(loc, region.head)
      // merge the last line of the region with the second half of the split line and add the
      // merged last line into the buffer
      val iidx = loc.row+1
      tail.insert(Loc.Zero, region.last, 0, region.last.length)
      _lines.insert(iidx, tail)
      // then squeeze the middle lines (unmodified) in between
      _lines.insert(iidx, region.slice(1, region.length-1).map(MutableLine(this, _)))
      // note the edit, and return the location at the end of the inserted region
      noteInsert(loc, loc + region)
  }

  override def delete (loc :Loc, count :Int) = {
    val dline = line(loc).delete(loc, count)
    noteDelete(loc, Seq(dline))
    dline
  }
  override def delete (start :Loc, until :Loc) =
    if (until < start) delete(until, start)
    else if (start.row == until.row) Seq(delete(start, until.col-start.col))
    else {
      val (fst, last) = (line(start), line(until))
      // replace the deleted part of the first line with the retained part of the last line
      val head = fst.replace(start, fst.length-start.col, last.slice(until.col))
      // then delete all the intervening lines
      val middle = delete(start.row+1, until.row-start.row)
      // truncate the last deleted line at the until column, otherwise we'd erroneously report the
      // retained part of that line as part of the deleted region
      val deleted = head +: middle.dropRight(1) :+ middle.last.slice(0, until.col)
      noteDelete(start, deleted)
      deleted
    }

  override def replace (loc :Loc, count :Int, line :LineV) = {
    val dline = _lines(loc.row).replace(loc, count, line)
    noteDelete(loc, Seq(dline))
    noteInsert(loc, loc + (0, line.length))
    dline
  }

  override def replace (start :Loc, until :Loc, lines :Ordered[LineV]) :Loc = {
    if (until < start) replace(until, start, lines) else {
      // if this is an exact replacement, handle it specially; this is mainly for efficient undoing
      // of transforms; overkill perhaps, but whatever, it's four lines of code
      if (until == start + lines) {
        val orig = Seq.builder[Line](lines.length)
        val iter = lines.iterator()
        onRows(start, until) { (l, s, c) =>
          orig += l.replace(s, c-s.col, iter.next)
        }
        noteTransform(start, orig.build())
      } else {
        delete(start, until)
        insert(start, lines)
      }
    }
  }

  override def transform (start :Loc, until :Loc, fn :Char => Char) =
    if (until < start) transform(until, start, fn)
    else {
      val orig = Seq.builder[Line](until.row-start.row)
      def xf (ln :MutableLine, start :Loc, end :Int) = {
        orig += ln.slice(start.col, end)
        ln.transform(fn, start, end)
      }
      if (start.row == until.row) xf(line(start), start, until.col)
      else onRows(start, until)(xf)
      noteTransform(start, orig.build())
    }

  override def split (loc :Loc) = {
    _lines.insert(loc.row+1, line(loc).split(loc))
    noteInsert(loc, loc.nextStart)
  }

  override def setSyntax (syntax :Syntax, start :Loc, until :Loc) {
    if (until < start) setSyntax(syntax, until, start)
    else if (until > end) setSyntax(syntax, start, end) // bound until into the buffer
    else if (start.row == until.row) line(start).setSyntax(syntax, start, until.col)
    else onRows(start, until)(_.setSyntax(syntax, _, _))
  }

  override def addTag[T] (tag :T, start :Loc, until :Loc) {
    if (until < start) addTag(tag, until, start)
    else if (until > end) addTag(tag, start, end) // bound until into the buffer
    else if (start.row == until.row) line(start).addTag(tag, start, until.col)
    else onRows(start, until)(_.addTag(tag, _, _))
  }

  override def removeTag[T] (tag :T, start :Loc, until :Loc) {
    if (until < start) removeTag(tag, until, start)
    else if (until > end) removeTag(tag, start, end) // bound until into the buffer
    else if (start.row == until.row) line(start).removeTag(tag, start, until.col)
    else onRows(start, until)(_.removeTag(tag, _, _))
  }

  override def removeTags[T] (tclass :Class[T], pred :T => Boolean, start :Loc, until :Loc) {
    if (until < start) removeTags(tclass, pred, until, start)
    else if (until > end) removeTags(tclass, pred, start, end) // bound until into the buffer
    else if (start.row == until.row) line(start).removeTags(tclass, pred, start, until.col)
    else onRows(start, until)(_.removeTags(tclass, pred, _, _))
  }

  override def setLineTag[T <: Line.Tag] (idx :Int, tag :T) = line(idx).lineTagSet.set(tag)
  override def clearLineTag (idx :Int, key :Any) = line(idx).lineTagSet.clear(key)

  //
  // impl details

  // called by BufferImpl.apply when building a buffer
  private def addLine (line :Array[Char]) = _lines += new MutableLine(this, line)

  /** Applies op to all rows from `start` up to (not including) `until`. `op` is passed `(line,
    * start, endCol)` which is adjusted properly for the first and last line. */
  private def onRows (start :Loc, until :Loc)(op :(MutableLine, Loc, Int) => Unit) {
    var loc = start
    while (loc.row < until.row) {
      val l = _lines(loc.row)
      op(l, loc, l.length)
      loc = loc.nextStart
    }
    op(_lines(loc.row), loc, until.col)
  }

  private def delete (row :Int, count :Int) :Seq[Line] =
    if (count == 0) BufferImpl.NoLines
    else {
      val deleted = _lines.slice(row, row+count)
      _lines.remove(row, count)
      // TODO: variant of slice that relinquishes the char array instead of copying it; since we're
      // giving up these lines their data can be bequeathed to immutable lines
      deleted.map(_.slice(0))
    }

  private def emit (edit :Edit) :Loc = {
    // println(edit)
    _dirty() = true
    _edited.emit(edit)
    edit.end
  }
  private def noteInsert (start :Loc, end :Loc) = {
    // if the insert preceded the mark, adjust it; note that the mark behaves differently than the
    // point in that an insert *at* the mark does not shift the mark, whereas an insert at the
    // point does shift the point
    if (_mark.get.isDefined && _mark.get.get > start) {
      mark = Loc.adjustForInsert(_mark.get.get, start, end)
    }
    emit(new Insert(start, end, this))
  }
  private def noteDelete (start :Loc, deleted :Seq[Line]) = {
    val edit = new Delete(start, deleted, this)
    // if the delete preceded the mark, adjust it
    if (_mark.get.isDefined) mark = Loc.adjustForDelete(_mark.get.get, start, end)
    emit(edit)
  }
  private def noteTransform (start :Loc, orig :Seq[Line]) =
    emit(new Transform(start, orig, this))

  private[impl] def noteLineStyled (loc :Loc) {
    // println(s"Styles @$loc")
    _lineStyled.emit(loc)
  }

  override def toString () = s"[name=$name, store=$store, lines=${lines.size}]"
}
