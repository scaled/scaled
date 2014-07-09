//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.impl

import java.nio.file.Paths
import reactual.{OptValue, ReactionException, Signal, SignalV, Value, ValueV}
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
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
    val lines = ArrayBuffer[Array[Char]]()
    // TODO: remove tab hackery when we support tabs
    store.readLines(ln => lines += ln.replace('\t', ' ').toCharArray)
    // TEMP: tack a blank line on the end to simulate a trailing line sep
    lines += MutableLine.NoChars
    new BufferImpl(store, lines)
  }

  /** Returns a blank buffer to be used by scratch views (e.g. the minibuffer). */
  def scratch (name :String) :BufferImpl = apply(new TextStore(name, cwd.toString, ""))

  /** An empty line sequence used for edits that delete no lines. */
  private final val NoLines = Seq[Line]()
}

/** Implements [Buffer] and [RBuffer]. This is where all the excitement happens. */
class BufferImpl private (initStore :Store, initLines :ArrayBuffer[Array[Char]]) extends RBuffer {
  import Buffer._

  // TODO: character encoding
  // TODO: line endings

  private[this] val _lines = initLines.map(new MutableLine(this, _))
  private[this] val _name = Value(initStore.name)
  private[this] val _store = Value(initStore)
  private[this] val _mark = Value(None :Option[Loc])
  private[this] val _editable = Value(true)
  private[this] val _dirty = Value(false)
  private[this] val _willSave = Signal[Buffer]()
  private[this] val _edited = Signal[Edit]()
  private[this] val _lineStyled = Signal[Loc]()
  private[this] var _maxRow = longest(_lines, 0, _lines.length)

  val undoStack = new UndoStack(this)

  //
  // from Buffer and RBuffer API

  override def nameV = _name
  override def storeV = _store
  override def markV = _mark
  override def mark_= (loc :Loc) = _mark() = Some(bound(loc))
  override def clearMark () = _mark() = None
  override def undoer = undoStack
  override def edited = _edited
  override def lineStyled = _lineStyled
  override def lines = _lines
  override def maxLineLength = lines(_maxRow).length
  // refine `dirtyV` return type to ease life for internal friends
  override def dirtyV :Value[Boolean] = _dirty
  override def editableV = _editable
  override def editable_= (editable :Boolean) = _editable() = editable
  override def willSave = _willSave
  override def line (idx :Int) :MutableLine = _lines(idx)
  override def line (loc :Loc) :MutableLine = _lines(loc.row)

  override def saveTo (store :Store) {
    if (store.readOnly) throw Errors.feedback(s"Cannot save to read-only file: $store")
    // run our on-save hooks, but don't let them abort the save if they choke
    val exn = try {
      _willSave.emit(this) ; null
    } catch {
      case e :Exception => e
    }
    store.write(lines)
    _name() = store.name
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
  override def insert (loc :Loc, region :Seq[LineV]) = region.size match {
    case 0 => loc
    case 1 => insert(loc, region.head)
    case _ =>
      // split the first line in twain
      val tail = line(loc).split(loc)
      // merge the first half of the split line with the first line of the region
      line(loc).append(loc, region.head)
      // merge the last line of the region with the second half of the split line
      tail.insert(Loc.Zero, region.last, 0, region.last.length)
      // finally add the middle lines (unmodified) and the merged last line into the buffer
      val rest = region.slice(1, region.length-1).map(MutableLine(this, _)) :+ tail
      _lines.insertAll(loc.row+1, rest)
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

  override def replace (start :Loc, until :Loc, lines :Seq[LineV]) :Loc = {
    if (until < start) replace(until, start, lines) else {
      // if this is an exact replacement, handle it specially; this is mainly for efficient undoing
      // of transforms; overkill perhaps, but whatever, it's four lines of code
      if (until == start + lines) {
        val original = new ArrayBuffer[Line]()
        onRows(start, until) { (l, s, c) =>
          original += l.replace(s, c-s.col, lines(s.row-start.row))
        }
        noteTransform(start, original)
      } else {
        delete(start, until)
        insert(start, lines)
      }
    }
  }

  override def transform (start :Loc, until :Loc, fn :Char => Char) =
    if (until < start) transform(until, start, fn)
    else {
      val original = new ArrayBuffer[Line]()
      def xf (ln :MutableLine, start :Loc, end :Int) = {
        original += ln.slice(start.col, end)
        ln.transform(fn, start, end)
      }
      if (start.row == until.row) xf(line(start), start, until.col)
      else onRows(start, until)(xf)
      noteTransform(start, original)
    }

  override def split (loc :Loc) {
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

  //
  // impl details

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

  private def longest (lines :Seq[LineV], start :Int, end :Int) :Int = {
    @inline @tailrec def loop (cur :Int, max :Int, maxLen :Int) :Int =
      if (cur >= end) max
      else {
        val curLen = lines(cur).length
        if (curLen > maxLen) loop(cur+1, cur, curLen)
        else loop(cur+1, max, maxLen)
      }
    loop(start, 0, 0)
  }

  private def emit (edit :Edit) :Loc = {
    // println(edit)
    _dirty() = true
    _edited.emit(edit)
    edit.end
  }
  private def noteInsert (start :Loc, end :Loc) = {
    // shift max row, then check inserted lines for new longest line
    if (_maxRow >= start.row) _maxRow += (end.row - start.row)
    val editMaxRow = longest(_lines, start.row, end.row+1)
    // TODO: if the insertion is in the max row, it might have split the max row into two non-max
    // rows, in which case we have to rescan the whole buffer; sigh...
    if (lines(editMaxRow).length > maxLineLength) _maxRow = editMaxRow
    // if the insert preceded the mark, adjust it; note that the mark behaves differently than the
    // point in that an insert *at* the mark does not shift the mark, whereas an insert at the point
    // does shift the point
    if (_mark.get.isDefined && _mark.get.get > start) {
      mark = Loc.adjustForInsert(_mark.get.get, start, end)
    }
    emit(new Insert(start, end, this))
  }
  private def noteDelete (start :Loc, deleted :Seq[Line]) = {
    val edit = new Delete(start, deleted, this)
    // if our max row was later in the buffer than the deleted region, shift it
    if (_maxRow > edit.end.row) _maxRow -= (edit.end.row - edit.start.row)
    // otherwise if our old max row was in the deleted region, rescan buffer for new longest
    else if (_maxRow >= start.row) _maxRow = longest(_lines, 0, _lines.length)
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
