//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.project

import java.io.File
import java.util.Date
import reactual.{Future, Value, ValueV}
import scala.annotation.tailrec
import scaled._
import scaled.util.Error

/** Provides services for a particular project. See [[ProjectService]] for a more detailed
  * description of what Scaled defines to be a project.
  */
abstract class Project {

  /** The history ring for file names in this project. */
  val fileHistory = new Ring(32) // TODO: how might we configure this?

  /** Completes files in this project. The string representation of the files should not be
    * prefixed with path information, but rather suffixed and only where necessary to avoid
    * name collisions.
    *
    * Thus one might see as possible completions: `Bar.scala Baz.scala(util/) Baz.scala(data/)`
    * When completing on `Ba`.
    */
  val fileCompleter :Completer[File]

  /** Returns the name of this project. */
  def name :String

  /** Returns the root of this project. */
  def root :File

  /** Returns a unique identifier for this project, if one can be determined. Generally this is
    * Maven-style: `groupId:name:version`, but a `Project` is welcome to use whatever makes sense
    * for the kind of projects it manages.
    */
  def id :Option[String] = None

  /** Returns the version control source URL for this project, if one can be determined. This should
    * be prefixed with the version control type if it is not already naturally part of the URL:
    *
    * - git:https://github.com/scaled/maven-project.git
    * - git:git@github.com:samskivert/samskivert.git
    * - hg:https://ooo-maven.googlecode.com/hg/
    * - svn:https://ooo-gwt-utils.googlecode.com/svn
    */
  def sourceURL :Option[String] = None

  /** Notes that buffer is now using this project. */
  def reference (buffer :Buffer) :this.type = {
    _refcount += 1
    this
  }

  /** Notes that buffer is no longer using this project. */
  def release (buffer :Buffer) {
    assert(_refcount > 0, s"$this released with zero refcount!")
    _refcount -= 1
    if (_refcount == 0) hibernate()
  }

  /** Advances the internal error pointer to the next error and visits that buffer in `editor`.
    * If we are at the end of the list, the user is informed via feedback that we have reached the
    * last error, and the internal counter is reset so that a subsequent request to visit the next
    * error will visit the first error.
    */
  def visitNextError (editor :Editor) {
    if (_compileErrs.isEmpty) editor.popStatus("No compilation errors.")
    else {
      _currentErr += 1
      if (_currentErr < _compileErrs.length) visitError(editor, _compileErrs(_currentErr))
      else {
        _currentErr = -1
        editor.emitStatus("At last error. Repeat command to start from first error.")
      }
    }
  }

  /** Regresses the internal error pointer to the previous error and visits that buffer in `editor`.
    * If we are at the start of the list, the user is informed via feedback that we have reached the
    * first error, and the internal counter is reset so that a subsequent request to visit the
    * previous error will visit the last error.
    */
  def visitPrevError (editor :Editor) {
    // TODO
  }

  /** Initiates a recompilation of this project, if supported.
    * @return a future which will report a summary of the compilation, or a failure if compilation
    * is not supported by this project.
    */
  def recompile (editor :Editor) {
    if (_compiler == null) _compiler = createCompiler()
    _compiler match {
      case None => editor.emitStatus("Compilation is not supported by this project.")
      case Some(comp) =>
        // create our compilation output buffer if necessary
        val buf = editor.createBuffer(s"*compile-$name*", "log" /*project-compile*/, true).buffer
        val start = System.currentTimeMillis
        buf.replace(buf.start, buf.end, Line.fromTextNL(s"Compilation started at ${new Date}..."))
        comp.compile(buf).onFailure(editor.emitError).onSuccess { msg =>
          // scan the results buffer for compiler errors
          val errs = Seq.newBuilder[Compiler.Error]
          @inline @tailrec def loop (loc :Loc) :Unit = comp.nextError(buf, loc) match {
            case Some((err, next)) => errs += err ; loop(next)
            case None => // done!
          }
          loop(buf.start)
          _currentErr = -1
          _compileErrs = errs.result
          val duration = (System.currentTimeMillis - start) / 1000
          buf.append(Line.fromTextNL(s"Completed in $duration s, at ${new Date}."))
          // report feedback to the user
          editor.popStatus(msg)
        }
        editor.emitStatus("Recompile initiated...")
    }
  }

  override def toString = s"Project($root, $name, $id, $sourceURL)"

  private def visitError (editor :Editor, err :Compiler.Error) {
    editor.visitFile(new File(err.path)).point() = err.loc
    editor.popStatus(err.descrip)
  }

  /** When a project is released by all project modes, it goes back into hibernation. This method
    * should shut down any complex services maintained by the project. The default implementation
    * shuts down any active compiler.
    */
  protected def hibernate () {
    if (_compiler != null) {
      _compiler foreach { _.shutdown() }
      _compiler = null
    }
  }

  /** If this mode supports a compiler, this should create and return a new compiler instance. */
  protected def createCompiler () :Option[Compiler] = None

  private[this] var _refcount = 0 // see reference/release

  // a reference to our active compiler, if one is resolved; a compiler is created on demand
  // when a project mode references this project and requests it; the compiler remains active
  // until all project modes relinquish the project, at which point it's shutdown
  private[this] var _compiler :Option[Compiler] = null

  private[this] var _compileErrs = Seq[Compiler.Error]()
  private[this] var _currentErr = -1
}
