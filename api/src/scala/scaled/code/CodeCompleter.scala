//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.code

import com.google.common.collect.TreeMultimap
import java.util.{HashMap, LinkedHashSet}
import scaled._
import scaled.util.Chars

/** Provides a pluggable mechanism for code completion. */
abstract class CodeCompleter {
  import CodeCompleter._

  /** Used to auto-trigger a completion when the user types a character. Defaults to triggering
    * complete on `.` but custom completers may override for different languages. */
  def shouldActivate (buffer :Buffer, point :Loc, typed :String) :Boolean =
    typed == "." && Chars.isWhitespace(buffer.charAt(point))

  /** Generates a list of completions given at the supplied completion `pos`. This may not be the
    * current buffer `point` (also supplied) if the user has manually requested to complete an
    * existing token (series of word characters). The completer may inspect the text between `pos`
    * and `point` and obtain its completion based on that prefix. */
  def completeAt (win :Window, buffer :Buffer, pos :Loc, point :Loc) :Future[Completion]
}

object CodeCompleter {

  /** Contains info on a single completion choice.
    * @param insert the text that is inserted into the buffer.
    */
  class Choice (val insert :String) {
    /** The label to display for this choice. Defaults to the `insert` text. */
    def label :String = insert
    /** An optional signature shown to the right of the completion in the list. */
    def sig :Option[LineV] = None
    /** Optional details to show when choice is active (e.g. method docs).
      * @param viewWidth the width of the current view (in characters) which should be used to
      * wrap the detail text to fit. */
    def details (viewWidth :Int) :Future[Option[Buffer]] = Future.success(None)
  }

  /** Encapsulates the result of a completion request.
    * @param start the start of the 'prefix' that was used by the completer.
    * @param choices the completions for the prefix previously specified.
    * @param index the index of the completion to use (generally starts at zero & is used by
    * CodeMode to track position when cycling through completions).
    */
  case class Completion (start :Loc, choices :SeqV[Choice], index :Int = 0)

  /** Returns a completer for use with `buffer`. */
  def completerFor (wspace :Workspace, buffer :Buffer) :CodeCompleter =
    buffer.state.get[CodeCompleter] match {
      case Some(comp) => comp
      case None       => tokenCompleter(wspace)
    }

  /** Returns a completer over all tokens in all buffers in `wspace`. */
  def tokenCompleter (wspace :Workspace) = wspace.state.get[TokenCompleter] match {
    case Some(comp) => comp
    case None =>
      val comp = new TokenCompleter(wspace)
      wspace.state.set[TokenCompleter](comp)
      comp
  }
}

class TokenCompleter (val wspace :Workspace) extends CodeCompleter {
  import CodeCompleter._
  final val MinRefreshInterval = 3000L

  class Tokener (val buffer :RBuffer) {
    val tokens = TreeMultimap.create[String,String]()
    var needsRefresh = true
    var lastRefresh = 0L

    buffer.edited.onEmit { needsRefresh = true }

    def addCompletions (prefix :String, sb :LinkedHashSet[String]) {
      if (needsRefresh) {
        val now = System.currentTimeMillis
        if (now - lastRefresh > MinRefreshInterval) {
          refresh()
          lastRefresh = now
          needsRefresh = false
        }
      }

      val iter = tokens.keySet.tailSet(prefix, true).iterator
      def loop () {
        val word = iter.next()
        if (word.startsWith(prefix)) {
          tokens.get(word) foreach sb.add
          if (iter.hasNext()) loop()
        }
      }
      if (iter.hasNext()) loop()
    }

    private def refresh () {
      tokens.clear()
      buffer.lines foreach { line =>
        val IsWord = Chars.isWord
        val sb = new java.lang.StringBuilder() // don't use Scala's retarded SB
        def flush () {
          if (sb.length > 0) {
            // ignore short tokens, not much point in completing a three letter word
            if (sb.length > 3) {
              val word = sb.toString
              val key = word.toLowerCase
              tokens.put(key, word)
            }
            sb.setLength(0)
          }
        }
        var ii = 0 ; val ll = line.length ; while (ii < ll) {
          val c = line.charAt(ii)
          val isToken = IsWord(c) || c == '_' // TODO: pluggable tokenizers?
          if (isToken) sb.append(c)
          else flush()
          ii += 1
        }
        flush()
      }
    }
  }

  private val tokeners = new HashMap[Buffer,Tokener]()
  private def tokener (buffer :Buffer) = tokeners.get(buffer) match {
    case null =>
      // note: fix naughtiness (have Workspace give out RBuffer?)
      val rbuffer = buffer.asInstanceOf[RBuffer]
      val ntok = new Tokener(rbuffer)
      tokeners.put(buffer, ntok)
      rbuffer.killed.onEmit { tokeners.remove(buffer) }
      ntok
    case tok => tok
  }

  override def completeAt (win :Window, buffer :Buffer, pos :Loc, point :Loc) = {
    val token = buffer.region(pos, point).map(_.asString).mkString
    val prefix = token.toLowerCase
    // we need at least a two char prefix to do our completion (TODO: maybe one?)
    val comps = if (prefix.length < 2) Seq() else {
      val matches = new LinkedHashSet[String]()
      // first add completions from the same buffer
      tokener(buffer).addCompletions(prefix, matches)
      // then add completions from all other (non-ephmeral) open buffers
      for (obuffer <- wspace.buffers) {
        if (obuffer != buffer && !Buffer.isScratch(obuffer.name)) {
          tokener(obuffer).addCompletions(prefix, matches)
        }
      }
      // if the token is mixed case, filter out all matches that don't match case exactly
      if (Chars.mixedCase(token)) matches.removeIf(word => !word.startsWith(token))
      // remove the exact token on which we matched
      matches.remove(token)
      // if we have >0 other matches, add our prefix back as the final match
      if (matches.size() > 0) matches.add(token)
      Seq.builder[String]().append(matches).build()
    }
    Future.success(Completion(pos, comps.map(t => new Choice(t)), 0))
  }
}
