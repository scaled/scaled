//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scala.annotation.varargs;
import scaled._
import scaled.major.TextConfig

/** A trait used to dynamically include elements in `describe-foo` buffers. */
trait Describable {
  /** Appends a description of `this` to `bb`. */
  def describeSelf (bb :BufferBuilder) :Unit
}

/** A helper class for programmatically populating a buffer. This makes it easy to create a styled
  * buffer with headers, text wrapped to a particular width, text in multiple columns, etc. It's
  * used by `describe-mode` and is useful for similar "generate a buffer describing something"
  * tasks.
  */
class BufferBuilder (val fillWidth :Int) {
  import TextConfig._

  private final val MinFillWidth = 40
  private val _lines = SeqBuffer[LineV]()

  /** Returns the lines accumulated to this builder. */
  def lines :SeqV[LineV] = _lines

  /** Replaces the contents of `buffer` buffer with the contents of this builder.
    * @return `buffer`, for easy chaining to a call to `visitBuffer`.
    */
  def applyTo (buffer :Buffer) :Buffer = {
    buffer.replace(buffer.start, buffer.end, lines)
    if (buffer.end.col > 0) buffer.split(buffer.end) // tack on a trailing NL if needed
    buffer.markClean()
    buffer
  }

  /** Appends `line` to the buffer. */
  def add (line :LineV) :this.type = {
    _lines += line
    this
  }

  /** Appends `lines` to the buffer. */
  def add (lines :Seq[LineV]) :this.type = {
    _lines ++= lines
    this
  }

  /** Appends a single line of `text` to the buffer, styled by `styles`. */
  @varargs def add (text :String, styles :String*) :this.type = add(styledLine(text, styles))

  /** Appends `text` to the buffer, filling it at this builder's fill width. */
  @varargs def addFilled (text :String, styles :String*) :this.type =
    addPreFilled("", text, styles :_*)

  /** Appends `text` to the buffer, prefixing every line with `prefix`, and filling it at this
    * builder's fill width (minus the width of the prefix). */
  @varargs def addPreFilled (prefix :String, text :String, styles :String*) :this.type = {
    val filler = new Filler(math.max(fillWidth-prefix.length, MinFillWidth))
    filler.append(Filler.flatten(text))
    if (prefix.length > 0) filler.filled.foreach { f => f.insert(0, prefix) }
    add(filler.filled.map(f => styledLine(f, styles)))
  }

  /** Appends a blank line to this buffer. */
  def addBlank () :this.type  = add(Line.Empty)

  /** Ensures that the buffer is either empty or contains a trailing blank line. */
  def ensureBlank () :this.type = {
    if (!_lines.isEmpty && _lines.last.length > 0) addBlank()
    this
  }

  /** Adds a header to the accumulating buffer. If the preceding line is not blank, a blank line
    * will be inserted before the header. The header text will be styled with
    * [[TextConfig.headerStyle]] and followed by a line of `===`s. */
  def addHeader (text :String) :this.type = addHeader(Line.builder(text))
  /** Adds a header to the accumulating buffer. See [[addHeader(String)]]. */
  def addHeader (lb :Line.Builder) :this.type = {
    ensureBlank()
    val line = lb.withStyle(TextConfig.headerStyle).build()
    add(line).add(toDashes(line, '='), TextConfig.headerStyle)
  }

  /** Adds a subheader to the accumulating buffer. If the preceding line is not blank, a blank line
    * will be inserted before the subheader. The text will be styled with
    * [[TextConfig.subHeaderStyle]] and followed by a line of `---`s. */
  def addSubHeader (text :String) :this.type  = addSubHeader(Line.builder(text))
  /** Adds a subheader to the accumulating buffer. See [[addSubHeader(String)]]. */
  def addSubHeader (lb :Line.Builder) :this.type  = {
    ensureBlank()
    val line = lb.withStyle(TextConfig.subHeaderStyle).build()
    add(line)
    add(toDashes(line, '-'), TextConfig.subHeaderStyle)
  }

  /** Adds a section header to the accumulating buffer. If the preceding line is not blank, a blank
    * line will be inserted before the section header. The text will be styled with
    * [[TextConfig.sectionStyle]]. */
  def addSection (text :String) :this.type  = addSection(Line.builder(text))
  /** Adds a section header to the accumulating buffer. See [[addSection(String)]]. */
  def addSection (lb :Line.Builder) :this.type  = {
    ensureBlank()
    add(lb.withStyle(TextConfig.sectionStyle).build())
  }

  /** Adds `keyvalue` with `key` styled in [[TextConfig.prefixStyle]]. The caller is expected to
    * include the separator and whitespace in `key` (i.e. `foo: ` or `bar = `). `value` will be
    * wrapped to the builder's fill width minus the width of the key (wrapped lines will be prefixed
    * with `key.length` spaces). */
  def addKeyValue (key :String, value :Any) :this.type = {
    def simple (value :CharSequence) = Line.builder(s"$key$value").withStyle(
      TextConfig.prefixStyle, 0, key.length).build()
    val valueFill = math.max(fillWidth-key.length, MinFillWidth)
    val valueStr = String.valueOf(value)
    if (valueStr.length <= valueFill) add(simple(valueStr))
    else {
      val filler = new Filler(valueFill)
      filler.append(Filler.flatten(valueStr))
      add(simple(filler.filled.head))
      val prefix = toDashes(key, ' ')
      filler.filled.drop(1).foreach(f => add(Line(prefix + f)))
      this
    }
  }

  /** Adds `keyvalue` for each key/value pair in `kvs`, where `key` is styled in
    * [[TextConfig.prefixStyle]] and all keys are padded to the width of the widest key. */
  def addKeysValues (kvs :(String,Any)*) :this.type = addKeysValues(Iterable.view(kvs))

  /** Adds `keyvalue` for each key/value pair in `kvs`, where `key` is styled in
    * [[TextConfig.prefixStyle]] and all keys are padded to the width of the widest key. */
  def addKeysValues (kvs :Iterable[(String,Any)]) :this.type = {
    val padWidth = kvs.fold(0)((m, kv) => math.max(m, kv._1.length))
    def pad (key :String) = key + (" " * (padWidth-key.length))
    kvs foreach { case (k, v) => addKeyValue(pad(k), v) }
    this
  }

  private def styledLine (text :CharSequence, styles :scala.Seq[String]) =
    styles.foldLeft(Line.builder(text))(_.withStyle(_)).build()

  private def toDashes (text :CharSequence, dash :Char) = {
    val sb = new StringBuilder()
    val ll = text.length ; var ii = 0 ; while (ii < ll) {
      sb.append(if (Character.isWhitespace(text.charAt(ii))) ' ' else dash)
      ii += 1
    }
    sb.toString
  }
}
