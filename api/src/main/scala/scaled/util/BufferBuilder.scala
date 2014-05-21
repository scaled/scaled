//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled.util

import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.major.TextConfig

/** A helper class for programmatically populating a buffer. This makes it easy to create a styled
  * buffer with headers, text wrapped to a particular width, text in multiple columns, etc. It's
  * used by `describe-mode` and is useful for similar "generate a buffer describing something"
  * tasks.
  */
class BufferBuilder (fillWidth :Int) {
  import TextConfig._

  private final val MinFillWidth = 40
  private val _lines = ArrayBuffer[LineV]()

  /** Returns the lines accumulated to this builder. */
  def lines :Seq[LineV] = _lines

  /** Replaces the contents of `view`s buffer with the contents of this builder. The view's point
    * is moved to the start of the buffer and the buffer is marked clean.
    * @return the buffer to which the builder was applied, for easy chaining to a call to
    * [[Editor.visitBuffer]].
    */
  def applyTo (view :BufferView) :Buffer = {
    val buf = view.buffer
    buf.replace(buf.start, buf.end, lines)
    buf.markClean()
    view.point() = Loc.Zero
    buf
  }

  /** Appends `line` to the buffer. */
  def add (line :LineV) :this.type = {
    _lines += line
    this
  }

  def add (lines :Seq[LineV]) :this.type = {
    _lines ++= lines
    this
  }

  /** Appends a single line of `text` to the buffer, styled by `styles`. */
  def add (text :String, styles :Styles = Styles.None) :this.type = add(styledLine(text, styles))

  /** Appends `text` to the buffer, filling it at this builder's fill width. */
  def addFilled (text :String, styles :Styles = Styles.None) :this.type =
    addPreFilled("", text, styles)

  /** Appends `text` to the buffer, prefixing every line with `prefix`, and filling it at this
    * builder's fill width (minus the width of the prefix). */
  def addPreFilled (prefix :String, text :String, styles :Styles = Styles.None) :this.type = {
    val filler = new Filler(math.max(fillWidth-prefix.length, MinFillWidth))
    filler.append(Filler.flatten(text))
    if (prefix.length > 0) filler.filled.foreach { f => f.insert(0, prefix) }
    add(filler.filled.map(f => styledLine(f, styles)))
  }

  /** Appends a blank line to this buffer. */
  def addBlank () :this.type  = add(Line.Empty)

  /** Adds a header to the accumulating buffer. If the preceding line is not blank, a blank line
    * will be inserted before the header. The header text will be styled with
    * [[TextConfig.headerStyle]] and followed by a line of `===`s. */
  def addHeader (text :String) :this.type  = {
    if (!_lines.isEmpty && _lines.last.length > 0) addBlank()
    add(text, Styles(TextConfig.headerStyle))
    add(toDashes(text, '='), Styles(TextConfig.headerStyle))
  }

  /** Adds a subheader to the accumulating buffer. If the preceding line is not blank, a blank line
    * will be inserted before the subheader. The text will be styled with
    * [[TextConfig.subHeaderStyle]] and followed by a line of `---`s. */
  def addSubHeader (text :String) :this.type  = {
    if (!_lines.isEmpty && _lines.last.length > 0) addBlank()
    add(text, Styles(TextConfig.subHeaderStyle))
    add(toDashes(text, '-'), Styles(TextConfig.subHeaderStyle))
  }

  /** Adds a section header to the accumulating buffer. If the preceding line is not blank, a blank
    * line will be inserted before the section header. The text will be styled with
    * [[TextConfig.sectionStyle]]. */
  def addSection (text :String) :this.type  = {
    if (!_lines.isEmpty && _lines.last.length > 0) addBlank()
    add(text, Styles(TextConfig.sectionStyle))
  }

  /** Adds `keyvalue` with `key` styled in [[TextConfig.prefixStyle]]. The caller is expected to
    * include the separator and whitespace in `key` (i.e. `foo: ` or `bar = `). `value` will be
    * wrapped to the builder's fill width minus the width of the key (wrapped lines will be prefixed
    * with `key.length` spaces). */
  def addKeyValue (key :String, value :String) :this.type = {
    def simple (value :CharSequence) = Line.builder("$key$value").withStyles(
      Styles(TextConfig.prefixStyle), 0, key.length).build()
    val valueFill = math.max(fillWidth-key.length, MinFillWidth)
    if (value.length <= valueFill) add(simple(value))
    else {
      val filler = new Filler(valueFill)
      filler.append(Filler.flatten(value))
      add(simple(filler.filled.head))
      val prefix = toDashes(key, ' ')
      filler.filled.drop(1).foreach(f => add(Line(prefix + f)))
      this
    }
  }

  /** Adds `keyvalue` for each key/value pair in `kvs`, where `key` is styled in
    * [[TextConfig.prefixStyle]] and all keys are padded to the width of the widest key. */
  def addKeysValues (kvs :Seq[(String,String)]) = {
    val padWidth = kvs.map(_._1).map(_.length).max
    def pad (key :String) = key + (" " * math.max(0, padWidth-key.length))
    kvs foreach { case (k, v) => addKeyValue(pad(k), v) }
  }

  private def styledLine (text :CharSequence, styles :Styles) = {
    if (styles eq Styles.None) Line(text)
    else Line.builder(text).withStyles(styles).build()
  }

  private def toDashes (text :String, dash :Char) = {
    val sb = new StringBuilder()
    val ll = text.length ; var ii = 0 ; while (ii < ll) {
      sb.append(if (Character.isWhitespace(text.charAt(ii))) ' ' else dash)
      ii += 1
    }
    sb.toString
  }
}
