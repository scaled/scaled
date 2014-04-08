//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.major

import scaled._

object CodeConfig extends ConfigDefs {

  /** The CSS style applied to `builtin` syntax. */
  val builtinStyle = "codeBuiltinFace"
  /** The CSS style applied to `preprocessor` syntax. */
  val preprocessorStyle = "codePreprocessorFace"
  /** The CSS style applied to `comment` syntax. */
  val commentStyle = "codeCommentFace"
  /** The CSS style applied to `constant` syntax. */
  val constantStyle = "codeConstantFace"
  /** The CSS style applied to `doc` syntax. */
  val docStyle = "codeDocFace"
  /** The CSS style applied to `keyword` syntax. */
  val keywordStyle = "codeKeywordFace"
  /** The CSS style applied to `reference` syntax. */
  val referenceStyle = "codeReferenceFace"
  /** The CSS style applied to `string` syntax. */
  val stringStyle = "codeStringFace"
  /** The CSS style applied to `type` syntax. */
  val typeStyle = "codeTypeFace"
  /** The CSS style applied to `variable` syntax. */
  val variableStyle = "codeVariableFace"
  /** The CSS style applied to `function` syntax. */
  val functionStyle = "codeFunctionFace"
}

/** A base class for major modes which edit program code.
  */
abstract class CodeMode (editor :Editor, config :Config, view :RBufferView, disp :Dispatcher)
    extends EditingMode(editor, config, view, disp) {

  override def configDefs = CodeConfig :: super.configDefs
  override def stylesheets = stylesheetURL("/code.css") :: super.stylesheets
}
