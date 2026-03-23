package scalanotation

final case class TextFormat(pretty: Boolean, indent: Int):
  require(indent >= 0, s"indent must be >= 0, got $indent")

object TextFormat:
  val compact: TextFormat = TextFormat(pretty = false, indent = 2)

  def pretty(indent: Int = 2): TextFormat =
    TextFormat(pretty = true, indent = indent)
