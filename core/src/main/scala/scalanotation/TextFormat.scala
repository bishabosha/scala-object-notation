package scalanotation

final case class TextFormat(pretty: Boolean, indent: Int, spacing: Int = 1):
  require(indent >= 0, s"indent must be >= 0, got $indent")
  require(spacing >= 0, s"spacing must be >= 0, got $spacing")

object TextFormat:
  def compact(spacing: Int = 1): TextFormat =
    TextFormat(pretty = false, indent = 2, spacing = spacing)

  def pretty(indent: Int = 2, spacing: Int = 1): TextFormat =
    TextFormat(pretty = true, indent = indent, spacing = spacing)
