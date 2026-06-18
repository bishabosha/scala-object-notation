package scalanotation

import scalanotation.internal.Encode

object Writers:
  def writeExpr[T: Writer](value: T): Expr =
    val writer = summon[Writer[T]]
    Encode.writeExpr(writer.schema, value)

  def write[T: Writer](value: T, format: TextFormat = TextFormat.compact()): String =
    Writer.renderText(summon[Writer[T]], value, format)

  def writePretty[T: Writer](value: T, indent: Int = 2, spacing: Int = 1): String =
    Writer.renderText(summon[Writer[T]], value, TextFormat.pretty(indent, spacing))

  def writeDecl[T: Writer](
      rootName: String,
      value: T,
      packageName: String = "",
      format: TextFormat = TextFormat.compact()
  ): String =
    Writer.renderDecl(summon[Writer[T]], rootName, value, packageName, format)

  def writeDeclPretty[T: Writer](
      rootName: String,
      value: T,
      packageName: String = "",
      indent: Int = 2,
      spacing: Int = 1
  ): String =
    Writer.renderDecl(
      summon[Writer[T]],
      rootName,
      value,
      packageName,
      TextFormat.pretty(indent, spacing)
    )
