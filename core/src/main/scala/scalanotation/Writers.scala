package scalanotation

import scalanotation.internal.Encode

object Writers:
  def writeExpr[T: Writer](value: T): Expr =
    val writer = summon[Writer[T]]
    Encode.writeExpr(writer.schema, value)

  def write[T: Writer](value: T): String =
    Writer.renderText(summon[Writer[T]], value, TextFormat.compact)

  def write[T: Writer](value: T, format: TextFormat): String =
    Writer.renderText(summon[Writer[T]], value, format)

  def writePretty[T: Writer](value: T, indent: Int = 2): String =
    Writer.renderText(summon[Writer[T]], value, TextFormat.pretty(indent))

  def writeDecl[T: Writer](rootName: String, value: T): String =
    Writer.renderDecl(summon[Writer[T]], rootName, value, TextFormat.compact)

  def writeDecl[T: Writer](rootName: String, value: T, format: TextFormat): String =
    Writer.renderDecl(summon[Writer[T]], rootName, value, format)

  def writeDecl[T: Writer](rootName: String, value: T, packageName: String): String =
    Writer.renderDecl(summon[Writer[T]], rootName, value, packageName, TextFormat.compact)

  def writeDecl[T: Writer](
      rootName: String,
      value: T,
      packageName: String,
      format: TextFormat
  ): String =
    Writer.renderDecl(summon[Writer[T]], rootName, value, packageName, format)

  def writeDeclPretty[T: Writer](rootName: String, value: T, indent: Int = 2): String =
    Writer.renderDecl(summon[Writer[T]], rootName, value, TextFormat.pretty(indent))

  def writeDeclPretty[T: Writer](
      rootName: String,
      value: T,
      packageName: String,
      indent: Int
  ): String =
    Writer.renderDecl(summon[Writer[T]], rootName, value, packageName, TextFormat.pretty(indent))
