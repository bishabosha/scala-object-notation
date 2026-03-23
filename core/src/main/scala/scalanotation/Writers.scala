package scalanotation

object Writers:
  def writeExpr[T: Writer](value: T): Expr =
    summon[Writer[T]].write(value)

  def write[T: Writer](value: T): String =
    summon[Writer[T]].writeText(value)

  def write[T: Writer](value: T, format: TextFormat): String =
    summon[Writer[T]].writeText(value, format)

  def writePretty[T: Writer](value: T, indent: Int = 2): String =
    summon[Writer[T]].writePrettyText(value, indent)

  def writeDecl[T: Writer](rootName: String, value: T): String =
    summon[Writer[T]].writeDecl(rootName, value)

  def writeDecl[T: Writer](rootName: String, value: T, format: TextFormat): String =
    summon[Writer[T]].writeDecl(rootName, value, format)

  def writeDeclPretty[T: Writer](rootName: String, value: T, indent: Int = 2): String =
    summon[Writer[T]].writeDeclPretty(rootName, value, indent)
