package scalanotation.internal

import scalanotation.Expr
import scalanotation.TextFormat

private[scalanotation] object ExprRenderer:
  final class Output:
    private val builder = new StringBuilder

    def append(ch: Char): Unit =
      builder += ch

    def append(str: String): Unit =
      builder ++= str

    def newlineAndIndent(depth: Int)(using format: TextFormat): Unit =
      if format.pretty then
        builder += '\n'
        var i = 0
        val n = depth * format.indent
        while i < n do
          builder += ' '
          i += 1

    def result(): String =
      builder.result()

  def renderDecl(name: String, expr: Expr): String =
    renderDecl(name, expr, TextFormat.compact)

  def renderDecl(name: String, expr: Expr, format: TextFormat): String =
    val out = Output()
    out.append("val ")
    out.append(name)
    out.append(" = ")
    renderExpr(expr, out, 0)(using format)
    out.result()

  def renderExpr(expr: Expr): String =
    renderExpr(expr, TextFormat.compact)

  def renderExpr(expr: Expr, format: TextFormat): String =
    val out = Output()
    renderExpr(expr, out, 0)(using format)
    out.result()

  private[scalanotation] def renderExpr(
      expr: Expr,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    expr match
      case Expr.NamedTupleExpr(elements) =>
        renderNamedTuple(out, depth, elements.length) { index =>
          val (name, value) = elements(index)
          out.append(name)
          out.append(" = ")
          renderExpr(value, out, depth + 1)
        }
      case Expr.VectorExpr(elements) =>
        renderVector(out, depth, elements.length) { index =>
          renderExpr(elements(index), out, depth + 1)
        }
      case Expr.StringConstant(value) =>
        renderStringLiteral(value, out)
      case Expr.CharConstant(value) =>
        renderCharLiteral(value, out)
      case Expr.IntConstant(value) =>
        out.append(value.toString)
      case Expr.LongConstant(value) =>
        out.append(s"${value}L")
      case Expr.FloatConstant(value) =>
        renderFloatLiteral(value, out)
      case Expr.DoubleConstant(value) =>
        renderDoubleLiteral(value, out)
      case Expr.BooleanConstant(value) =>
        out.append(value.toString)
      case Expr.NullConstant =>
        out.append("null")

  private[scalanotation] def renderNamedTuple(
      out: Output,
      depth: Int,
      size: Int
  )(
      renderField: Int => Unit
  )(using format: TextFormat): Unit =
    renderComposite(out, depth, size, open = "(", close = ")")(renderField)

  private[scalanotation] def renderVector(
      out: Output,
      depth: Int,
      size: Int
  )(
      renderValue: Int => Unit
  )(using format: TextFormat): Unit =
    renderComposite(out, depth, size, open = "Vector(", close = ")")(renderValue)

  private def renderComposite(
      out: Output,
      depth: Int,
      size: Int,
      open: String,
      close: String
  )(
      renderValue: Int => Unit
  )(using format: TextFormat): Unit =
    out.append(open)
    if size == 0 then out.append(close)
    else if !format.pretty then
      var i = 0
      while i < size do
        if i > 0 then out.append(", ")
        renderValue(i)
        i += 1
      out.append(close)
    else
      var i = 0
      while i < size do
        out.newlineAndIndent(depth + 1)
        renderValue(i)
        if i + 1 < size then out.append(',')
        i += 1
      out.newlineAndIndent(depth)
      out.append(close)

  private[scalanotation] def renderStringLiteral(
      value: String,
      out: Output
  ): Unit =
    out.append('"')
    value.iterator.foreach(ch => out.append(escapeChar(ch, inString = true)))
    out.append('"')

  private[scalanotation] def renderCharLiteral(
      value: Char,
      out: Output
  ): Unit =
    out.append('\'')
    out.append(escapeChar(value, inString = false))
    out.append('\'')

  private[scalanotation] def renderFloatLiteral(value: Float, out: Output): Unit =
    if !java.lang.Float.isFinite(value) then
      throw IllegalArgumentException(s"Cannot render non-finite Float value $value")
    out.append(s"${java.lang.Float.toString(value)}f")

  private[scalanotation] def renderDoubleLiteral(value: Double, out: Output): Unit =
    if !java.lang.Double.isFinite(value) then
      throw IllegalArgumentException(s"Cannot render non-finite Double value $value")
    out.append(java.lang.Double.toString(value))

  private def escapeChar(ch: Char, inString: Boolean): String =
    ch match
      case '\n'              => "\\n"
      case '\r'              => "\\r"
      case '\t'              => "\\t"
      case '\b'              => "\\b"
      case '\f'              => "\\f"
      case '\\'              => "\\\\"
      case '"' if inString   => "\\\""
      case '\'' if !inString => "\\'"
      case other             => other.toString
