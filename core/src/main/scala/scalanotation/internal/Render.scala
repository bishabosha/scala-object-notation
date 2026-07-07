package scalanotation.internal

import scalanotation.Expr
import scalanotation.TextFormat

private[scalanotation] object ExprRenderer:
  final class Output:
    // the underlying java builder's typed append overloads format primitives in place, so no
    // intermediate String is allocated for numeric and boolean literals
    private val builder = new java.lang.StringBuilder

    def append(ch: Char): Unit =
      builder.append(ch)

    def append(str: String): Unit =
      builder.append(str)

    def append(value: Int): Unit =
      builder.append(value)

    def append(value: Long): Unit =
      builder.append(value)

    def append(value: Float): Unit =
      builder.append(value)

    def append(value: Double): Unit =
      builder.append(value)

    def append(value: Boolean): Unit =
      builder.append(value)

    /** appends `str.substring(from, until)` without allocating the substring */
    def appendSlice(str: String, from: Int, until: Int): Unit =
      builder.append(str, from, until)

    def tokenSpacing()(using format: TextFormat): Unit =
      var i = 0
      while i < format.spacing do
        builder.append(' ')
        i += 1

    def appendToken(ch: Char)(using format: TextFormat): Unit =
      tokenSpacing()
      builder.append(ch)
      tokenSpacing()

    def newlineAndIndent(depth: Int)(using format: TextFormat): Unit =
      if format.pretty then
        builder.append('\n')
        var i = 0
        val n = depth * format.indent
        while i < n do
          builder.append(' ')
          i += 1

    def result(): String =
      builder.toString

  def renderDecl(name: String, expr: Expr): String =
    renderDecl(name, expr, TextFormat.compact())

  def renderDecl(name: String, expr: Expr, format: TextFormat): String =
    val out = Output()
    out.append("val ")
    IdentifierSyntax.appendIdentifier(name, out)
    out.append(" = ")
    renderExpr(expr, out, 0)(using format)
    out.result()

  def renderExpr(expr: Expr): String =
    renderExpr(expr, TextFormat.compact())

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
      case Expr.NamedTupleExpr(elements) if elements.isEmpty =>
        out.append("NamedTuple.Empty")
      case Expr.NamedTupleExpr(elements) =>
        renderNamedTuple(out, depth, elements.length) { index =>
          val (name, value) = elements(index)
          IdentifierSyntax.appendIdentifier(name, out)
          out.appendToken('=')
          renderExpr(value, out, depth + 1)
        }
      case Expr.TupleExpr(elements) =>
        renderTuple(out, depth, elements.length) { index =>
          renderTupleElement(elements(index), out, depth + 1)
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
        out.append(value)
      case Expr.LongConstant(value) =>
        out.append(value)
        out.append('L')
      case Expr.FloatConstant(value) =>
        renderFloatLiteral(value, out)
      case Expr.DoubleConstant(value) =>
        renderDoubleLiteral(value, out)
      case Expr.BooleanConstant(value) =>
        out.append(value)
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

  private[scalanotation] def renderTuple(
      out: Output,
      depth: Int,
      size: Int
  )(
      renderValue: Int => Unit
  )(using format: TextFormat): Unit =
    if size == 0 then out.append("EmptyTuple")
    else if size == 1 then
      renderComposite(out, depth, size, open = "Tuple(", close = ")")(renderValue)
    else renderComposite(out, depth, size, open = "(", close = ")")(renderValue)

  private def renderTupleElement(
      expr: Expr,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    renderExpr(expr, out, depth)

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
        if i > 0 then
          out.append(',')
          out.tokenSpacing()
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
    // escape-free runs (the common case: the whole string) are bulk-appended straight from the
    // input; nothing is allocated per character
    val length = value.length
    var start  = 0
    var i      = 0
    while i < length do
      val escaped = escapeCode(value.charAt(i), inString = true)
      if escaped != '\u0000' then
        if i > start then out.appendSlice(value, start, i)
        out.append('\\')
        out.append(escaped)
        start = i + 1
      i += 1
    if start == 0 then out.append(value)
    else if length > start then out.appendSlice(value, start, length)
    out.append('"')

  private[scalanotation] def renderCharLiteral(
      value: Char,
      out: Output
  ): Unit =
    out.append('\'')
    val escaped = escapeCode(value, inString = false)
    if escaped != '\u0000' then
      out.append('\\')
      out.append(escaped)
    else out.append(value)
    out.append('\'')

  private[scalanotation] def renderFloatLiteral(value: Float, out: Output): Unit =
    if !java.lang.Float.isFinite(value) then
      throw IllegalArgumentException(s"Cannot render non-finite Float value $value")
    out.append(value)
    out.append('f')

  private[scalanotation] def renderDoubleLiteral(value: Double, out: Output): Unit =
    if !java.lang.Double.isFinite(value) then
      throw IllegalArgumentException(s"Cannot render non-finite Double value $value")
    out.append(value)

  /** The char following the backslash for chars that must be escaped, or '\u0000' for "render
    * as-is". Returning the escape code instead of a String keeps the escape decision
    * allocation-free.
    */
  private def escapeCode(ch: Char, inString: Boolean): Char =
    ch match
      case '\n'              => 'n'
      case '\r'              => 'r'
      case '\t'              => 't'
      case '\b'              => 'b'
      case '\f'              => 'f'
      case '\\'              => '\\'
      case '"' if inString   => '"'
      case '\'' if !inString => '\''
      case _                 => '\u0000'
