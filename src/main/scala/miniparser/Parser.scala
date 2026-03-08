package miniparser

import scala.NamedTuple

import scala.collection.mutable

final class Parser(tokens: List[Token]):
  private var index = 0

  def parseSourceFile(): SourceFile =
    expectVal()
    val name = expectIdentifier()
    expectEquals()
    val value = parseExpr()
    expectEof()
    SourceFile(ValDecl(name, value))

  private def parseExpr(): Expr = parseConcat()

  private def parseConcat(): Expr =
    var left = parsePrimary()
    if currentIsPlus then
      def strExpected(curr: Token) = fail(s"expected a string, found ${describe(curr)}", curr.span)
      left match
        case Expr.StringConstant(value) =>
          val buf = StringBuilder() ++= value
          while currentIsPlus do
            advance()
            parsePrimary() match
              case Expr.StringConstant(value) =>
                buf ++= value
              case _ =>
                strExpected(currentToken())
          end while
          Expr.StringConstant(buf.toString())
        case _ =>
          strExpected(currentToken())
    else
      left

  private def parsePrimary(): Expr =
    currentToken() match
      case Token.VectorId(_) => parseVector()
      case Token.LParen(_) => parseNamedTuple()
      case Token.StringLit(raw, value, _) => advance(); Expr.StringConstant(value)
      case Token.CharLit(raw, value, _) => advance(); Expr.CharConstant(value)
      case Token.IntLit(raw, value, _) => advance(); Expr.IntConstant(value)
      case Token.LongLit(raw, value, _) => advance(); Expr.LongConstant(value)
      case Token.FloatLit(raw, value, _) => advance(); Expr.FloatConstant(value)
      case Token.DoubleLit(raw, value, _) => advance(); Expr.DoubleConstant(value)
      case Token.TrueKw(_) => advance(); Expr.BooleanConstant(true)
      case Token.FalseKw(_) => advance(); Expr.BooleanConstant(false)
      case Token.NullKw(_) => advance(); Expr.NullConstant
      case Token.Minus(_) => parseSignedNumber()
      case other => fail(s"Expected an expression but found ${describe(other)}", other.span)

  private def parseSignedNumber(): Expr =
    val minus = currentToken().span
    advance()
    currentToken() match
      case Token.IntLit(raw, value, _) => advance(); Expr.IntConstant(-value)
      case Token.LongLit(raw, value, _) => advance(); Expr.LongConstant(-value)
      case Token.FloatLit(raw, value, _) => advance(); Expr.FloatConstant(-value)
      case Token.DoubleLit(raw, value, _) => advance(); Expr.DoubleConstant(-value)
      case other => fail("A minus sign must be followed by a numeric literal", minus)

  private def parseNamedTuple(): Expr =
    currentToken() match
      case Token.LParen(_) => advance()
      case other => fail(s"Expected '(' but found ${describe(other)}", other.span)
    val peek = currentToken()
    if peek.isInstanceOf[Token.RParen] then
      fail("Unit value '()' is not valid.", peek.span)
    else
      var seen = collection.mutable.Set.empty[String]
      val names = IArray.newBuilder[String]
      val elements = IArray.newBuilder[Expr]
      val buf = TupBuf(null, null)
      if parseTupleElement(buf) then
        names += buf.name.nn
        elements += buf.elem.nn
      var sawComma = false
      while currentToken().isInstanceOf[Token.Comma] do
        sawComma = true
        advance()
        if !currentToken().isInstanceOf[Token.RParen] && parseTupleElement(buf) then
          names += buf.name.nn
          elements += buf.elem.nn
      currentToken() match
        case Token.RParen(_) => advance()
        case other => fail(s"Expected ')' but found ${describe(other)}", other.span)
      Expr.NamedTupleExpr(names.result(), elements.result())

  val emptyExprs = IArray.empty[Expr]
  private def parseVector(): Expr =
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) => fail(s"Expected 'Vector(' but found ${describe(other)}", other.span)
    if currentToken().isInstanceOf[Token.RParen] then
      advance()
      Expr.VectorExpr(emptyExprs)
    else
      val elements = IArray.newBuilder[Expr]
      elements += parseExpr()
      var sawComma = false
      while currentToken().isInstanceOf[Token.Comma] do
        sawComma = true
        advance()
        if !currentToken().isInstanceOf[Token.RParen] then elements += parseExpr()
      currentToken() match
        case Token.RParen(_) => advance()
        case other => fail(s"Expected ')' but found ${describe(other)}", other.span)
      Expr.VectorExpr(elements.result())

  class TupBuf(var name: String | Null, var elem: Expr | Null)

  private def parseTupleElement(buf: TupBuf): Boolean =
    (currentToken(), peekToken()) match
      case (Token.Identifier(name, _), Token.Equals(_)) =>
        advance()
        advance()
        buf.name = name
        buf.elem = parseExpr()
        true
      case (other, _) =>
        fail(s"expected field name 'x = ' but found ${describe(other)}", other.span)

  private def expectVal(): Unit =
    currentToken() match
      case Token.ValKw(_) => advance()
      case other => fail(s"Expected 'val' but found ${describe(other)}", other.span)

  private def expectIdentifier(): String =
    currentToken() match
      case Token.Identifier(name, _) => advance(); name
      case other => fail(s"Expected an identifier but found ${describe(other)}", other.span)

  private def expectEquals(): Unit =
    currentToken() match
      case Token.Equals(_) => advance()
      case other => fail(s"Expected '=' but found ${describe(other)}", other.span)

  private def expectEof(): Unit =
    currentToken() match
      case Token.Eof(_) => ()
      case other => fail(s"Expected end of input but found ${describe(other)}", other.span)

  private def currentIsPlus: Boolean = currentToken().isInstanceOf[Token.Plus]

  private def currentToken(): Token = tokens(index)

  private def peekToken(): Token =
    if index + 1 < tokens.length then tokens(index + 1) else tokens.last

  private def advance(): Token =
    val token = tokens(index)
    if index < tokens.length - 1 then index += 1
    token

  private def describe(token: Token): String = token match
    case Token.ValKw(_) => "'val'"
    case Token.VectorId(_) => "'Vector'"
    case Token.TrueKw(_) => "'true'"
    case Token.FalseKw(_) => "'false'"
    case Token.NullKw(_) => "'null'"
    case Token.Identifier(name, _) => s"identifier '$name'"
    case Token.IntLit(raw, _, _) => s"integer literal '$raw'"
    case Token.LongLit(raw, _, _) => s"long literal '$raw'"
    case Token.FloatLit(raw, _, _) => s"float literal '$raw'"
    case Token.DoubleLit(raw, _, _) => s"double literal '$raw'"
    case Token.StringLit(raw, _, _) => s"string literal $raw"
    case Token.CharLit(raw, _, _) => s"character literal $raw"
    case Token.Equals(_) => "'='"
    case Token.Plus(_) => "'+'"
    case Token.Minus(_) => "'-'"
    case Token.Comma(_) => "','"
    case Token.LParen(_) => "'('"
    case Token.RParen(_) => "')'"
    case Token.Eof(_) => "end of input"

  private def fail(message: String, span: Span): Nothing =
    throw ParseException(s"$message at ${span.line}:${span.column}")

object Parser:
  def parse(input: String): SourceFile =
    val tokens = Tokenizer.tokenize(input)
    new Parser(tokens).parseSourceFile()

  def parseValueAs[T](input: String)(using decoder: AstDecoder[T]): Either[DecodeError, T] =
    parse(input).decodeValueAs[T]

  def parseNamedTupleAs[T <: NamedTuple.AnyNamedTuple](input: String)(using decoder: AstDecoder[T]): Either[DecodeError, T] =
    parse(input).decodeNamedTupleAs[T]
