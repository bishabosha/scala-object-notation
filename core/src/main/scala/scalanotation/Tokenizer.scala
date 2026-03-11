package scalanotation

import scala.collection.mutable
import Tokenizer.TokenError
import steps.result.Result

private final class Tokenizer(input: String):
  private var index = 0
  private var line = 1
  private var column = 1

  private val namesCache = mutable.HashMap.empty[String, String]
  def nameCached(str: String): String = namesCache.getOrElseUpdate(str, str)

  val Seq(KW_val @ _, KW_true @ _, KW_false @ _, KW_null @ _, KW_Vector @ _) =
    Seq("val","true","false","null","Vector").map(nameCached(_)).runtimeChecked

  private class ParseException(val message: String, val span: Span)
    extends Exception with scala.util.control.NoStackTrace

  def tokenize(): Result[IArray[Token], TokenError] =
    Result.catchException({ case e: ParseException => TokenError(e.message, e.span) }):
      val tokens = IArray.newBuilder[Token]
      while !isAtEnd do
        skipTrivia()
        if !isAtEnd then tokens += nextToken()
      tokens += Token.Eof(currentSpan())
      tokens.result()

  private def nextToken(): Token =
    val start = currentSpan()
    currentChar() match
      case '(' => advance(); Token.LParen(start)
      case ')' => advance(); Token.RParen(start)
      case '=' => advance(); Token.Equals(start)
      case '+' => advance(); Token.Plus(start)
      case '-' => advance(); Token.Minus(start)
      case ',' => advance(); Token.Comma(start)
      case '"' => scanString(start)
      case '\'' => scanChar(start)
      case ch if isIdentifierStart(ch) => scanIdentifier(start)
      case ch if ch.isDigit => scanNumber(start)
      case ch => fail(s"Unexpected character '$ch'", start)

  private def scanIdentifier(start: Span): Token =
    val builder = new StringBuilder
    while !isAtEnd && isIdentifierPart(currentChar()) do builder += advance()
    nameCached(builder.result()) match
      case KW_val => Token.ValKw(start)
      case KW_true => Token.TrueKw(start)
      case KW_false => Token.FalseKw(start)
      case KW_null => Token.NullKw(start)
      case KW_Vector => Token.VectorId(start)
      case name => Token.Identifier(name, start)

  private def scanNumber(start: Span): Token =
    if currentChar() == '0' && peekChar().exists(ch => ch == 'x' || ch == 'X' || ch == 'b' || ch == 'B') then
      scanPrefixedInteger(start)
    else
      scanDecimalNumber(start)

  private def scanPrefixedInteger(start: Span): Token =
    val prefix = new StringBuilder
    prefix += advance()
    val marker = advance()
    prefix += marker

    val base =
      if marker == 'x' || marker == 'X' then 16
      else 2

    val digits = new StringBuilder
    var sawDigit = false
    while !isAtEnd && isDigitForBase(currentChar(), base) do
      digits += advance()
      sawDigit = true

    while !isAtEnd && currentChar() == '_' do
      digits += advance()
      if isAtEnd || !isDigitForBase(currentChar(), base) then
        fail(s"Expected a base-$base digit after numeric separator", start)
      while !isAtEnd && isDigitForBase(currentChar(), base) do
        digits += advance()
        sawDigit = true

    if !sawDigit then fail(s"Expected at least one base-$base digit after numeric prefix", start)

    if !isAtEnd && currentChar().isLetterOrDigit && currentChar() != 'l' && currentChar() != 'L' then
      fail(s"Invalid digit '${currentChar()}' for base-$base literal", start)

    val suffix =
      if !isAtEnd && (currentChar() == 'l' || currentChar() == 'L') then Some(advance())
      else None

    val rawDigits = digits.result()
    val raw = prefix.result() + rawDigits + suffix.fold("")(_.toString)
    val normalizedDigits = rawDigits.replace("_", "")

    suffix match
      case Some(_) => Token.LongLit(raw, parseLongLiteral(normalizedDigits, raw, start, base), start)
      case None => Token.IntLit(raw, parseIntLiteral(normalizedDigits, raw, start, base), start)

  private def scanDecimalNumber(start: Span): Token =
    val builder = new StringBuilder
    var hasDot = false
    var hasExponent = false

    def takeDigits(): Unit =
      while !isAtEnd && (currentChar().isDigit || currentChar() == '_') do builder += advance()

    takeDigits()
    if !isAtEnd && currentChar() == '.' && peekChar().exists(_.isDigit) then
      hasDot = true
      builder += advance()
      takeDigits()

    if !isAtEnd && (currentChar() == 'e' || currentChar() == 'E') then
      hasExponent = true
      builder += advance()
      if !isAtEnd && (currentChar() == '+' || currentChar() == '-') then builder += advance()
      if isAtEnd || !currentChar().isDigit then fail("Exponent requires at least one digit", start)
      takeDigits()

    val suffix =
      if !isAtEnd && "lLfFdD".contains(currentChar()) then Some(advance())
      else None

    val raw = builder.result() + suffix.fold("")(_.toString)
    val normalizedDigits = builder.result().replace("_", "")

    suffix match
      case Some('l' | 'L') =>
        if hasDot || hasExponent then fail("Long literals cannot contain a decimal point or exponent", start)
        Token.LongLit(raw, parseLongLiteral(normalizedDigits, raw, start), start)
      case Some('f' | 'F') =>
        Token.FloatLit(raw, parseFloatLiteral(normalizedDigits, raw, start), start)
      case Some('d' | 'D') =>
        Token.DoubleLit(raw, parseDoubleLiteral(normalizedDigits, raw, start), start)
      case Some(_) =>
        fail(s"unrecognised numeric literal suffix '${suffix.get}'", start)
      case None if hasDot || hasExponent =>
        Token.DoubleLit(raw, parseDoubleLiteral(normalizedDigits, raw, start), start)
      case None =>
        Token.IntLit(raw, parseIntLiteral(normalizedDigits, raw, start), start)


  private def scanString(start: Span): Token =
    val raw = new StringBuilder
    val value = new StringBuilder
    raw += advance()
    while !isAtEnd && currentChar() != '"' do
      if currentChar() == '\\' then
        raw += advance()
        if isAtEnd then fail("Unterminated string literal", start)
        val escaped = advance()
        raw += escaped
        value += decodeEscape(escaped, start)
      else
        val ch = advance()
        raw += ch
        value += ch
    if isAtEnd then fail("Unterminated string literal", start)
    raw += advance()
    Token.StringLit(raw.result(), value.result(), start)

  private def scanChar(start: Span): Token =
    val raw = new StringBuilder
    raw += advance()
    if isAtEnd then fail("Unterminated character literal", start)
    val value =
      if currentChar() == '\\' then
        raw += advance()
        if isAtEnd then fail("Unterminated character literal", start)
        val escaped = advance()
        raw += escaped
        decodeEscape(escaped, start)
      else
        val ch = advance()
        raw += ch
        ch
    if isAtEnd || currentChar() != '\'' then fail("Character literal must contain exactly one character", start)
    raw += advance()
    Token.CharLit(raw.result(), value, start)

  private def decodeEscape(ch: Char, start: Span): Char =
    ch match
      case 'n' => '\n'
      case 'r' => '\r'
      case 't' => '\t'
      case 'b' => '\b'
      case 'f' => '\f'
      case '\\' => '\\'
      case '\'' => '\''
      case '"' => '"'
      case other => fail(s"Unsupported escape sequence \\$other", start)

  private def skipTrivia(): Unit =
    var keepGoing = true
    while keepGoing && !isAtEnd do
      skipWhitespace()
      if isAtEnd then
        keepGoing = false
      else if currentChar() == '/' && peekChar().contains('/') then
        skipLineComment()
      else if currentChar() == '/' && peekChar().contains('*') then
        skipBlockComment()
      else
        keepGoing = false

  private def skipWhitespace(): Unit =
    while !isAtEnd && currentChar().isWhitespace do advance()

  private def skipLineComment(): Unit =
    advance()
    advance()
    while !isAtEnd && currentChar() != '\n' do advance()

  private def skipBlockComment(): Unit =
    val start = currentSpan()
    advance()
    advance()
    var depth = 1
    while depth > 0 do
      if isAtEnd then fail("Unterminated block comment", start)
      else if currentChar() == '/' && peekChar().contains('*') then
        advance()
        advance()
        depth += 1
      else if currentChar() == '*' && peekChar().contains('/') then
        advance()
        advance()
        depth -= 1
      else
        advance()

  private def isAtEnd: Boolean = index >= input.length

  private def currentChar(): Char = input.charAt(index)

  private def peekChar(): Option[Char] =
    val nextIndex = index + 1
    if nextIndex < input.length then Some(input.charAt(nextIndex)) else None

  private def advance(): Char =
    val ch = input.charAt(index)
    index += 1
    if ch == '\n' then
      line += 1
      column = 1
    else
      column += 1
    ch

  private def currentSpan(): Span = Span(index, line, column)

  private def isIdentifierStart(ch: Char): Boolean = ch.isLetter || ch == '_'

  private def isIdentifierPart(ch: Char): Boolean = ch.isLetterOrDigit || ch == '_'

  private def parseIntLiteral(digits: String, raw: String, span: Span, base: Int = 10): Int =
    try Integer.parseInt(digits, base)
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Int literal '$raw'", span)

  private def parseLongLiteral(digits: String, raw: String, span: Span, base: Int = 10): Long =
    try java.lang.Long.parseLong(digits, base)
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Long literal '$raw'", span)

  private def parseFloatLiteral(digits: String, raw: String, span: Span): Float =
    try
      val value = java.lang.Float.parseFloat(digits)
      if !java.lang.Float.isFinite(value) then fail(s"Invalid Float literal '$raw'", span)
      value
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Float literal '$raw'", span)

  private def parseDoubleLiteral(digits: String, raw: String, span: Span): Double =
    try
      val value = java.lang.Double.parseDouble(digits)
      if !java.lang.Double.isFinite(value) then fail(s"Invalid Double literal '$raw'", span)
      value
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Double literal '$raw'", span)

  private def isDigitForBase(ch: Char, base: Int): Boolean =
    (base == 2 && (ch == '0' || ch == '1'))
    || (base == 16 && ch.isDigit)
    || (base == 16 && (ch >= 'a' && ch <= 'f'))
    || (base == 16 && (ch >= 'A' && ch <= 'F'))

  private def fail(message: String, span: Span): Nothing =
    throw ParseException(message, span)

object Tokenizer:
  case class TokenError(message: String, span: Span):
    def format: String = s"$message at ${span.line}:${span.column}"

  def tokenize(input: String): Result[IArray[Token], TokenError] =
    new Tokenizer(input).tokenize()
