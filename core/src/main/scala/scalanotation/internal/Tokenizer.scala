package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result

import scala.collection.mutable

private[scalanotation] enum Token:
  case PackageKw(span: DecodeError.Span)
  case ValKw(span: DecodeError.Span)
  case VectorId(span: DecodeError.Span)
  case TrueKw(span: DecodeError.Span)
  case FalseKw(span: DecodeError.Span)
  case NullKw(span: DecodeError.Span)
  case Keyword(raw: String, span: DecodeError.Span)
  case Identifier(name: String, span: DecodeError.Span)
  case IntLit(raw: String, value: Int, span: DecodeError.Span)
  case LongLit(raw: String, value: Long, span: DecodeError.Span)
  case FloatLit(raw: String, value: Float, span: DecodeError.Span)
  case DoubleLit(raw: String, value: Double, span: DecodeError.Span)
  case StringLit(raw: String, value: String, span: DecodeError.Span)
  case CharLit(raw: String, value: Char, span: DecodeError.Span)
  case Equals(span: DecodeError.Span)
  case Dot(span: DecodeError.Span)
  case Plus(span: DecodeError.Span)
  case Minus(span: DecodeError.Span)
  case Comma(span: DecodeError.Span)
  case Semicolon(span: DecodeError.Span)
  case LParen(span: DecodeError.Span)
  case RParen(span: DecodeError.Span)
  case Eof(span: DecodeError.Span)

  def span: DecodeError.Span

private[scalanotation] object Token:
  lazy val Empty: Token = Token.Eof(DecodeError.Span(0, 0, 0))

  given DefaultToken: PublicInternal.HasDefault[Token]:
    val Default: Token = Token.Empty

private[scalanotation] final class Tokenizer(input: String):
  import Tokenizer.*

  private var index  = 0
  private var line   = 1
  private var column = 1

  private val namesCache              = mutable.HashMap.empty[String, String]
  private def nameCached(str: String) = namesCache.getOrElseUpdate(str, str)

  private class ParseException(val message: String, val span: DecodeError.Span)
      extends Exception
      with scala.util.control.NoStackTrace

  private inline def keyword(raw: String, start: DecodeError.Span): Token =
    Token.Keyword(raw, start)

  def tokenize(debug: Boolean): Result[List[Token], DecodeError] =
    Result.catchException({ case e: ParseException =>
      DecodeError.TokenFormat(e.message).atToken(e.span)
    }):
      val tokens = List.newBuilder[Token]
      while !isAtEnd do
        skipTrivia()
        if !isAtEnd then
          val token = nextToken()
          tokens += token
          if debug then
            Console.err.println(token) // TODO: should allow logger customization in the future
      val finalToken = Token.Eof(currentSpan())
      tokens += finalToken
      if debug then Console.err.println(finalToken)
      tokens.result()

  private def nextToken(): Token =
    val start = currentSpan()
    currentChar() match
      case '('                         => advance(); Token.LParen(start)
      case ')'                         => advance(); Token.RParen(start)
      case '.'                         => advance(); Token.Dot(start)
      case ','                         => advance(); Token.Comma(start)
      case ';'                         => advance(); Token.Semicolon(start)
      case '`'                         => scanQuotedIdentifier(start)
      case '"'                         => scanString(start)
      case '\''                        => scanChar(start)
      case ch if isIdentifierStart(ch) => scanIdentifier(start)
      case ch if ch.isDigit            => scanNumber(start)
      case ch if isOperatorPart(ch)    => scanOperator(start)
      case ch                          => fail(s"Unexpected character '$ch'", start)

  private def scanIdentifier(start: DecodeError.Span): Token =
    val builder = new StringBuilder
    while !isAtEnd && isIdentifierPart(currentChar()) do builder += advance()
    if builder.nonEmpty && builder.charAt(builder.length - 1) == '_' then
      while !isAtEnd && isOperatorPart(currentChar()) do builder += advance()
    builder.result() match
      case KW_package                                        => Token.PackageKw(start)
      case KW_val                                            => Token.ValKw(start)
      case KW_true                                           => Token.TrueKw(start)
      case KW_false                                          => Token.FalseKw(start)
      case KW_null                                           => Token.NullKw(start)
      case KW_Vector                                         => Token.VectorId(start)
      case name if reservedIdentifierKeywords.contains(name) =>
        keyword(name, start)
      case name =>
        Token.Identifier(nameCached(name), start)

  private def scanQuotedIdentifier(start: DecodeError.Span): Token =
    advance()
    val builder = new StringBuilder
    while !isAtEnd && currentChar() != '`' do
      currentChar() match
        case '\n' | '\r' =>
          fail("Quoted identifier cannot contain a raw newline", start)
        case '\\' =>
          advance()
          if isAtEnd then fail("Unterminated quoted identifier", start)
          builder += scanEscape(start)
        case _ =>
          builder += advance()
    if isAtEnd then fail("Unterminated quoted identifier", start)
    advance()
    Token.Identifier(nameCached(builder.result()), start)

  private def scanEscape(start: DecodeError.Span): Char =
    if currentChar() == 'u' then scanUnicodeEscape(start)
    else decodeEscape(advance(), start)

  private def scanUnicodeEscape(start: DecodeError.Span): Char =
    while !isAtEnd && currentChar() == 'u' do advance()
    var value = 0
    var count = 0
    while count < 4 do
      if isAtEnd then fail("Incomplete unicode escape sequence", start)
      val digit = Character.digit(currentChar(), 16)
      if digit < 0 then fail(s"Invalid unicode escape digit '${currentChar()}'", start)
      value = (value << 4) | digit
      advance()
      count += 1
    value.toChar

  private def scanNumber(start: DecodeError.Span): Token =
    if currentChar() == '0' && (
        peekCompare('x')
          || peekCompare('X')
          || peekCompare('b')
          || peekCompare('B')
      )
    then scanPrefixedInteger(start)
    else scanDecimalNumber(start)

  private def scanOperator(start: DecodeError.Span): Token =
    val builder = new StringBuilder
    while !isAtEnd && isOperatorPart(currentChar()) do builder += advance()
    builder.result() match
      case "=" => Token.Equals(start)
      case "+" => Token.Plus(start)
      case "-" => Token.Minus(start)
      case KW_colon | KW_leftArrow | KW_arrow | KW_subtype | KW_supertype | KW_hash | KW_at |
          KW_tlArrow | KW_ctxArrow =>
        keyword(builder.result(), start)
      case name =>
        Token.Identifier(nameCached(name), start)

  private def scanPrefixedInteger(start: DecodeError.Span): Token =
    val prefix = new StringBuilder
    prefix += advance()
    val marker = advance()
    prefix += marker

    val base =
      if marker == 'x' || marker == 'X' then 16
      else 2

    val digits   = new StringBuilder
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

    if !sawDigit then
      fail(
        s"Expected at least one base-$base digit after numeric prefix",
        start
      )

    if !isAtEnd && currentChar().isLetterOrDigit && currentChar() != 'l' && currentChar() != 'L'
    then fail(s"Invalid digit '${currentChar()}' for base-$base literal", start)

    val suffix =
      if !isAtEnd && (currentChar() == 'l' || currentChar() == 'L') then Some(advance())
      else None

    val rawDigits        = digits.result()
    val raw              = prefix.result() + rawDigits + suffix.fold("")(_.toString)
    val normalizedDigits = rawDigits.replace("_", "")

    suffix match
      case Some(_) =>
        Token.LongLit(
          raw,
          parseLongLiteral(normalizedDigits, raw, start, base),
          start
        )
      case None =>
        Token.IntLit(
          raw,
          parseIntLiteral(normalizedDigits, raw, start, base),
          start
        )

  private def scanDecimalNumber(start: DecodeError.Span): Token =
    val builder     = new StringBuilder
    var hasDot      = false
    var hasExponent = false

    def takeDigits(): Unit =
      while !isAtEnd && (currentChar().isDigit || currentChar() == '_') do builder += advance()

    takeDigits()
    if !isAtEnd && currentChar() == '.' && peekIsDigit() then
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

    val raw              = builder.result() + suffix.fold("")(_.toString)
    val normalizedDigits = builder.result().replace("_", "")

    suffix match
      case Some('l' | 'L') =>
        if hasDot || hasExponent then
          fail(
            "Long literals cannot contain a decimal point or exponent",
            start
          )
        Token.LongLit(
          raw,
          parseLongLiteral(normalizedDigits, raw, start),
          start
        )
      case Some('f' | 'F') =>
        Token.FloatLit(
          raw,
          parseFloatLiteral(normalizedDigits, raw, start),
          start
        )
      case Some('d' | 'D') =>
        Token.DoubleLit(
          raw,
          parseDoubleLiteral(normalizedDigits, raw, start),
          start
        )
      case Some(_) =>
        fail(s"unrecognised numeric literal suffix '${suffix.get}'", start)
      case None if hasDot || hasExponent =>
        Token.DoubleLit(
          raw,
          parseDoubleLiteral(normalizedDigits, raw, start),
          start
        )
      case None =>
        Token.IntLit(raw, parseIntLiteral(normalizedDigits, raw, start), start)

  private def scanString(start: DecodeError.Span): Token =
    val raw   = new StringBuilder
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

  private def scanChar(start: DecodeError.Span): Token =
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
    if isAtEnd || currentChar() != '\'' then
      fail("Character literal must contain exactly one character", start)
    raw += advance()
    Token.CharLit(raw.result(), value, start)

  private def decodeEscape(ch: Char, start: DecodeError.Span): Char =
    ch match
      case 'n'   => '\n'
      case 'r'   => '\r'
      case 't'   => '\t'
      case 'b'   => '\b'
      case 'f'   => '\f'
      case '\\'  => '\\'
      case '\''  => '\''
      case '"'   => '"'
      case other => fail(s"Unsupported escape sequence \\$other", start)

  private def skipTrivia(): Unit =
    var keepGoing = true
    while keepGoing && !isAtEnd do
      skipWhitespace()
      if isAtEnd then keepGoing = false
      else if currentChar() == '/' && peekCompare('/') then skipLineComment()
      else if currentChar() == '/' && peekCompare('*') then skipBlockComment()
      else keepGoing = false

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
      else if currentChar() == '/' && peekCompare('*') then
        advance()
        advance()
        depth += 1
      else if currentChar() == '*' && peekCompare('/') then
        advance()
        advance()
        depth -= 1
      else advance()

  private def isAtEnd: Boolean = index >= input.length

  private def currentChar(): Char = input.charAt(index)

  private def peekCompare(expected: Char, offset: Int = 1): Boolean =
    val nextIndex = index + offset
    nextIndex < input.length && input.charAt(nextIndex) == expected

  private def peekIsDigit(offset: Int = 1): Boolean =
    val nextIndex = index + offset
    nextIndex < input.length && input.charAt(nextIndex).isDigit

  private def advance(): Char =
    val ch = input.charAt(index)
    index += 1
    if ch == '\n' then
      line += 1
      column = 1
    else column += 1
    ch

  private def currentSpan(): DecodeError.Span = DecodeError.Span(index, line, column)

  private def isIdentifierStart(ch: Char): Boolean =
    IdentifierSyntax.isIdentifierStart(ch)

  private def isIdentifierPart(ch: Char): Boolean =
    IdentifierSyntax.isIdentifierPart(ch)

  private def isOperatorPart(ch: Char): Boolean =
    IdentifierSyntax.isOperatorPart(ch)

  private def parseIntLiteral(
      digits: String,
      raw: String,
      span: DecodeError.Span,
      base: Int = 10
  ): Int =
    try Integer.parseInt(digits, base)
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Int literal '$raw'", span)

  private def parseLongLiteral(
      digits: String,
      raw: String,
      span: DecodeError.Span,
      base: Int = 10
  ): Long =
    try java.lang.Long.parseLong(digits, base)
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Long literal '$raw'", span)

  private def parseFloatLiteral(
      digits: String,
      raw: String,
      span: DecodeError.Span
  ): Float =
    try
      val value = java.lang.Float.parseFloat(digits)
      if !java.lang.Float.isFinite(value) then fail(s"Invalid Float literal '$raw'", span)
      value
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Float literal '$raw'", span)

  private def parseDoubleLiteral(
      digits: String,
      raw: String,
      span: DecodeError.Span
  ): Double =
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

  private def fail(message: String, span: DecodeError.Span): Nothing =
    throw ParseException(message, span)

private[scalanotation] object Tokenizer:
  private val KW_val       = "val"
  private val KW_package   = "package"
  private val KW_true      = "true"
  private val KW_false     = "false"
  private val KW_null      = "null"
  private val KW_Vector    = "Vector"
  private val KW_colon     = ":"
  private val KW_leftArrow = "<-"
  private val KW_arrow     = "=>"
  private val KW_subtype   = "<:"
  private val KW_supertype = ">:"
  private val KW_hash      = "#"
  private val KW_at        = "@"
  private val KW_tlArrow   = "=>>"
  private val KW_ctxArrow  = "?=>"

  private val reservedIdentifierKeywords: Set[String] =
    Set(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "enum",
      "export",
      "extends",
      "final",
      "finally",
      "for",
      "given",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "then",
      "throw",
      "trait",
      "try",
      "type",
      "var",
      "while",
      "with",
      "yield"
    )

  // TODO: made private so we could evolve the token stream format without breaking the public API.
  def tokenize(input: String, debug: Boolean): Result[List[Token], DecodeError] =
    Tokenizer(input).tokenize(debug)
