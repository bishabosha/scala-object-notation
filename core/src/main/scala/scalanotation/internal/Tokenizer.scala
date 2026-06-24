package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result
import scala.annotation.publicInBinary

private[scalanotation] trait TokenKinds:
  inline val PackageKw    = 0
  inline val ValKw        = 1
  inline val VectorId     = 2
  inline val TrueKw       = 3
  inline val FalseKw      = 4
  inline val NullKw       = 5
  inline val EmptyTupleId = 6
  inline val TupleId      = 7
  inline val Keyword      = 8
  inline val Identifier   = 9
  inline val IntLit       = 10
  inline val LongLit      = 11
  inline val FloatLit     = 12
  inline val DoubleLit    = 13
  inline val StringLit    = 14
  inline val CharLit      = 15
  inline val Equals       = 16
  inline val Dot          = 17
  inline val Plus         = 18
  inline val Minus        = 19
  inline val Comma        = 20
  inline val Semicolon    = 21
  inline val LParen       = 22
  inline val RParen       = 23
  inline val Eof          = 24

/** Unboxed token kind constants. The decoder's happy path only ever inspects these (plus the
  * unboxed offset slots in [[TokenStream]]); boxed [[Token]] values and [[DecodeError.Span]]s are
  * materialized lazily, only when a [[DecodeError]] needs to be constructed (or for
  * debugging/tests).
  */
// FIXME: publicInBinary is needed, or else extending a base trait, otherwise inlining doesnt see through
private[scalanotation] object TokenKind extends TokenKinds

/** Boxed token representation — only materialized for debugging and tests; the decode path works on
  * the unboxed slots of [[TokenStream]].
  */
private[scalanotation] enum Token:
  case PackageKw(span: DecodeError.Span)
  case ValKw(span: DecodeError.Span)
  case VectorId(span: DecodeError.Span)
  case TrueKw(span: DecodeError.Span)
  case FalseKw(span: DecodeError.Span)
  case NullKw(span: DecodeError.Span)
  case EmptyTupleId(span: DecodeError.Span)
  case TupleId(span: DecodeError.Span)
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

private[scalanotation] final class TokenizeException(val message: String, val offset: Int)
    extends Exception
    with scala.util.control.NoStackTrace

private[scalanotation] object ExperimentalFlags:
  final val None: Int       = 0
  final val AllowSIP72: Int = 1 << 0

  inline def enabled(flags: Int, flag: Int): Boolean =
    (flags & flag) != 0

/** A streaming scanner: [[scanNext]] scans a single token into the unboxed slot fields. Callers own
  * buffering (see [[TokenStream]]); the scanner itself holds no token history, so memory use is
  * bounded regardless of input size.
  */
private[scalanotation] final class Tokenizer private[internal] (
    private var input: String,
    private var index: Int,
    private var experimentalFlags: Int
):
  def this(input: String) =
    this(input, 0, ExperimentalFlags.None)

  import Tokenizer.*

  /** Repositions this scanner at the start of a new input, for reuse from a pool. */
  private[internal] def reset(
      newInput: String,
      newExperimentalFlags: Int = ExperimentalFlags.None
  ): Unit =
    input = newInput
    index = 0
    experimentalFlags = newExperimentalFlags
    kind = TokenKind.Eof
    str = null

  private[internal] def setExperimentalFlags(newExperimentalFlags: Int): Unit =
    experimentalFlags = newExperimentalFlags

  /** Tokenize the rest of the input into boxed [[Token]]s for tests and debugging; the decode path
    * streams tokens through [[TokenStream]] without materializing them.
    */
  def tokenize(debug: Boolean): Result[List[Token], DecodeError] =
    Result.catchException({ case e: TokenizeException =>
      DecodeError.TokenFormat(e.message).atToken(spanAt(input, e.offset))
    }):
      val tokens = List.newBuilder[Token]
      var done   = false
      while !done do
        scanNext()
        val token = materialize(input, this)
        tokens += token
        if debug then Console.err.println(token)
        done = kind == TokenKind.Eof
      tokens.result()

  // slots describing the most recently scanned token — unboxed in the happy path
  private[internal] var kind: Int          = TokenKind.Eof
  private[internal] var start: Int         = 0
  private[internal] var end: Int           = 0
  private[internal] var str: String | Null = null
  private[internal] var num: Long          = 0L
  private[internal] var dbl: Double        = 0.0

  private def sliceEquals(from: Int, until: Int, expected: String): Boolean =
    val len = until - from
    len == expected.length && input.regionMatches(from, expected, 0, len)

  /** Scan the next token into the slot fields. Throws [[TokenizeException]] on malformed input. */
  def scanNext(): Unit =
    skipTrivia()
    start = index
    str = null
    if isAtEnd then kind = TokenKind.Eof
    else scanToken()
    end = index

  private def scanToken(): Unit =
    currentChar() match
      case '('                            => advance(); kind = TokenKind.LParen
      case ')'                            => advance(); kind = TokenKind.RParen
      case '.'                            => advance(); kind = TokenKind.Dot
      case ','                            => advance(); kind = TokenKind.Comma
      case ';'                            => advance(); kind = TokenKind.Semicolon
      case '`'                            => scanQuotedIdentifier()
      case '"'                            => scanString()
      case '\'' if canStartDedentedString => scanDedentedString()
      case '\''                           => scanChar()
      case ch if isIdentifierStart(ch)    => scanIdentifier()
      case ch if ch.isDigit               => scanNumber()
      case ch if isOperatorPart(ch)       => scanOperator()
      case ch                             => fail(s"Unexpected character '$ch'")

  /** the raw source text of the token scanned so far — used in error messages only */
  private def rawText: String = input.substring(start, index)

  private def scanIdentifier(): Unit =
    // the identifier is a slice of the input: track offsets, no per-token builder
    while !isAtEnd && isIdentifierPart(currentChar()) do advance()
    if index > start && input.charAt(index - 1) == '_' then
      while !isAtEnd && isOperatorPart(currentChar()) do advance()
    kind =
      if sliceEquals(start, index, KW_package) then TokenKind.PackageKw
      else if sliceEquals(start, index, KW_val) then TokenKind.ValKw
      else if sliceEquals(start, index, KW_true) then TokenKind.TrueKw
      else if sliceEquals(start, index, KW_false) then TokenKind.FalseKw
      else if sliceEquals(start, index, KW_null) then TokenKind.NullKw
      else if sliceEquals(start, index, KW_Vector) then TokenKind.VectorId
      else if sliceEquals(start, index, KW_EmptyTuple) then TokenKind.EmptyTupleId
      else if sliceEquals(start, index, KW_Tuple) then TokenKind.TupleId
      else if reservedIdentifierKeywordSlice(start, index) then TokenKind.Keyword
      else TokenKind.Identifier

  private def scanQuotedIdentifier(): Unit =
    advance()
    val builder = new StringBuilder
    while !isAtEnd && currentChar() != '`' do
      currentChar() match
        case '\n' | '\r' =>
          fail("Quoted identifier cannot contain a raw newline")
        case '\\' =>
          advance()
          if isAtEnd then fail("Unterminated quoted identifier")
          builder.append(scanEscape())
        case _ =>
          builder.append(advance())
    if isAtEnd then fail("Unterminated quoted identifier")
    advance()
    kind = TokenKind.Identifier
    str = builder.result()

  private def scanEscape(): Char =
    if currentChar() == 'u' then scanUnicodeEscape()
    else decodeEscape(advance())

  private def scanUnicodeEscape(): Char =
    while !isAtEnd && currentChar() == 'u' do advance()
    var value = 0
    var count = 0
    while count < 4 do
      if isAtEnd then fail("Incomplete unicode escape sequence")
      val digit = Character.digit(currentChar(), 16)
      if digit < 0 then fail(s"Invalid unicode escape digit '${currentChar()}'")
      value = (value << 4) | digit
      advance()
      count += 1
    value.toChar

  private def scanNumber(): Unit =
    if currentChar() == '0' && (
        peekCompare('x')
          || peekCompare('X')
          || peekCompare('b')
          || peekCompare('B')
      )
    then scanPrefixedInteger()
    else scanDecimalNumber()

  private def scanOperator(): Unit =
    while !isAtEnd && isOperatorPart(currentChar()) do advance()
    val len = index - start
    // the fixed punctuation operators are classified straight from the slice — `=` especially is
    // scanned once per named-tuple field, so it must never allocate
    if len == 1 then
      input.charAt(start) match
        case '=' => kind = TokenKind.Equals
        case '+' => kind = TokenKind.Plus
        case '-' => kind = TokenKind.Minus
        case _   => classifyOperatorIdentifier()
    else classifyOperatorIdentifier()

  private def classifyOperatorIdentifier(): Unit =
    kind =
      if sliceEquals(start, index, KW_colon)
        || sliceEquals(start, index, KW_leftArrow)
        || sliceEquals(start, index, KW_arrow)
        || sliceEquals(start, index, KW_subtype)
        || sliceEquals(start, index, KW_supertype)
        || sliceEquals(start, index, KW_hash)
        || sliceEquals(start, index, KW_at)
        || sliceEquals(start, index, KW_tlArrow)
        || sliceEquals(start, index, KW_ctxArrow)
      then TokenKind.Keyword
      else TokenKind.Identifier

  private def scanPrefixedInteger(): Unit =
    advance()
    val marker = advance()

    val base =
      if marker == 'x' || marker == 'X' then 16
      else 2

    // only the shape is validated here; the value is interpreted at consumption
    // ([[Tokenizer.intValueAt]]/[[Tokenizer.longValueAt]]), once the sign is known
    var sawDigit = false
    while !isAtEnd && isDigitForBase(currentChar(), base) do
      advance()
      sawDigit = true

    while !isAtEnd && currentChar() == '_' do
      advance()
      if isAtEnd || !isDigitForBase(currentChar(), base) then
        fail(s"Expected a base-$base digit after numeric separator")
      while !isAtEnd && isDigitForBase(currentChar(), base) do
        advance()
        sawDigit = true

    if !sawDigit then fail(s"Expected at least one base-$base digit after numeric prefix")

    if !isAtEnd && currentChar().isLetterOrDigit && currentChar() != 'l' && currentChar() != 'L'
    then fail(s"Invalid digit '${currentChar()}' for base-$base literal")

    val isLong = !isAtEnd && (currentChar() == 'l' || currentChar() == 'L')
    if isLong then advance()
    kind = if isLong then TokenKind.LongLit else TokenKind.IntLit

  private def scanDecimalNumber(): Unit =
    // the digits are a slice of the input: track offsets, no per-token builder
    var hasDot      = false
    var hasExponent = false

    // reports a seen '_' separator via the result — assigning a captured local var from a local
    // def would lift the var into a heap-allocated BooleanRef on every scan
    def takeDigits(): Boolean =
      var sawSeparator = false
      while !isAtEnd && (currentChar().isDigit || currentChar() == '_') do
        if currentChar() == '_' then sawSeparator = true
        advance()
      sawSeparator

    var sawSeparator = takeDigits()
    if !isAtEnd && currentChar() == '.' && peekIsDigit() then
      hasDot = true
      advance()
      if takeDigits() then sawSeparator = true

    if !isAtEnd && (currentChar() == 'e' || currentChar() == 'E') then
      hasExponent = true
      advance()
      if !isAtEnd && (currentChar() == '+' || currentChar() == '-') then advance()
      if isAtEnd || !currentChar().isDigit then fail("Exponent requires at least one digit")
      if takeDigits() then sawSeparator = true

    val digitsEnd = index
    // '\u0000' marks "no suffix" — no Option[Char] is allocated per literal
    val suffix =
      if isAtEnd then '\u0000'
      else
        currentChar() match
          case ch @ ('l' | 'L' | 'f' | 'F' | 'd' | 'D') => advance(); ch
          case _                                        => '\u0000'

    // Int and Long are interpreted at consumption (when the sign is known) directly from the
    // input slice; only Float and Double materialize the digits here, because their parse and
    // rounding are sign-symmetric and parseFloat/parseDouble have no offset-range form on any
    // platform. The separator flag is a parameter: capturing the var would lift it into a
    // heap-allocated Ref.
    def normalizedDigits(sawSeparator: Boolean): String =
      val digits = input.substring(start, digitsEnd)
      if sawSeparator then digits.replace("_", "") else digits

    suffix match
      case 'l' | 'L' =>
        if hasDot || hasExponent then
          fail("Long literals cannot contain a decimal point or exponent")
        kind = TokenKind.LongLit
      case 'f' | 'F' =>
        kind = TokenKind.FloatLit
        dbl = parseFloatLiteral(normalizedDigits(sawSeparator)).toDouble
      case 'd' | 'D' =>
        kind = TokenKind.DoubleLit
        dbl = parseDoubleLiteral(normalizedDigits(sawSeparator))
      case _ if hasDot || hasExponent =>
        kind = TokenKind.DoubleLit
        dbl = parseDoubleLiteral(normalizedDigits(sawSeparator))
      case _ =>
        kind = TokenKind.IntLit

  private def scanString(): Unit =
    advance()
    // fast path: an escape-free string is a slice of the input, no builder needed
    val contentStart = index
    while !isAtEnd && currentChar() != '"' && currentChar() != '\\' do advance()
    if isAtEnd then fail("Unterminated string literal")
    if currentChar() == '"' then
      str = input.substring(contentStart, index)
      advance()
      kind = TokenKind.StringLit
    else
      val value = new StringBuilder(input.substring(contentStart, index))
      while !isAtEnd && currentChar() != '"' do
        if currentChar() == '\\' then
          advance()
          if isAtEnd then fail("Unterminated string literal")
          value.append(decodeEscape(advance()))
        else value.append(advance())
      if isAtEnd then fail("Unterminated string literal")
      advance()
      kind = TokenKind.StringLit
      str = value.result()

  private def canStartDedentedString: Boolean =
    ExperimentalFlags.enabled(experimentalFlags, ExperimentalFlags.AllowSIP72)
      && peekCompare('\'')
      && peekCompare('\'', 2)

  private def scanDedentedString(): Unit =
    val delimiterStart = index
    val delimiterCount = consumeQuoteRun()
    val contentStart   = index
    val chars          = new StringBuilder
    val offsets        = new Array[Int](input.length - contentStart)
    var offsetCount    = 0

    def appendContent(ch: Char, offset: Int): Unit =
      chars.append(ch)
      offsets(offsetCount) = offset
      offsetCount += 1

    var done = false
    while !done do
      if isAtEnd then failAt("unclosed multi-line string literal", delimiterStart)
      else if currentChar() == '\'' then
        val runLength = quoteRunLength()
        if runLength >= delimiterCount then
          val contentQuotes = runLength - delimiterCount
          var i             = 0
          while i < contentQuotes do
            appendContent('\'', index + i)
            i += 1
          index += runLength
          done = true
        else
          var i = 0
          while i < runLength do
            appendContent('\'', index + i)
            i += 1
          index += runLength
      else
        val offset = index
        currentChar() match
          case '\r' =>
            advance()
            if !isAtEnd && currentChar() == '\n' then advance()
            appendContent('\n', offset)
          case other =>
            advance()
            appendContent(other, offset)

    kind = TokenKind.StringLit
    str = trimDedentedString(chars.result(), offsets, offsetCount, contentStart, index)

  private def consumeQuoteRun(): Int =
    var count = 0
    while !isAtEnd && currentChar() == '\'' do
      advance()
      count += 1
    count

  private def quoteRunLength(): Int =
    var i = 0
    while index + i < input.length && input.charAt(index + i) == '\'' do i += 1
    i

  private def trimDedentedString(
      content: String,
      offsets: Array[Int],
      contentLength: Int,
      contentStart: Int,
      closingEnd: Int
  ): String =
    def offsetAt(index: Int): Int =
      if index >= 0 && index < contentLength then offsets(index)
      else if index <= 0 then contentStart
      else closingEnd

    var firstLineEnd = 0
    while firstLineEnd < contentLength && isIndentWhitespace(content.charAt(firstLineEnd)) do
      firstLineEnd += 1
    if firstLineEnd >= contentLength || content.charAt(firstLineEnd) != '\n' then
      failAt(
        "Dedented string literal must start with newline after opening quotes",
        offsetAt(firstLineEnd)
      )
    val bodyStart = firstLineEnd + 1

    var closingLineBreak = contentLength - 1
    while closingLineBreak >= 0 && isIndentWhitespace(content.charAt(closingLineBreak)) do
      closingLineBreak -= 1
    if closingLineBreak < 0 || content.charAt(closingLineBreak) != '\n' then
      failAt(
        "Last line of dedented string literal must contain only whitespace before closing delimiter",
        offsetAt(closingLineBreak)
      )

    val closingIndent = content.substring(closingLineBreak + 1, contentLength)
    val bodyEnd       = closingLineBreak
    if bodyEnd <= bodyStart then ""
    else dedentLines(content, offsets, bodyStart, bodyEnd, closingIndent)

  private def dedentLines(
      content: String,
      offsets: Array[Int],
      bodyStart: Int,
      bodyEnd: Int,
      closingIndent: String
  ): String =
    if closingIndent.isEmpty then content.substring(bodyStart, bodyEnd)
    else
      val out       = new StringBuilder
      var lineStart = bodyStart
      while lineStart < bodyEnd do
        var lineEnd = lineStart
        while lineEnd < bodyEnd && content.charAt(lineEnd) != '\n' do lineEnd += 1

        val cut =
          if lineHasPrefix(content, lineStart, lineEnd, closingIndent) then closingIndent.length
          else if isIndentOnlyLine(content, lineStart, lineEnd) then 0
          else
            failAt(
              "Line in dedented string literal must be indented at least as much as the closing delimiter",
              offsets(lineStart)
            )
        out.append(content.substring(lineStart + cut, lineEnd))
        if lineEnd < bodyEnd then
          out.append('\n')
          lineStart = lineEnd + 1
        else lineStart = bodyEnd
      out.result()

  private def lineHasPrefix(
      content: String,
      lineStart: Int,
      lineEnd: Int,
      prefix: String
  ): Boolean =
    lineEnd - lineStart >= prefix.length
      && content.regionMatches(lineStart, prefix, 0, prefix.length)

  private def isIndentOnlyLine(content: String, lineStart: Int, lineEnd: Int): Boolean =
    var i = lineStart
    while i < lineEnd do
      if !isIndentWhitespace(content.charAt(i)) then return false
      i += 1
    true

  private def isIndentWhitespace(ch: Char): Boolean =
    ch == ' ' || ch == '\t' || ch == '\f'

  private def scanChar(): Unit =
    advance()
    if isAtEnd then fail("Unterminated character literal")
    val value =
      if currentChar() == '\\' then
        advance()
        if isAtEnd then fail("Unterminated character literal")
        decodeEscape(advance())
      else advance()
    if isAtEnd || currentChar() != '\'' then
      fail("Character literal must contain exactly one character")
    advance()
    kind = TokenKind.CharLit
    num = value.toLong

  private def decodeEscape(ch: Char): Char =
    ch match
      case 'n'   => '\n'
      case 'r'   => '\r'
      case 't'   => '\t'
      case 'b'   => '\b'
      case 'f'   => '\f'
      case '\\'  => '\\'
      case '\''  => '\''
      case '"'   => '"'
      case other => fail(s"Unsupported escape sequence \\$other")

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
    val startOffset = index
    advance()
    advance()
    var depth = 1
    while depth > 0 do
      if isAtEnd then failAt("Unterminated block comment", startOffset)
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
    ch

  private def isIdentifierStart(ch: Char): Boolean =
    IdentifierSyntax.isIdentifierStart(ch)

  private def isIdentifierPart(ch: Char): Boolean =
    IdentifierSyntax.isIdentifierPart(ch)

  private def isOperatorPart(ch: Char): Boolean =
    IdentifierSyntax.isOperatorPart(ch)

  private def parseFloatLiteral(digits: String): Float =
    try
      val value = java.lang.Float.parseFloat(digits)
      if !java.lang.Float.isFinite(value) then fail(s"Invalid Float literal '$rawText'")
      value
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Float literal '$rawText'")

  private def parseDoubleLiteral(digits: String): Double =
    try
      val value = java.lang.Double.parseDouble(digits)
      if !java.lang.Double.isFinite(value) then fail(s"Invalid Double literal '$rawText'")
      value
    catch
      case _: NumberFormatException =>
        fail(s"Invalid Double literal '$rawText'")

  private def isDigitForBase(ch: Char, base: Int): Boolean =
    (base == 2 && (ch == '0' || ch == '1'))
      || (base == 16 && ch.isDigit)
      || (base == 16 && (ch >= 'a' && ch <= 'f'))
      || (base == 16 && (ch >= 'A' && ch <= 'F'))

  private def fail(message: String): Nothing =
    failAt(message, start)

  private def failAt(message: String, offset: Int): Nothing =
    throw TokenizeException(message, offset)

  private def reservedIdentifierKeywordSlice(from: Int, until: Int): Boolean =
    var index = 0
    while index < Tokenizer.reservedIdentifierKeywords.length do
      if sliceEquals(from, until, Tokenizer.reservedIdentifierKeywords(index)) then return true
      index += 1
    false

private[scalanotation] object Tokenizer:
  private val KW_val        = "val"
  private val KW_package    = "package"
  private val KW_true       = "true"
  private val KW_false      = "false"
  private val KW_null       = "null"
  private val KW_Vector     = "Vector"
  private val KW_EmptyTuple = "EmptyTuple"
  private val KW_Tuple      = "Tuple"
  private val KW_colon      = ":"
  private val KW_leftArrow  = "<-"
  private val KW_arrow      = "=>"
  private val KW_subtype    = "<:"
  private val KW_supertype  = ">:"
  private val KW_hash       = "#"
  private val KW_at         = "@"
  private val KW_tlArrow    = "=>>"
  private val KW_ctxArrow   = "?=>"

  private val reservedIdentifierKeywords: Array[String] =
    Array(
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

  /** Interprets an `IntLit` token's value in place from its input slice. The scanner validates only
    * the shape: the sign lives in a separate token, so the range check has to happen here, at
    * consumption, where the sign is known — otherwise `Int.MinValue` would be rejected, since its
    * magnitude overflows a positive Int.
    */
  private[internal] def intValueAt(input: String, start: Int, end: Int, negative: Boolean): Int =
    val limit   = if negative then Int.MinValue.toLong else -Int.MaxValue.toLong
    val negated = negatedValueAt(input, start, end, limit, "Int")
    (if negative then negated else -negated).toInt

  /** interprets a `LongLit` token's value in place from its input slice — see [[intValueAt]] */
  private[internal] def longValueAt(input: String, start: Int, end: Int, negative: Boolean): Long =
    val limit   = if negative then Long.MinValue else -Long.MaxValue
    val negated = negatedValueAt(input, start, end, limit, "Long")
    if negative then negated else -negated

  /** Accumulates the literal's magnitude negatively (mirroring `java.lang.Long.parseLong`), so
    * `MinValue`'s magnitude — one more than `MaxValue`'s — stays representable. Skips '_'
    * separators, the `0x`/`0b` prefix and the `l`/`L` suffix; no intermediate string is allocated.
    * The scanner has already validated every char in the slice.
    */
  private def negatedValueAt(
      input: String,
      start: Int,
      end: Int,
      limit: Long,
      name: String
  ): Long =
    def invalid(): Nothing =
      throw TokenizeException(s"Invalid $name literal '${input.substring(start, end)}'", start)

    var from = start
    var base = 10
    if end - start > 2 && input.charAt(start) == '0' then
      input.charAt(start + 1) match
        case 'x' | 'X' => base = 16; from = start + 2
        case 'b' | 'B' => base = 2; from = start + 2
        case _         => ()

    val multmin = limit / base
    var result  = 0L
    var i       = from
    while i < end do
      val ch = input.charAt(i)
      if ch != '_' && ch != 'l' && ch != 'L' then
        val digit = Character.digit(ch, base)
        if digit < 0 || result < multmin then invalid()
        result = result * base
        if result < limit + digit then invalid()
        result = result - digit
      i += 1
    result

  /** Materialize line/column information for an offset — error/debug path only. */
  def spanAt(input: String, offset: Int): DecodeError.Span =
    var line   = 1
    var column = 1
    var i      = 0
    val limit  = math.min(offset, input.length)
    while i < limit do
      if input.charAt(i) == '\n' then
        line += 1
        column = 1
      else column += 1
      i += 1
    DecodeError.Span(offset, line, column)

  /** Materialize the scanner's current slots as a boxed [[Token]] — debug/test path only. */
  private[internal] def materialize(input: String, scanner: Tokenizer): Token =
    val span = spanAt(input, scanner.start)
    def raw  = input.substring(scanner.start, scanner.end)
    scanner.kind match
      case TokenKind.PackageKw    => Token.PackageKw(span)
      case TokenKind.ValKw        => Token.ValKw(span)
      case TokenKind.VectorId     => Token.VectorId(span)
      case TokenKind.TrueKw       => Token.TrueKw(span)
      case TokenKind.FalseKw      => Token.FalseKw(span)
      case TokenKind.NullKw       => Token.NullKw(span)
      case TokenKind.EmptyTupleId => Token.EmptyTupleId(span)
      case TokenKind.TupleId      => Token.TupleId(span)
      case TokenKind.Keyword      => Token.Keyword(raw, span)
      case TokenKind.Identifier   =>
        Token.Identifier(if scanner.str == null then raw else scanner.str.nn, span)
      // no sign context here: the boxed token carries the positive interpretation, and a literal
      // only valid with a leading '-' (MinValue's magnitude) is rejected like any other overflow
      case TokenKind.IntLit =>
        Token.IntLit(raw, intValueAt(input, scanner.start, scanner.end, negative = false), span)
      case TokenKind.LongLit =>
        Token.LongLit(raw, longValueAt(input, scanner.start, scanner.end, negative = false), span)
      case TokenKind.FloatLit  => Token.FloatLit(raw, scanner.dbl.toFloat, span)
      case TokenKind.DoubleLit => Token.DoubleLit(raw, scanner.dbl, span)
      case TokenKind.StringLit => Token.StringLit(raw, scanner.str.nn, span)
      case TokenKind.CharLit   => Token.CharLit(raw, scanner.num.toChar, span)
      case TokenKind.Equals    => Token.Equals(span)
      case TokenKind.Dot       => Token.Dot(span)
      case TokenKind.Plus      => Token.Plus(span)
      case TokenKind.Minus     => Token.Minus(span)
      case TokenKind.Comma     => Token.Comma(span)
      case TokenKind.Semicolon => Token.Semicolon(span)
      case TokenKind.LParen    => Token.LParen(span)
      case TokenKind.RParen    => Token.RParen(span)
      case _                   => Token.Eof(span)

  /** Tokenize the whole input into boxed [[Token]]s — for tests and debugging only; the decode path
    * streams tokens through [[TokenStream]] without materializing them.
    */
  def tokenize(input: String, debug: Boolean): Result[List[Token], DecodeError] =
    Tokenizer(input).tokenize(debug)

/** A bounded buffer over a streaming [[Tokenizer]]: at most three tokens (the current token plus
  * two lookahead tokens) are buffered at any time, held in vectorized slots — parallel arrays with
  * an unboxed Int kind slot and unboxed Int offset slots per token. Boxed [[Token]]s and
  * [[DecodeError.Span]]s are only materialized when constructing a [[DecodeError]] (or for debug
  * output).
  */
private[scalanotation] abstract class TokenStream private[internal] (
    private var input: String,
    private var debug: Boolean,
    scanOnInit: Boolean
) extends PushSlots {
  def this(input: String, debug: Boolean) =
    this(input, debug, scanOnInit = true)

  private val scanner =
    Tokenizer(input, 0, ExperimentalFlags.None)

  private var experimentalFlags = ExperimentalFlags.None

  // vectorized slots: a tiny ring holding current, first lookahead, and second lookahead
  private val kinds  = new Array[Int](3)
  private val starts = new Array[Int](3)
  private val ends   = new Array[Int](3)
  private val strs   = new Array[String | Null](3)
  private val nums   = new Array[Long](3)
  private val dbls   = new Array[Double](3)

  private var cur            = 0
  private var lookaheadCount = 0

  if scanOnInit then scanIntoSlot(cur) // initialize the current token

  /** Re-aims this stream at the start of a new input, for reuse from a pool. */
  protected def resetStream(newInput: String, newDebug: Boolean): Unit =
    input = newInput
    debug = newDebug
    experimentalFlags = ExperimentalFlags.None
    scanner.reset(newInput, experimentalFlags)
    strs(0) = null // release references to the previous input's strings
    strs(1) = null
    strs(2) = null
    cur = 0
    lookaheadCount = 0
    scanIntoSlot(cur)

  private def scanIntoSlot(slot: Int): Unit =
    scanner.scanNext()
    kinds(slot) = scanner.kind
    starts(slot) = scanner.start
    ends(slot) = scanner.end
    strs(slot) = scanner.str
    nums(slot) = scanner.num
    dbls(slot) = scanner.dbl
    if debug then
      Console.err.println(
        Tokenizer.materialize(input, scanner)
      ) // TODO: should allow logger customization in the future

  protected final def addExperimentalFlags(flags: Int): Unit =
    experimentalFlags |= flags
    scanner.setExperimentalFlags(experimentalFlags)

  protected def currentKind(): Int   = kinds(cur)
  protected def currentOffset(): Int = starts(cur)

  private def slot(offset: Int): Int =
    val next = cur + offset
    if next >= 3 then next - 3 else next

  private def ensureLookahead(count: Int): Unit =
    while lookaheadCount < count do
      lookaheadCount += 1
      scanIntoSlot(slot(lookaheadCount))

  protected def peekKind(): Int =
    ensureLookahead(1)
    kinds(slot(1))

  protected def peekSecondKind(): Int =
    ensureLookahead(2)
    kinds(slot(2))

  protected def advance(): Unit =
    if lookaheadCount > 0 then
      cur = slot(1)
      lookaheadCount -= 1
    else if kinds(cur) != TokenKind.Eof then scanIntoSlot(cur)

  // payload accessors — happy path, unboxed where primitive. Int and Long take the sign decoded
  // from the preceding token: the value is interpreted here, in place from the input slice, so
  // MinValue (whose magnitude overflows the positive range) parses once the sign is known.
  protected def currentName(): String        = nameAt(cur)
  protected def currentStringValue(): String = strs(cur).nn
  protected def currentCharValue(): Char     = nums(cur).toChar
  protected def currentFloatValue(): Float   = dbls(cur).toFloat
  protected def currentDoubleValue(): Double = dbls(cur)

  private def nameAt(slot: Int): String =
    val cached = strs(slot)
    if cached != null then cached
    else
      val name = input.substring(starts(slot), ends(slot))
      strs(slot) = name
      name

  protected def currentFieldName(): String =
    currentKind() match
      case TokenKind.Identifier   => currentName()
      case TokenKind.VectorId     => "Vector"
      case TokenKind.EmptyTupleId => "EmptyTuple"
      case TokenKind.TupleId      => "Tuple"
      case TokenKind.Plus         => "+"
      case TokenKind.Minus        => "-"
      case _                      => currentName()

  protected def currentFieldNameMatches(expected: String): Boolean =
    currentKind() match
      case TokenKind.Identifier =>
        val cached = strs(cur)
        if cached != null then cached == expected
        else sliceMatches(cur, expected)
      case TokenKind.VectorId     => expected == "Vector"
      case TokenKind.EmptyTupleId => expected == "EmptyTuple"
      case TokenKind.TupleId      => expected == "Tuple"
      case TokenKind.Plus         => expected == "+"
      case TokenKind.Minus        => expected == "-"
      case _                      => false

  private def sliceMatches(slot: Int, expected: String): Boolean =
    val from = starts(slot)
    val len  = ends(slot) - from
    len == expected.length && input.regionMatches(from, expected, 0, len)

  protected def currentIntValue(negative: Boolean): Int =
    Tokenizer.intValueAt(input, starts(cur), ends(cur), negative)

  protected def currentLongValue(negative: Boolean): Long =
    Tokenizer.longValueAt(input, starts(cur), ends(cur), negative)

  // error-path materialization
  protected def spanAt(offset: Int): DecodeError.Span = Tokenizer.spanAt(input, offset)
  protected def currentSpan(): DecodeError.Span       = spanAt(starts(cur))

  protected def describeCurrent(): String = describeSlot(cur)

  private def describeSlot(slot: Int): String =
    def raw = input.substring(starts(slot), ends(slot))
    kinds(slot) match
      case TokenKind.PackageKw    => "'package'"
      case TokenKind.ValKw        => "'val'"
      case TokenKind.VectorId     => "'Vector'"
      case TokenKind.TrueKw       => "'true'"
      case TokenKind.FalseKw      => "'false'"
      case TokenKind.NullKw       => "'null'"
      case TokenKind.EmptyTupleId => "'EmptyTuple'"
      case TokenKind.TupleId      => "'Tuple'"
      case TokenKind.Keyword      => s"'$raw'"
      case TokenKind.Identifier   => s"identifier '${nameAt(slot)}'"
      case TokenKind.IntLit       => s"integer literal '$raw'"
      case TokenKind.LongLit      => s"long literal '$raw'"
      case TokenKind.FloatLit     => s"float literal '$raw'"
      case TokenKind.DoubleLit    => s"double literal '$raw'"
      case TokenKind.StringLit    => s"string literal $raw"
      case TokenKind.CharLit      => s"character literal '$raw'"
      case TokenKind.Equals       => "'='"
      case TokenKind.Dot          => "'.'"
      case TokenKind.Plus         => "'+'"
      case TokenKind.Minus        => "'-'"
      case TokenKind.Comma        => "','"
      case TokenKind.Semicolon    => "';'"
      case TokenKind.LParen       => "'('"
      case TokenKind.RParen       => "')'"
      case _                      => "end of input"
}
