package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result

/** Unboxed token kind constants. The decoder's happy path only ever inspects these (plus the
  * unboxed offset slots in [[TokenStream]]); boxed [[Token]] values and [[DecodeError.Span]]s are
  * materialized lazily, only when a [[DecodeError]] needs to be constructed (or for
  * debugging/tests).
  */
private[scalanotation] object TokenKind:
  final val PackageKw    = 0
  final val ValKw        = 1
  final val VectorId     = 2
  final val TrueKw       = 3
  final val FalseKw      = 4
  final val NullKw       = 5
  final val EmptyTupleId = 6
  final val TupleId      = 7
  final val Keyword      = 8
  final val Identifier   = 9
  final val IntLit       = 10
  final val LongLit      = 11
  final val FloatLit     = 12
  final val DoubleLit    = 13
  final val StringLit    = 14
  final val CharLit      = 15
  final val Equals       = 16
  final val Dot          = 17
  final val Plus         = 18
  final val Minus        = 19
  final val Comma        = 20
  final val Semicolon    = 21
  final val LParen       = 22
  final val RParen       = 23
  final val Eof          = 24

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

/** A streaming scanner: [[scanNext]] scans a single token into the unboxed slot fields. Callers own
  * buffering (see [[TokenStream]]); the scanner itself holds no token history, so memory use is
  * bounded regardless of input size.
  */
private[scalanotation] final class Tokenizer private[internal] (
    private var input: String,
    private var index: Int,
    // Whether token payloads are materialized. Lookahead scouts read only token kinds, so they
    // disable this to skip per-name, string and numeric payload work.
    private var internNames: Boolean,
    // Whether interned names are cached in [[internTable]]. One-shot scanners disable this so the
    // fixed table is not allocated per decode; names are still materialized, just uncached.
    private var cacheNames: Boolean
):
  def this(input: String) = this(input, 0, internNames = true, cacheNames = true)

  import Tokenizer.*

  /** Repositions this scanner at the start of a new input, for reuse from a pool. The name cache is
    * kept: interned names stay useful across inputs.
    */
  private[internal] def reset(newInput: String): Unit =
    input = newInput
    index = 0
    kind = TokenKind.Eof
    str = null

  /** Tokenize the rest of the input into boxed [[Token]]s — kept for binary compatibility with the
    * eager tokenizer, and used for tests and debugging; the decode path streams tokens through
    * [[TokenStream]] without materializing them.
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

  @deprecated("Kept for binary compatibility; will be removed in a future version", "0.3.6")
  private class ParseException(val message: String, val span: DecodeError.Span)
      extends Exception
      with scala.util.control.NoStackTrace:
    // retained only for binary compatibility with the eager tokenizer
    // reference the outer scanner so the constructor keeps its original outer-pointer parameter
    def offset: Int = ParseException.this.span.offset.max(Tokenizer.this.start)

  @deprecated("Kept for binary compatibility; will be removed in a future version", "0.3.6")
  private inline def __Token: Token.type =
    // retained only for binary compatibility with the eager tokenizer
    // (regenerates the `inline$Token` accessor)
    Token

  // slots describing the most recently scanned token — unboxed in the happy path
  private[internal] var kind: Int          = TokenKind.Eof
  private[internal] var start: Int         = 0
  private[internal] var end: Int           = 0
  private[internal] var str: String | Null = null
  private[internal] var num: Long          = 0L
  private[internal] var dbl: Double        = 0.0

  // Bounded, direct-mapped intern table for identifier and field-name strings, keyed by content.
  // A hash collision overwrites the prior entry rather than growing, so the table never leaks; the
  // cost is only that a colliding name re-allocates. Because entries can be dropped, callers must
  // compare names by value (never by reference). Allocated lazily on first cached intern and kept
  // across [[reset]] so the cache stays warm for a pooled scanner; substrings copy their chars
  // (JDK 7u6+), so cached names never pin old inputs.
  private var internTable: Array[String | Null] | Null = null

  private def internTableOrAlloc(): Array[String | Null] =
    val table = internTable
    if table != null then table
    else
      val fresh = new Array[String | Null](InternTableSize)
      internTable = fresh
      fresh

  /** Interns the input slice `[from, until)`. With caching, a hit returns the cached instance with
    * no allocation and a miss allocates the substring, stores it (overwriting any colliding entry)
    * and returns it. Without caching, it is a plain substring. Only called when [[internNames]].
    */
  private def internSlice(from: Int, until: Int): String =
    if !cacheNames then input.substring(from, until)
    else
      val table = internTableOrAlloc()
      val len   = until - from
      var h     = 0
      var i     = from
      while i < until do
        h = h * 31 + input.charAt(i)
        i += 1
      val slot     = h & (table.length - 1)
      val existing = table(slot)
      if existing != null && existing.length == len && input.regionMatches(from, existing, 0, len)
      then existing
      else
        val name = input.substring(from, until)
        table(slot) = name
        name

  /** Interns an already-materialized name — a quoted identifier, whose value can differ from its
    * input slice because of escapes, so it cannot be interned by offsets. Shares the intern table:
    * `String.hashCode` is the same 31-polynomial as [[internSlice]]'s hash, so equal content lands
    * in the same slot. A no-op when caching is disabled.
    */
  private def internString(s: String): String =
    if !cacheNames then s
    else
      val table    = internTableOrAlloc()
      val slot     = s.hashCode & (table.length - 1)
      val existing = table(slot)
      if existing != null && existing == s then existing
      else
        table(slot) = s
        s

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
      case '('                         => advance(); kind = TokenKind.LParen
      case ')'                         => advance(); kind = TokenKind.RParen
      case '.'                         => advance(); kind = TokenKind.Dot
      case ','                         => advance(); kind = TokenKind.Comma
      case ';'                         => advance(); kind = TokenKind.Semicolon
      case '`'                         => scanQuotedIdentifier()
      case '"'                         => scanString()
      case '\''                        => scanChar()
      case ch if isIdentifierStart(ch) => scanIdentifier()
      case ch if ch.isDigit            => scanNumber()
      case ch if isOperatorPart(ch)    => scanOperator()
      case ch                          => fail(s"Unexpected character '$ch'")

  /** the raw source text of the token scanned so far — used in error messages only */
  private def rawText: String = input.substring(start, index)

  private def scanIdentifier(): Unit =
    // the identifier is a slice of the input: track offsets, no per-token builder
    while !isAtEnd && isIdentifierPart(currentChar()) do advance()
    if index > start && input.charAt(index - 1) == '_' then
      while !isAtEnd && isOperatorPart(currentChar()) do advance()
    if !internNames then
      kind =
        if sliceEquals(start, index, KW_true) then TokenKind.TrueKw
        else if sliceEquals(start, index, KW_false) then TokenKind.FalseKw
        else if sliceEquals(start, index, KW_null) then TokenKind.NullKw
        else if sliceEquals(start, index, KW_Vector) then TokenKind.VectorId
        else if sliceEquals(start, index, KW_EmptyTuple) then TokenKind.EmptyTupleId
        else if sliceEquals(start, index, KW_Tuple) then TokenKind.TupleId
        else TokenKind.Identifier
    else
      // intern first (allocation-free on a hit), then classify the cached string: the hard
      // keywords need no `str`, identifiers and reserved keywords carry the interned name
      val name = internSlice(start, index)
      name match
        case KW_package    => kind = TokenKind.PackageKw
        case KW_val        => kind = TokenKind.ValKw
        case KW_true       => kind = TokenKind.TrueKw
        case KW_false      => kind = TokenKind.FalseKw
        case KW_null       => kind = TokenKind.NullKw
        case KW_Vector     => kind = TokenKind.VectorId
        case KW_EmptyTuple => kind = TokenKind.EmptyTupleId
        case KW_Tuple      => kind = TokenKind.TupleId
        case _             =>
          kind =
            if reservedIdentifierKeywords.contains(name) then TokenKind.Keyword
            else TokenKind.Identifier
          str = name

  private def scanQuotedIdentifier(): Unit =
    advance()
    if !internNames then
      while !isAtEnd && currentChar() != '`' do
        currentChar() match
          case '\n' | '\r' =>
            fail("Quoted identifier cannot contain a raw newline")
          case '\\' =>
            advance()
            if isAtEnd then fail("Unterminated quoted identifier")
            scanEscape()
          case _ =>
            advance()
      if isAtEnd then fail("Unterminated quoted identifier")
      advance()
      kind = TokenKind.Identifier
    else
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
      str = internString(builder.result())

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
    // scanned once per named-tuple field, so it must never allocate or touch the intern table
    if len == 1 then
      input.charAt(start) match
        case '=' => kind = TokenKind.Equals
        case '+' => kind = TokenKind.Plus
        case '-' => kind = TokenKind.Minus
        case _   => classifyOperatorIdentifier()
    else classifyOperatorIdentifier()

  private def classifyOperatorIdentifier(): Unit =
    if !internNames then
      // Scouts only need to distinguish fixed punctuation from generic identifiers, so neither
      // keyword classification nor interning is needed for operator identifiers.
      kind = TokenKind.Identifier
    else
      val op = internSlice(start, index)
      op match
        case KW_colon | KW_leftArrow | KW_arrow | KW_subtype | KW_supertype | KW_hash | KW_at |
            KW_tlArrow | KW_ctxArrow =>
          kind = TokenKind.Keyword
          str = op
        case _ =>
          kind = TokenKind.Identifier
          str = op

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
        if internNames then dbl = parseFloatLiteral(normalizedDigits(sawSeparator)).toDouble
      case 'd' | 'D' =>
        kind = TokenKind.DoubleLit
        if internNames then dbl = parseDoubleLiteral(normalizedDigits(sawSeparator))
      case _ if hasDot || hasExponent =>
        kind = TokenKind.DoubleLit
        if internNames then dbl = parseDoubleLiteral(normalizedDigits(sawSeparator))
      case _ =>
        kind = TokenKind.IntLit

  private def scanString(): Unit =
    advance()
    if !internNames then
      var done = false
      while !done do
        if isAtEnd then fail("Unterminated string literal")
        currentChar() match
          case '"' =>
            advance()
            done = true
          case '\\' =>
            advance()
            if isAtEnd then fail("Unterminated string literal")
            decodeEscape(advance())
          case _ =>
            advance()
      kind = TokenKind.StringLit
    else
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
    if internNames then num = value.toLong

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

private[scalanotation] object Tokenizer:
  // capacity of the per-scanner intern table; power of two so the hash maps in with a bitmask.
  // Sized for the distinct identifier/field-name count of a typical decode with headroom; a
  // higher-diversity input (e.g. a large dict) simply sees more collisions and re-allocates.
  private final val InternTableSize = 512

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
      case TokenKind.Keyword      => Token.Keyword(scanner.str.nn, span)
      case TokenKind.Identifier   => Token.Identifier(scanner.str.nn, span)
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
    cacheNamesOnInit: Boolean,
    scanOnInit: Boolean
) extends PushSlots {
  def this(input: String, debug: Boolean) =
    this(input, debug, cacheNamesOnInit = true, scanOnInit = true)

  private val scanner =
    Tokenizer(input, 0, internNames = true, cacheNames = cacheNamesOnInit)

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
    scanner.reset(newInput)
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
  protected def currentName(): String        = strs(cur).nn
  protected def currentStringValue(): String = strs(cur).nn
  protected def currentCharValue(): Char     = nums(cur).toChar
  protected def currentFloatValue(): Float   = dbls(cur).toFloat
  protected def currentDoubleValue(): Double = dbls(cur)

  protected def currentIntValue(negative: Boolean): Int =
    Tokenizer.intValueAt(input, starts(cur), ends(cur), negative)

  protected def currentLongValue(negative: Boolean): Long =
    Tokenizer.longValueAt(input, starts(cur), ends(cur), negative)

  // error-path materialization
  protected def spanAt(offset: Int): DecodeError.Span = Tokenizer.spanAt(input, offset)
  protected def currentSpan(): DecodeError.Span       = spanAt(starts(cur))

  protected def peekSpan(): DecodeError.Span =
    ensureLookahead(1)
    spanAt(starts(slot(1)))

  protected def describeCurrent(): String = describeSlot(cur)

  protected def describePeek(): String =
    ensureLookahead(1)
    describeSlot(slot(1))

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
      case TokenKind.Keyword      => s"'${strs(slot)}'"
      case TokenKind.Identifier   => s"identifier '${strs(slot)}'"
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
