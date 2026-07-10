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
  inline val LBracket     = 25
  inline val RBracket     = 26
  inline val Arrow        = 27
  inline val LBrace       = 28
  inline val RBrace       = 29

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
  case LBracket(span: DecodeError.Span)
  case RBracket(span: DecodeError.Span)
  case Arrow(span: DecodeError.Span)
  case LBrace(span: DecodeError.Span)
  case RBrace(span: DecodeError.Span)
  case Eof(span: DecodeError.Span)

  def span: DecodeError.Span

private[scalanotation] object Token:
  lazy val Empty: Token = Token.Eof(DecodeError.Span(0, 0, 0))

  given DefaultToken: PublicInternal.HasDefault[Token]:
    val Default: Token = Token.Empty

private[scalanotation] final class TokenizeException(val message: String, val offset: Int)
    extends Exception
    with scala.util.control.NoStackTrace

/** One scanned token as unboxed slot fields. The scanner owns one instance that it scans into;
  * [[TokenStream]] keeps two more as a lookahead buffer and copies into them only when a peek has
  * to preserve the current token.
  */
private[internal] final class TokenSlots:
  var kind: Int          = TokenKind.Eof
  var start: Int         = 0
  var end: Int           = 0
  var str: String | Null = null
  var num: Long          = 0L
  var dbl: Double        = 0.0

  /** For IntLit/LongLit: digit count of the base-10 negated value accumulated into [[num]] during
    * the scan, or [[Tokenizer.AccumulatorInvalid]] when the literal needs the slow interpretation
    * (separators, prefixes, unicode digits, more than [[Tokenizer.MaxAccumulatedDigits]] digits).
    * Meaningless for other kinds.
    */
  var accDigits: Int = Tokenizer.AccumulatorInvalid

  def copyFrom(other: TokenSlots): Unit =
    kind = other.kind
    start = other.start
    end = other.end
    str = other.str
    num = other.num
    dbl = other.dbl
    accDigits = other.accDigits

private[scalanotation] object ExperimentalFlags:
  final val None: Int                    = 0
  final val AllowSIP72: Int              = 1 << 0
  final val AllowCollectionLiterals: Int = 1 << 1

  inline def enabled(flags: Int, flag: Int): Boolean =
    (flags & flag) != 0

/** A streaming scanner: [[scanNext]] scans a single token into the unboxed slot fields. Callers own
  * buffering (see [[TokenStream]]); the scanner itself holds no token history, so memory use is
  * bounded regardless of input size.
  */
private[scalanotation] final class Tokenizer private[internal] (
    initialInput: String,
    private var index: Int,
    private var experimentalFlags: Int
) extends Scan:
  def this(input: String) =
    this(input, 0, ExperimentalFlags.None)

  import Tokenizer.*

  /** The decoded input as a char array — the scanner's only view of the input: every scan reads
    * plain array loads, and values and error text materialize via `new String(chars, ...)`. Both
    * input forms fill it: a String copies its chars, a UTF-8 byte array widens (see
    * [[resetBytes]]). Inputs up to [[Tokenizer.MaxPooledInputChars]] refill the buffer pooled with
    * the scanner — a copy but no allocation, with retention hard-bounded by the cap; larger inputs
    * take a transient buffer that the next pooled use discards. The buffer may be longer than the
    * input: every scan limit must come from [[inputLength]], never from `chars.length`.
    */
  private[internal] var chars: Array[Char] = initialInput.toCharArray

  /** the number of live chars in [[chars]] */
  private[internal] var inputLength: Int = initialInput.length

  /** Sizes [[chars]] for `needed` live chars under the pooling policy. */
  private def prepareBuffer(needed: Int): Unit =
    if needed <= MaxPooledInputChars then
      if chars.length < needed || chars.length > MaxPooledInputChars then
        var capacity = MinPooledInputChars
        while capacity < needed do capacity *= 2
        chars = new Array[Char](capacity)
    else if chars.length < needed then chars = new Array[Char](needed)

  /** Repositions this scanner at the start of a new input, for reuse from a pool. */
  private[internal] def reset(
      newInput: String,
      newExperimentalFlags: Int = ExperimentalFlags.None
  ): Unit =
    val newLength = newInput.length
    prepareBuffer(newLength)
    newInput.getChars(0, newLength, chars, 0)
    inputLength = newLength
    index = 0
    experimentalFlags = newExperimentalFlags
    kind = TokenKind.Eof
    str = null

  /** Repositions this scanner at the start of a UTF-8 byte input. ASCII bytes widen directly into
    * the char buffer (widening is not UTF-8 decoding — each non-negative byte is its char); the
    * first non-ASCII byte delegates the whole input to the JDK's UTF-8 decoder through one
    * transient String. Identifier and name inspection stay allocation-free either way.
    */
  private[internal] def resetBytes(
      newInput: Array[Byte],
      newExperimentalFlags: Int = ExperimentalFlags.None
  ): Unit =
    val byteLength = newInput.length
    prepareBuffer(byteLength)
    val text  = chars
    var i     = 0
    var ascii = true
    while ascii && i < byteLength do
      val b = newInput(i)
      if b >= 0 then
        text(i) = b.toChar
        i += 1
      else ascii = false
    if ascii then inputLength = byteLength
    else
      val decoded       = new String(newInput, java.nio.charset.StandardCharsets.UTF_8)
      val decodedLength = decoded.length
      prepareBuffer(decodedLength)
      decoded.getChars(0, decodedLength, chars, 0)
      inputLength = decodedLength
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
      DecodeError.TokenFormat(e.message).atToken(spanAt(chars, inputLength, e.offset))
    }):
      val tokens = List.newBuilder[Token]
      var done   = false
      while !done do
        scanNext()
        val token = materialize(this)
        tokens += token
        if debug then Console.err.println(token)
        done = kind == TokenKind.Eof
      tokens.result()

  // Slots describing the most recently scanned token — unboxed in the happy path. The slots live
  // in a [[TokenSlots]] object so [[TokenStream]] can read the scanner's output in place: the
  // stream's current-token pointer aims here directly, and a copy is only made when lookahead
  // needs to preserve a token. The forwarders below keep the scanning code reading naturally.
  private[internal] val out: TokenSlots = new TokenSlots

  private def kind: Int                         = out.kind
  private def kind_=(value: Int): Unit          = out.kind = value
  private def start: Int                        = out.start
  private def start_=(value: Int): Unit         = out.start = value
  private def end: Int                          = out.end
  private def end_=(value: Int): Unit           = out.end = value
  private def str: String | Null                = out.str
  private def str_=(value: String | Null): Unit = out.str = value
  private def num: Long                         = out.num
  private def num_=(value: Long): Unit          = out.num = value
  private def dbl: Double                       = out.dbl
  private def dbl_=(value: Double): Unit        = out.dbl = value

  private def sliceEquals(from: Int, until: Int, expected: String): Boolean =
    // keywords are all short, so a plain char loop beats regionMatches' vectorized setup
    val len = until - from
    if len != expected.length then false
    else
      var i = 0
      while i < len do
        if chars(from + i) != expected.charAt(i) then return false
        i += 1
      true

  /** Scan the next token into the slot fields. Throws [[TokenizeException]] on malformed input. */
  def scanNext(): Unit =
    skipTrivia()
    start = index
    str = null
    if isAtEnd then kind = TokenKind.Eof
    else scanToken()
    end = index

  // --- char-level scans for the schema-directed decode path -------------------------------------
  // Each consumes exactly the asked-for shape or nothing (returning false/0 with the position
  // untouched), so a fallback token scan over the same characters sees exactly what it would have
  // seen without the attempt.

  /** start offset of the last successful char-level scan below — for error spans */
  private[internal] var charScanStart: Int = 0

  /** Scans one plain identifier as an offset slice into `out.start`/`out.end`, without keyword
    * classification and without touching `out.kind`. False when the next char cannot start a plain
    * identifier (quoted names, operators, digits, EOF) — the caller falls back to a token scan. The
    * consumed chars are exactly what [[scanIdentifier]] would consume, including the trailing
    * '_'-then-operator continuation rule.
    */
  private[internal] def scanIdentSlice(): Boolean =
    // probe-first: an identifier start is never a trivia char, so a direct hit at the cursor
    // needs no whitespace machinery at all; a miss backs out to the general trivia reader
    var from  = index
    var ident = identifier(from)
    if ident < 0 then
      skipTrivia()
      from = index
      ident = identifier(from)
    if ident < 0 then false
    else
      start = from
      end = ident
      charScanStart = from
      index = ident
      true

  /** Fused scan of a `<expected> =` field header: 1 with both consumed, 0 with only the identifier
    * consumed as a slice (a mismatched name or a missing `=` — the caller resolves via the slice),
    * -1 with nothing consumed (no plain identifier follows).
    *
    * The expected name is matched directly against the input in one pass; `expected` must be
    * plan-eligible ([[Tokenizer.isPlainFieldName]]), which also rules out '_'-ending names, so the
    * boundary check below is exactly the identifier end the generic scanner would find.
    */
  private[internal] def scanFieldHeader(expected: Array[Char]): Int =
    // The hottest scan in record decoding — the combinator chain inlines to one pass over
    // register-resident text/index with a single write-back. Unusual whitespace (comments,
    // unicode spaces) diverts to the general reader path.
    val text  = chars
    val limit = inputLength
    var i     = index
    // probe-first name match: the name's first char is never a trivia char, so a direct hit
    // needs no whitespace walk; only a miss walks the gap
    if !(i < limit && text(i) == expected(0)) then i = plainGap(i)
    val len = expected.length
    var j   = 0
    while j < len && i < limit && text(i) == expected(j) do
      i += 1
      j += 1
    if j == len then
      // the identifier boundary is IMPLIED by what follows the name: `=` directly (compact) or
      // one space then `=` (spaced) both end it and complete the header in the same pass;
      // anything else routes to the boundary check and the retry/mismatch arms
      var eq = i
      if eq < limit && text(eq) == ' ' then eq += 1
      if eq < limit && text(eq) == '='
        && (eq + 1 >= limit || !operatorPart(text(eq + 1)))
      then
        val slots = out
        slots.start = i - len
        slots.end = i
        index = eq + 1
        HeaderMatched
      else if i >= limit || identifierEndsAt(text(i)) then
        val slots = out
        slots.start = i - len
        slots.end = i
        headerEqualsRetry(i)    // wider gaps or comments before '='
      else headerNameMismatch() // the identifier continues: a longer name
    else headerNameMismatch()

  /** '=' not found directly after a matched name: comments or unusual whitespace may hide it — the
    * general char reader retries (walking any trivia from the name's end) before the caller falls
    * back to the token path. Outlined so [[scanFieldHeader]]'s hot path stays within the JIT's
    * inlining budgets.
    */
  private def headerEqualsRetry(name: Int): Int =
    index = name
    if scanEqualsChar() then HeaderMatched else HeaderNameSlice

  /** A different (or non-plain) name, or unusual leading trivia: rescan generically as a slice for
    * the caller's resolution — outlined like [[headerEqualsRetry]].
    */
  private def headerNameMismatch(): Int =
    index = plainTrivia(index)
    if scanIdentSlice() then HeaderNameSlice else HeaderNone

  /** Consumes a single `=` that is a whole operator (rejecting `==`, `=>`, ...): false consumes
    * nothing.
    */
  private[internal] def scanEqualsChar(): Boolean =
    if peek(index) == ' ' then index += 1 // a single space — the idiomatic spaced form
    if peek(index) != '=' then skipTrivia()
    val from = index
    val eq   = wholeOperator(from, '=')
    if eq >= 0 then
      charScanStart = from
      index = eq
      true
    else false

  /** Consumes an element separator: 1 for `,`, 2 for `closing`, 0 (nothing consumed) otherwise. */
  private[internal] def scanSeparatorChar(closing: Char): Int =
    var ch = peek(index)
    if ch != ',' && ch != closing then
      skipTrivia()
      ch = peek(index)
    val from = index
    if ch == ',' then
      charScanStart = from
      index = from + 1
      // a trailing comma may directly precede the closing char: consuming both here saves the
      // caller a separate probe after every element
      if peek(index) != closing then skipTrivia()
      val at = index
      if peek(at) == closing then
        charScanStart = at
        index = at + 1
        SeparatorTrailingClosing
      else SeparatorComma
    else if ch == closing then
      charScanStart = from
      index = from + 1
      SeparatorClosing
    else SeparatorNone

  /** Scanner-direct scan of an optionally negated numeric literal in one trivia pass — the
    * [[Tokenizer.NumberNone]]/[[Tokenizer.NumberPositive]]/[[Tokenizer.NumberNegated]] codes. The
    * token path treats the sign as its own Minus token with identical interpretation, so only the
    * round trip is skipped.
    */
  private[internal] def scanSignedNumberValue(): Int =
    if peek(index) == ' ' then index += 1
    val ch0 = peek(index)
    if ch0 != '-' && (ch0 < '0' || ch0 > '9') then skipTrivia()
    val from = index
    val ch   = peek(from)
    if ch >= '0' && ch <= '9' then
      start = from
      str = null
      scanNumber()
      end = index
      NumberPositive
    else if ch == '-' && { val d = peek(from + 1); d >= '0' && d <= '9' } then
      // only the sign commits here; scanNumber reads the digits (and everything after them)
      index = from + 1
      start = from + 1
      str = null
      scanNumber()
      end = index
      NumberNegated
    else NumberNone

  /** Scanner-direct scan of a string literal at a value position — see [[scanSignedNumberValue]].
    * Dedented multi-quote strings stay on the token path.
    */
  private[internal] def scanStringValue(): Boolean =
    if peek(index) == ' ' then index += 1
    if peek(index) != '"' then skipTrivia()
    val from = index
    if peek(from) == '"' then
      start = from
      str = null
      scanString()
      end = index
      true
    else false

  /** Scans an escape-free, non-concatenated `"` string literal's CONTENT as a slice: true with the
    * literal consumed and the content bounds in `out.start`/`out.end` — nothing materializes, so
    * callers compare the content like an identifier in quotes. Any discrepancy — not a `"` literal,
    * an escape, an unterminated literal, or a following `+` (concatenation) — restores the position
    * and returns false, so the general string decode sees the identical characters.
    */
  private[internal] def scanStringContentSlice(): Boolean =
    if peek(index) == ' ' then index += 1
    if peek(index) != '"' then skipTrivia()
    val literal = index
    val closed  = stringSlice(literal) // no `"` literal, an escape or the end of input fail here
    if closed < 0 then false
    else if concatenationMayFollow(closed) then false
    else
      out.start = literal + 1
      out.end = closed - 1
      charScanStart = literal
      index = closed
      true

  /** Whether a `+` (concatenation) may follow the closing quote at `closed` — only the general
    * decode handles concatenation. The probe walks plain trivia only, so anything that could HIDE a
    * `+` (a comment, unicode whitespace, separator controls — skipTrivia's divert set) also answers
    * true. Outlined so [[scanStringContentSlice]] stays within the JIT's inlining budgets.
    */
  private def concatenationMayFollow(closed: Int): Boolean =
    val probe = plainTrivia(closed)
    val next  = peek(probe)
    (next == '+' && wholeOperator(probe, '+') >= 0)
    || next == '/' || next > IdentifierSyntax.MaxAscii
    || (next >= MinSeparatorControl && next <= MaxSeparatorControl)

  /** Fused scan of a `true`/`false` literal at a value position: 1/0 with it consumed, -1 with
    * nothing consumed. The boundary check mirrors [[scanIdentifier]], so any other identifier shape
    * (`trueish`, `true_`) is left for the token path, which classifies it identically.
    */
  private[internal] def scanBooleanValue(): Int =
    val ch0 = peek(index)
    if ch0 != 't' && ch0 != 'f' then skipTrivia()
    val from = index
    // dispatch on the first char: a false value must not pay a failed `true` attempt
    if peek(from) == 't' then
      val isTrue = keyword["true"](from)
      if isTrue >= 0 then
        index = isTrue
        BooleanTrue
      else BooleanNone
    else scanFalseValue(from)

  /** the `false` half of [[scanBooleanValue]] — split so each half sits within the JIT's inlining
    * budgets
    */
  private def scanFalseValue(from: Int): Int =
    val isFalse = keyword["false"](from)
    if isFalse >= 0 then
      index = isFalse
      BooleanFalse
    else BooleanNone

  /** Whether the next char begins a `+` operator (string concatenation) — consumes only trivia. */
  private[internal] def peekPlusChar(): Boolean =
    if peek(index) != '+' then skipTrivia()
    wholeOperator(index, '+') >= 0

  /** Consumes a single expected punctuation char: false consumes nothing. */
  private[internal] def scanPunctChar(expected: Char): Boolean =
    if peek(index) == ' ' then index += 1
    if peek(index) != expected then skipTrivia()
    val from = index
    if peek(from) == expected then
      charScanStart = from
      index = from + 1
      true
    else false

  /** Rewinds to `offset` so the token path can rescan a char-level attempt — only valid while the
    * owning stream is between tokens.
    */
  private[internal] def rewindTo(offset: Int): Unit =
    index = offset

  private def scanToken(): Unit =
    currentChar() match
      case '('                            => advance(); kind = TokenKind.LParen
      case ')'                            => advance(); kind = TokenKind.RParen
      case '.'                            => advance(); kind = TokenKind.Dot
      case ','                            => advance(); kind = TokenKind.Comma
      case ';'                            => advance(); kind = TokenKind.Semicolon
      case '['                            => advance(); kind = TokenKind.LBracket
      case ']'                            => advance(); kind = TokenKind.RBracket
      case '{'                            => advance(); kind = TokenKind.LBrace
      case '}'                            => advance(); kind = TokenKind.RBrace
      case '`'                            => scanQuotedIdentifier()
      case '"'                            => scanString()
      case '\'' if canStartDedentedString => scanDedentedString()
      case '\''                           => scanChar()
      case ch if isIdentifierStart(ch)    => scanIdentifier()
      case ch if ch.isDigit               => scanNumber()
      case '-' if canScanPairArrow        => advance(); advance(); kind = TokenKind.Arrow
      case ch if isOperatorPart(ch)       => scanOperator()
      case ch                             => fail(s"Unexpected character '$ch'")

  /** the raw source text of the token scanned so far — used in error messages only */
  private def rawText: String = new String(chars, start, index - start)

  private def scanIdentifier(): Unit =
    // the identifier is a slice of the input: track offsets, no per-token builder (the
    // dispatcher guarantees an identifier start char, so the scan cannot fail)
    index = identifier(index)
    kind = classifyIdentifier()

  /** Classifies the identifier slice against the special and reserved keywords, dispatching on the
    * first character: every identifier — one per field name — is compared against at most six
    * length-gated candidates instead of scanning the whole keyword table.
    */
  private def classifyIdentifier(): Int =
    inline def kw(inline expected: String): Boolean = sliceEquals(start, index, expected)
    chars(start) match
      case 'a' =>
        if kw("abstract") then TokenKind.Keyword else TokenKind.Identifier
      case 'c' =>
        if kw("case") || kw("catch") || kw("class") then TokenKind.Keyword
        else TokenKind.Identifier
      case 'd' =>
        if kw("def") || kw("do") then TokenKind.Keyword else TokenKind.Identifier
      case 'e' =>
        if kw("else") || kw("enum") || kw("export") || kw("extends") then TokenKind.Keyword
        else TokenKind.Identifier
      case 'f' =>
        if kw(KW_false) then TokenKind.FalseKw
        else if kw("final") || kw("finally") || kw("for") then TokenKind.Keyword
        else TokenKind.Identifier
      case 'g' =>
        if kw("given") then TokenKind.Keyword else TokenKind.Identifier
      case 'i' =>
        if kw("if") || kw("implicit") || kw("import") then TokenKind.Keyword
        else TokenKind.Identifier
      case 'l' =>
        if kw("lazy") then TokenKind.Keyword else TokenKind.Identifier
      case 'm' =>
        if kw("match") then TokenKind.Keyword else TokenKind.Identifier
      case 'n' =>
        if kw(KW_null) then TokenKind.NullKw
        else if kw("new") then TokenKind.Keyword
        else TokenKind.Identifier
      case 'o' =>
        if kw("object") || kw("override") then TokenKind.Keyword else TokenKind.Identifier
      case 'p' =>
        if kw(KW_package) then TokenKind.PackageKw
        else if kw("private") || kw("protected") then TokenKind.Keyword
        else TokenKind.Identifier
      case 'r' =>
        if kw("return") then TokenKind.Keyword else TokenKind.Identifier
      case 's' =>
        if kw("sealed") || kw("super") then TokenKind.Keyword else TokenKind.Identifier
      case 't' =>
        if kw(KW_true) then TokenKind.TrueKw
        else if kw("then") || kw("throw") || kw("trait") || kw("try") || kw("type") then
          TokenKind.Keyword
        else TokenKind.Identifier
      case 'v' =>
        if kw(KW_val) then TokenKind.ValKw
        else if kw("var") then TokenKind.Keyword
        else TokenKind.Identifier
      case 'w' =>
        if kw("while") || kw("with") then TokenKind.Keyword else TokenKind.Identifier
      case 'y' =>
        if kw("yield") then TokenKind.Keyword else TokenKind.Identifier
      case 'V' =>
        if kw(KW_Vector) then TokenKind.VectorId else TokenKind.Identifier
      case 'E' =>
        if kw(KW_EmptyTuple) then TokenKind.EmptyTupleId else TokenKind.Identifier
      case 'T' =>
        if kw(KW_Tuple) then TokenKind.TupleId else TokenKind.Identifier
      case _ =>
        TokenKind.Identifier

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
    index = takeWhile(index)(ch => isOperatorPart(ch))
    val len = index - start
    // the fixed punctuation operators are classified straight from the slice — `=` especially is
    // scanned once per named-tuple field, so it must never allocate
    if len == 1 then
      chars(start) match
        case '=' => kind = TokenKind.Equals
        case '+' => kind = TokenKind.Plus
        case '-' => kind = TokenKind.Minus
        case _   => classifyOperatorIdentifier()
    else if len == 2 && sliceEquals(start, index, KW_pairArrow) then kind = TokenKind.Arrow
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
    out.accDigits = AccumulatorInvalid // prefixed literals always interpret via the slow path

  private def scanDecimalNumber(): Unit =
    // Three [[digitRun]] walks — integer, fraction, exponent — with the accumulation living in
    // this method's locals through the run's inline callbacks. The integer-part walk accumulates
    // the negated base-10 value alongside shape validation, so plain Int/Long literals need no
    // second pass at consumption. Separators, unicode digits (accepted by the shape, interpreted
    // slowly), or more than 18 digits invalidate the accumulator.
    var hasDot       = false
    var hasExponent  = false
    var acc          = 0L
    var accDigits    = 0
    var sawSeparator = false
    // Exact-double eligibility: the walks accumulate the (negated) mantissa across integer and
    // fraction digits plus the decimal exponent. When the mantissa fits 2^53 and |exp10| <= 22,
    // both operands of a single multiply/divide by a power of ten are exactly representable, so
    // one IEEE rounding produces the same result parseDouble would — everything else (separators,
    // unicode digits, > 18 digits, big exponents) falls back to parseDouble.
    var dblOk = true
    var exp10 = 0

    val text   = chars
    val length = inputLength

    var i = digitRun(index) { ch =>
      if accDigits != AccumulatorInvalid && accDigits < MaxAccumulatedDigits then
        acc = acc * 10 - (ch - '0')
        accDigits += 1
      else
        accDigits = AccumulatorInvalid
        dblOk = false
    } { isSeparator =>
      if isSeparator then sawSeparator = true
      accDigits = AccumulatorInvalid
      dblOk = false
    }

    if i + 1 < length && text(i) == '.' && {
        val d = text(i + 1)
        (d >= '0' && d <= '9') || (d > IdentifierSyntax.MaxAscii && Character.isDigit(d))
      }
    then
      hasDot = true
      i = digitRun(i + 1) { ch =>
        if accDigits != AccumulatorInvalid && accDigits < MaxAccumulatedDigits then
          acc = acc * 10 - (ch - '0')
          accDigits += 1
          exp10 -= 1
        else dblOk = false
      } { isSeparator =>
        if isSeparator then sawSeparator = true
        dblOk = false
      }

    if i < length && { val ch = text(i); ch == 'e' || ch == 'E' } then
      hasExponent = true
      i += 1
      var expNegative = false
      if i < length && { val ch = text(i); ch == '+' || ch == '-' } then
        expNegative = text(i) == '-'
        i += 1
      index = i
      if i >= length || ! {
          val ch = text(i)
          (ch >= '0' && ch <= '9') || (ch > IdentifierSyntax.MaxAscii && Character.isDigit(ch))
        }
      then fail("Exponent requires at least one digit")
      var explicitExp = 0
      i = digitRun(i) { ch =>
        if explicitExp < ExponentSaturation then explicitExp = explicitExp * 10 + (ch - '0')
      } { isSeparator =>
        if isSeparator then sawSeparator = true
        dblOk = false
      }
      exp10 += (if expNegative then -explicitExp else explicitExp)

    index = i
    val digitsEnd = index
    val mantissa  = -acc
    val dblExact  =
      dblOk && accDigits >= 1 && mantissa <= ExactMantissaLimit
        && exp10 >= -MaxExactPow10 && exp10 <= MaxExactPow10
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
      val digits = new String(chars, start, digitsEnd - start)
      if sawSeparator then digits.replace("_", "") else digits

    suffix match
      case 'l' | 'L' =>
        if hasDot || hasExponent then
          fail("Long literals cannot contain a decimal point or exponent")
        kind = TokenKind.LongLit
        num = acc
        out.accDigits = accDigits
      case 'f' | 'F' =>
        kind = TokenKind.FloatLit
        dbl = parseFloatLiteral(normalizedDigits(sawSeparator)).toDouble
      case 'd' | 'D' =>
        kind = TokenKind.DoubleLit
        dbl =
          if dblExact then
            if exp10 >= 0 then mantissa.toDouble * exactPow10(exp10)
            else mantissa.toDouble / exactPow10(-exp10)
          else parseDoubleLiteral(normalizedDigits(sawSeparator))
      case _ if hasDot || hasExponent =>
        kind = TokenKind.DoubleLit
        dbl =
          if dblExact then
            if exp10 >= 0 then mantissa.toDouble * exactPow10(exp10)
            else mantissa.toDouble / exactPow10(-exp10)
          else parseDoubleLiteral(normalizedDigits(sawSeparator))
      case _ =>
        kind = TokenKind.IntLit
        num = acc
        out.accDigits = accDigits

  private def scanString(): Unit =
    advance()
    // fast path: an escape-free string is a slice of the input, no builder needed
    val contentStart = index
    index = takeWhile(index)(ch => ch != '"' && ch != '\\')
    if isAtEnd then fail("Unterminated string literal")
    if currentChar() == '"' then
      str = new String(chars, contentStart, index - contentStart)
      advance()
      kind = TokenKind.StringLit
    else
      val value = new StringBuilder(new String(chars, contentStart, index - contentStart))
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
    var contentEnd     = -1
    var sawCR          = false

    var done = false
    while !done do
      if isAtEnd then failAt("unclosed multi-line string literal", delimiterStart)
      else if currentChar() == '\'' then
        val quoteStart = index
        val runLength  = quoteRunLength()
        if runLength >= delimiterCount then
          contentEnd = quoteStart + (runLength - delimiterCount)
          index = quoteStart + runLength
          done = true
        else index = quoteStart + runLength
      else
        if currentChar() == '\r' then sawCR = true
        index += 1

    kind = TokenKind.StringLit
    str = trimDedentedString(contentStart, contentEnd, sawCR)

  private def consumeQuoteRun(): Int =
    var count = 0
    while !isAtEnd && currentChar() == '\'' do
      advance()
      count += 1
    count

  private def quoteRunLength(): Int =
    var i = 0
    while index + i < inputLength && chars(index + i) == '\'' do i += 1
    i

  private def trimDedentedString(
      contentStart: Int,
      contentEnd: Int,
      sawCR: Boolean
  ): String =
    var firstLineEnd = contentStart
    while firstLineEnd < contentEnd && isIndentWhitespace(chars(firstLineEnd)) do firstLineEnd += 1
    if firstLineEnd >= contentEnd || !isLineBreak(chars(firstLineEnd)) then
      failAt(
        "Dedented string literal must start with newline after opening quotes",
        firstLineEnd
      )
    val bodyStart = afterLineBreak(firstLineEnd)

    var closingLineBreak = contentEnd - 1
    while closingLineBreak >= contentStart && isIndentWhitespace(chars(closingLineBreak)) do
      closingLineBreak -= 1
    if closingLineBreak < contentStart || !isLineBreak(chars(closingLineBreak)) then
      failAt(
        "Last line of dedented string literal must contain only whitespace before closing delimiter",
        math.max(closingLineBreak, contentStart)
      )
    if chars(closingLineBreak) == '\n'
      && closingLineBreak > contentStart
      && chars(closingLineBreak - 1) == '\r'
    then closingLineBreak -= 1

    val closingIndentStart = afterLineBreak(closingLineBreak)
    val bodyEnd            = closingLineBreak
    if bodyEnd <= bodyStart then ""
    else dedentLines(bodyStart, bodyEnd, closingIndentStart, contentEnd, sawCR)

  private def dedentLines(
      bodyStart: Int,
      bodyEnd: Int,
      closingIndentStart: Int,
      closingIndentEnd: Int,
      sawCR: Boolean
  ): String =
    if closingIndentStart == closingIndentEnd then
      if sawCR then normalizedSubstring(bodyStart, bodyEnd)
      else new String(chars, bodyStart, bodyEnd - bodyStart)
    else
      val out           = new StringBuilder
      val closingIndent = closingIndentEnd - closingIndentStart
      var lineStart     = bodyStart
      while lineStart < bodyEnd do
        var lineEnd = lineStart
        while lineEnd < bodyEnd && !isLineBreak(chars(lineEnd)) do lineEnd += 1

        val cut =
          if lineHasPrefix(lineStart, lineEnd, closingIndentStart, closingIndentEnd) then
            closingIndent
          else if isIndentOnlyLine(lineStart, lineEnd) then 0
          else
            failAt(
              "Line in dedented string literal must be indented at least as much as the closing delimiter",
              lineStart
            )
        out.appendAll(chars, lineStart + cut, lineEnd - (lineStart + cut))
        if lineEnd < bodyEnd then
          out.append('\n')
          lineStart = afterLineBreak(lineEnd)
        else lineStart = bodyEnd
      out.result()

  private def lineHasPrefix(
      lineStart: Int,
      lineEnd: Int,
      prefixStart: Int,
      prefixEnd: Int
  ): Boolean =
    val prefixLength = prefixEnd - prefixStart
    lineEnd - lineStart >= prefixLength
    && {
      var i  = 0
      var ok = true
      while ok && i < prefixLength do
        if chars(lineStart + i) != chars(prefixStart + i) then ok = false
        else i += 1
      ok
    }

  private def isIndentOnlyLine(lineStart: Int, lineEnd: Int): Boolean =
    var i = lineStart
    while i < lineEnd do
      if !isIndentWhitespace(chars(i)) then return false
      i += 1
    true

  private def normalizedSubstring(from: Int, until: Int): String =
    var i = from
    while i < until && chars(i) != '\r' do i += 1
    if i >= until then new String(chars, from, until - from)
    else
      val out = new StringBuilder(until - from)
      out.appendAll(chars, from, i - from)
      while i < until do
        chars(i) match
          case '\r' =>
            out.append('\n')
            i += 1
            if i < until && chars(i) == '\n' then i += 1
          case ch =>
            out.append(ch)
            i += 1
      out.result()

  private def isIndentWhitespace(ch: Char): Boolean =
    ch == ' ' || ch == '\t' || ch == '\f'

  private def isLineBreak(ch: Char): Boolean =
    ch == '\n' || ch == '\r'

  private def afterLineBreak(offset: Int): Int =
    if chars(offset) == '\r' && offset + 1 < inputLength && chars(offset + 1) == '\n'
    then offset + 2
    else offset + 1

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
    // Gaps between tokens are almost always zero or one ' ': both exits are reached with at most
    // two char loads and no loop. Everything else diverts to the general walk.
    val text   = chars
    val length = inputLength
    var i      = index
    if i < length then
      val ch0 = text(i)
      if ch0 == ' ' then
        i += 1
        if i < length then
          val ch1 = text(i)
          if ch1 == ' ' || (ch1 >= '\t' && ch1 <= '\r') || ch1 == '/'
            || ch1 > IdentifierSyntax.MaxAscii
            || (ch1 >= MinSeparatorControl && ch1 <= MaxSeparatorControl)
          then
            index = i
            skipTriviaWalk()
          else index = i
        else index = i
      else if (ch0 >= '\t' && ch0 <= '\r') || ch0 == '/' || ch0 > IdentifierSyntax.MaxAscii
        || (ch0 >= MinSeparatorControl && ch0 <= MaxSeparatorControl)
      then skipTriviaWalk()

  private def skipTriviaWalk(): Unit =
    val text   = chars
    val length = inputLength
    var i      = index
    while i < length do
      val ch = text(i)
      if ch == ' ' || (ch >= '\t' && ch <= '\r') then i += 1
      else if ch == '/' || ch > IdentifierSyntax.MaxAscii
        || (ch >= MinSeparatorControl && ch <= MaxSeparatorControl)
      then
        index = i
        skipTriviaSlow()
        return
      else
        index = i
        return
    index = i

  private def skipTriviaSlow(): Unit =
    var keepGoing = true
    while keepGoing && !isAtEnd do
      skipWhitespace()
      if isAtEnd then keepGoing = false
      else if currentChar() == '/' && peekCompare('/') then skipLineComment()
      else if currentChar() == '/' && peekCompare('*') then skipBlockComment()
      else keepGoing = false

  private def skipWhitespace(): Unit =
    // ' ' and '\t'..'\r' cover virtually all whitespace in real input; Character.isWhitespace
    // (which also accepts the unicode spaces and the file separators) only runs for the leftovers
    while !isAtEnd do
      val ch = currentChar()
      if ch == ' ' || (ch >= '\t' && ch <= '\r') then index += 1
      else if ch > IdentifierSyntax.MaxAscii
        || (ch >= MinSeparatorControl && ch <= MaxSeparatorControl)
      then
        if ch.isWhitespace then index += 1
        else return
      else return

  private def skipLineComment(): Unit =
    index = takeWhile(index + 2)(ch => ch != '\n')

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

  private def isAtEnd: Boolean = index >= inputLength

  private def currentChar(): Char = chars(index)

  private def peekCompare(expected: Char, offset: Int = 1): Boolean =
    val nextIndex = index + offset
    nextIndex < inputLength && chars(nextIndex) == expected

  private def peekIsDigit(offset: Int = 1): Boolean =
    val nextIndex = index + offset
    nextIndex < inputLength && chars(nextIndex).isDigit

  private def canScanPairArrow: Boolean =
    peekCompare('>') && {
      val afterArrow = index + 2
      afterArrow >= inputLength || !isOperatorPart(chars(afterArrow))
    }

  private def advance(): Char =
    val ch = chars(index)
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
  /** Base-10 digit counts up to this always fit a Long magnitude, so the scanner can accumulate
    * literal values during shape validation without overflow checks.
    */
  private[internal] inline val MaxAccumulatedDigits = 18

  /** [[TokenSlots.accDigits]] value marking the accumulator unusable (separators, prefixes, unicode
    * digits, or more than [[MaxAccumulatedDigits]] digits) — consumers interpret the literal slice
    * through the exact slow path instead.
    */
  private[internal] inline val AccumulatorInvalid = -1

  /** an Int literal has at most this many base-10 digits (Int.MaxValue = 2147483647) */
  private[internal] inline val MaxIntDigits = 10

  /** Powers of ten up to this exponent are exactly representable as Doubles, so a mantissa within
    * [[ExactMantissaLimit]] converts with a single correctly-rounded multiply or divide.
    */
  private[internal] inline val MaxExactPow10 = 22

  /** doubles represent every integer exactly up to 2^53 */
  private[internal] inline val ExactMantissaLimit = 1L << 53

  /** Explicit exponents stop accumulating here: far beyond [[MaxExactPow10]] (so eligibility
    * decisions are unaffected) and far below Int overflow when digits keep arriving.
    */
  private[internal] inline val ExponentSaturation = 10000

  /** the FS/GS/RS/US separator controls — the only chars below ' ' that might still be whitespace
    */
  /** Inputs up to this many chars refill the scanner-pooled char buffer (retention per pooled
    * scanner is hard-bounded at two bytes per char of this cap); larger inputs copy transiently. A
    * power of two, so the buffer's doubling growth never exceeds it.
    */
  private[internal] inline val MaxPooledInputChars = 8192

  /** the pooled char buffer's smallest capacity — a power of two like the cap */
  private[internal] inline val MinPooledInputChars = 16

  // ---- kernel op result codes ----

  /** [[Tokenizer.scanFieldHeader]]: the expected name and its `=` were both consumed */
  private[scalanotation] inline val HeaderMatched = 1

  /** [[Tokenizer.scanFieldHeader]]: a name slice was consumed — the caller resolves it */
  private[scalanotation] inline val HeaderNameSlice = 0

  /** [[Tokenizer.scanFieldHeader]]: nothing consumed — the token path takes over */
  private[scalanotation] inline val HeaderNone = -1

  /** [[Tokenizer.scanSeparatorChar]]: nothing consumed (a pending token or another shape) */
  private[scalanotation] inline val SeparatorNone = 0

  /** [[Tokenizer.scanSeparatorChar]]: a `,` was consumed — another element follows */
  private[scalanotation] inline val SeparatorComma = 1

  /** [[Tokenizer.scanSeparatorChar]]: the closing char was consumed */
  private[scalanotation] inline val SeparatorClosing = 2

  /** [[Tokenizer.scanSeparatorChar]]: a trailing `,` and the closing char were both consumed */
  private[scalanotation] inline val SeparatorTrailingClosing = 3

  /** [[Tokenizer.scanSignedNumberValue]]: nothing consumed */
  private[scalanotation] inline val NumberNone = 0

  /** [[Tokenizer.scanSignedNumberValue]]: a positive literal was scanned */
  private[scalanotation] inline val NumberPositive = 1

  /** [[Tokenizer.scanSignedNumberValue]]: a `-` sign and its literal were scanned */
  private[scalanotation] inline val NumberNegated = 2

  /** [[Tokenizer.scanBooleanValue]]: `true` was consumed */
  private[scalanotation] inline val BooleanTrue = 1

  /** [[Tokenizer.scanBooleanValue]]: `false` was consumed */
  private[scalanotation] inline val BooleanFalse = 0

  /** [[Tokenizer.scanBooleanValue]]: nothing consumed — the token path takes over */
  private[scalanotation] inline val BooleanNone = -1

  private[internal] inline val MinSeparatorControl = 28
  private[internal] inline val MaxSeparatorControl = 31

  /** 10^0 .. 10^[[MaxExactPow10]] — every entry is exactly representable as a Double */
  private[internal] val exactPow10: Array[Double] =
    val table = new Array[Double](MaxExactPow10 + 1)
    var i     = 0
    var value = 1.0
    while i < table.length do
      table(i) = value
      value *= 10.0
      i += 1
    table

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
  private val KW_pairArrow  = "->"
  private val KW_arrow      = "=>"
  private val KW_subtype    = "<:"
  private val KW_supertype  = ">:"
  private val KW_hash       = "#"
  private val KW_at         = "@"
  private val KW_tlArrow    = "=>>"
  private val KW_ctxArrow   = "?=>"

  /** Interprets an `IntLit` token's value in place from its input slice. The scanner validates only
    * the shape: the sign lives in a separate token, so the range check has to happen here, at
    * consumption, where the sign is known — otherwise `Int.MinValue` would be rejected, since its
    * magnitude overflows a positive Int.
    */
  private[internal] def intValueAt(
      chars: Array[Char],
      start: Int,
      end: Int,
      negative: Boolean
  ): Int =
    val limit   = if negative then Int.MinValue.toLong else -Int.MaxValue.toLong
    val negated = negatedValueAt(chars, start, end, limit, "Int")
    (if negative then negated else -negated).toInt

  /** interprets a `LongLit` token's value in place from its input slice — see [[intValueAt]] */
  private[internal] def longValueAt(
      chars: Array[Char],
      start: Int,
      end: Int,
      negative: Boolean
  ): Long =
    val limit   = if negative then Long.MinValue else -Long.MaxValue
    val negated = negatedValueAt(chars, start, end, limit, "Long")
    if negative then negated else -negated

  /** Accumulates the literal's magnitude negatively (mirroring `java.lang.Long.parseLong`), so
    * `MinValue`'s magnitude — one more than `MaxValue`'s — stays representable. Skips '_'
    * separators, the `0x`/`0b` prefix and the `l`/`L` suffix; no intermediate string is allocated.
    * The scanner has already validated every char in the slice.
    */
  private def negatedValueAt(
      chars: Array[Char],
      start: Int,
      end: Int,
      limit: Long,
      name: String
  ): Long =
    def invalid(): Nothing =
      throw TokenizeException(
        s"Invalid $name literal '${new String(chars, start, end - start)}'",
        start
      )

    var from = start
    var base = 10
    if end - start > 2 && chars(start) == '0' then
      chars(start + 1) match
        case 'x' | 'X' => base = 16; from = start + 2
        case 'b' | 'B' => base = 2; from = start + 2
        case _         => ()

    val multmin = limit / base
    var result  = 0L
    var i       = from
    while i < end do
      val ch = chars(i)
      if ch != '_' && ch != 'l' && ch != 'L' then
        val digit = Character.digit(ch, base)
        if digit < 0 || digit >= base || result < multmin then invalid()
        result = result * base
        if result < limit + digit then invalid()
        result = result - digit
      i += 1
    result

  /** Whether `name` scans as a single plain identifier-like token that the token path's field-name
    * matcher accepts by slice comparison, so a char-level slice match on `name` is exactly
    * equivalent to the token path. Decided by running the real scanner over the name once (cold:
    * cached per schema). Keyword-classified names (`true`, `val`, ...), quoted names, operator
    * names (`+`, `-`) and names ending in '_' (operator continuation) are excluded.
    */
  private[scalanotation] def isPlainFieldName(name: String): Boolean =
    if name.isEmpty || name.charAt(name.length - 1) == '_' then false
    else
      try
        val scanner = Tokenizer(name)
        scanner.scanNext()
        val kindOk = scanner.out.kind match
          case TokenKind.Identifier =>
            scanner.out.str == null // quoted identifiers materialize their name
          case TokenKind.VectorId | TokenKind.EmptyTupleId | TokenKind.TupleId => true
          case _                                                               => false
        kindOk && scanner.out.start == 0 && scanner.out.end == name.length
      catch case _: TokenizeException => false

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

  /** [[spanAt]] over a decoded char buffer — error/debug path only. */
  private[internal] def spanAt(
      chars: Array[Char],
      inputLength: Int,
      offset: Int
  ): DecodeError.Span =
    var line   = 1
    var column = 1
    var i      = 0
    val limit  = math.min(offset, inputLength)
    while i < limit do
      if chars(i) == '\n' then
        line += 1
        column = 1
      else column += 1
      i += 1
    DecodeError.Span(offset, line, column)

  /** Materialize the scanner's current slots as a boxed [[Token]] — debug/test path only. */
  private[internal] def materialize(scanner: Tokenizer): Token =
    val span = spanAt(scanner.chars, scanner.inputLength, scanner.start)
    def raw  = new String(scanner.chars, scanner.start, scanner.end - scanner.start)
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
        Token.IntLit(
          raw,
          intValueAt(scanner.chars, scanner.start, scanner.end, negative = false),
          span
        )
      case TokenKind.LongLit =>
        Token.LongLit(
          raw,
          longValueAt(scanner.chars, scanner.start, scanner.end, negative = false),
          span
        )
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
      case TokenKind.LBracket  => Token.LBracket(span)
      case TokenKind.RBracket  => Token.RBracket(span)
      case TokenKind.Arrow     => Token.Arrow(span)
      case TokenKind.LBrace    => Token.LBrace(span)
      case TokenKind.RBrace    => Token.RBrace(span)
      case _                   => Token.Eof(span)

  /** Tokenize the whole input into boxed [[Token]]s — for tests and debugging only; the decode path
    * streams tokens through [[TokenStream]] without materializing them.
    */
  def tokenize(input: String, debug: Boolean): Result[List[Token], DecodeError] =
    Tokenizer(input).tokenize(debug)

/** A bounded buffer over a streaming [[Tokenizer]]: at most three tokens (the current token plus
  * two lookahead tokens) are live at any time, held as unboxed [[TokenSlots]]. The current-token
  * pointer usually aims straight at the scanner's own output slots, so the common no-lookahead path
  * never copies a token; a peek preserves the current token by copying it into one of two buffers
  * first. Boxed [[Token]]s and [[DecodeError.Span]]s are only materialized when constructing a
  * [[DecodeError]] (or for debug output).
  */
private[scalanotation] abstract class TokenStream private[internal] (
    initialInput: String,
    private var debug: Boolean,
    scanOnInit: Boolean
) extends PushSlots {
  def this(input: String, debug: Boolean) =
    this(input, debug, scanOnInit = true)

  private val scanner =
    Tokenizer(initialInput, 0, ExperimentalFlags.None)

  private var experimentalFlags = ExperimentalFlags.None

  // lookahead buffers: only written when a peek must preserve a token
  private val bufA = new TokenSlots
  private val bufB = new TokenSlots

  /** the current token: the scanner's own output slots unless a peek buffered it */
  private var cur: TokenSlots = scanner.out

  /** number of tokens scanned beyond the current one (0, 1, or 2) */
  private var lookaheadCount = 0

  /** Whether `cur` holds a scanned, un-consumed token. Scanning is lazy: [[advance]] only marks the
    * current token consumed, and the next [[currentKind]] (or any other token accessor) scans on
    * demand. Between tokens (`hasToken == false`) the char-level readers below can consume
    * structure directly off the input without materializing tokens at all.
    */
  private var hasToken = false

  /** Re-aims this stream at the start of a new input, for reuse from a pool. */
  protected def resetStream(newInput: String, newDebug: Boolean): Unit =
    debug = newDebug
    experimentalFlags = ExperimentalFlags.None
    scanner.reset(newInput, experimentalFlags)
    resetStreamState()

  /** [[resetStream]] over a UTF-8 byte input — see [[Tokenizer.resetBytes]]. */
  protected def resetStreamBytes(newInput: Array[Byte], newDebug: Boolean): Unit =
    debug = newDebug
    experimentalFlags = ExperimentalFlags.None
    scanner.resetBytes(newInput, experimentalFlags)
    resetStreamState()

  private def resetStreamState(): Unit =
    bufA.str = null // release references to the previous input's strings
    bufB.str = null
    cur = scanner.out
    lookaheadCount = 0
    hasToken = false

  private def scanNextToken(): Unit =
    scanner.scanNext()
    if debug then
      Console.err.println(
        Tokenizer.materialize(scanner)
      ) // TODO: should allow logger customization in the future

  private def ensureToken(): Unit =
    if !hasToken then
      scanNextToken()
      cur = scanner.out
      hasToken = true

  protected final def addExperimentalFlags(flags: Int): Unit =
    experimentalFlags |= flags
    scanner.setExperimentalFlags(experimentalFlags)

  protected final def experimentalFlagEnabled(flag: Int): Boolean =
    ExperimentalFlags.enabled(experimentalFlags, flag)

  protected def currentKind(): Int =
    ensureToken()
    cur.kind

  protected def currentOffset(): Int =
    ensureToken()
    cur.start

  private def otherBuffer: TokenSlots =
    if cur eq bufA then bufB else bufA

  private def ensureLookahead(count: Int): Unit =
    ensureToken()
    while lookaheadCount < count do
      // preserve the newest un-consumed token before the scanner overwrites its slots; the first
      // peek moves the current token itself into a buffer
      val buffer = otherBuffer
      buffer.copyFrom(scanner.out)
      if lookaheadCount == 0 then cur = buffer
      scanNextToken()
      lookaheadCount += 1

  protected def peekKind(): Int =
    ensureLookahead(1)
    if lookaheadCount == 1 then scanner.out.kind else otherBuffer.kind

  protected def peekSecondKind(): Int =
    ensureLookahead(2)
    scanner.out.kind

  protected def advance(): Unit =
    ensureToken()
    if lookaheadCount == 0 then hasToken = false
    else if lookaheadCount == 1 then
      cur = scanner.out
      lookaheadCount = 0
    else
      cur = otherBuffer
      lookaheadCount = 1

  // --- char-level readers for the schema decode layer -------------------------------------------
  // Valid only between tokens (no pending current token — the stream's resting state under lazy
  // scanning). Each either consumes exactly the asked-for shape or consumes nothing and returns
  // false/0; the caller then falls back to the token path, which scans the same characters
  // generically for identical semantics and errors.

  /** true when no scanned, un-consumed token is pending */
  protected final def betweenTokens: Boolean = !hasToken

  /** Fused char-level read of a `<expected> =` field header: 1 with both consumed, 0 with only the
    * name consumed as a slice (readable via [[sliceNameOffset]]), -1 with nothing consumed (a
    * pending token or no plain identifier next).
    */
  protected final inline def expectFieldHeader(expected: Array[Char]): Int =
    if hasToken then -1 /* Tokenizer.HeaderNone: inline vals cannot fold here */
    else scanner.scanFieldHeader(expected)

  /** Scans a plain-identifier field name as an offset slice — no token, no classification. On
    * success the slice offset is readable via [[sliceNameOffset]] until the next scan;
    * [[rescanNameSliceAsToken]] rewinds and rescans it generically for mismatch handling.
    */
  protected final inline def tryReadNameSlice(): Boolean =
    !hasToken && scanner.scanIdentSlice()

  protected final def sliceNameOffset(): Int = scanner.out.start

  /** Compares the last [[tryReadNameSlice]] result against an expected name's chars — a plain array
    * compare, no classification and no allocation.
    */
  protected final def sliceNameEquals(expected: Array[Char]): Boolean =
    val slots = scanner.out
    val from  = slots.start
    val len   = slots.end - from
    if len != expected.length then false
    else
      val text = scanner.chars
      var i    = 0
      var ok   = true
      while ok && i < len do
        if text(from + i) != expected(i) then ok = false
        else i += 1
      ok

  /** Rescans the last [[tryReadNameSlice]] result as a generic token, so the token path sees the
    * identical characters (classification included) for error reporting and non-plain matching.
    */
  protected final def rescanNameSliceAsToken(): Unit =
    scanner.rewindTo(scanner.out.start)
    ensureToken()

  protected final inline def tryReadEqualsChar(): Boolean =
    !hasToken && scanner.scanEqualsChar()

  /** 1 = `,` consumed, 2 = `closing` consumed, 3 = a trailing `,` and the `closing` both consumed,
    * 0 = nothing consumed (pending token or another shape).
    */
  protected final inline def tryReadSeparatorChar(closing: Char): Int =
    if hasToken then 0 /* Tokenizer.SeparatorNone: inline vals cannot fold here */
    else scanner.scanSeparatorChar(closing)

  protected final inline def tryReadPunctChar(expected: Char): Boolean =
    !hasToken && scanner.scanPunctChar(expected)

  /** Whether a `+` operator follows, probed at the char level — consumes nothing (beyond trivia),
    * so the stream stays between tokens when the answer is false.
    */
  protected final inline def probePlusChar(): Boolean =
    !hasToken && scanner.peekPlusChar()

  /** Char-level read of a `true`/`false` value: 1/0 consumed, -1 nothing consumed. */
  protected final inline def tryReadBooleanChar(): Int =
    if hasToken then -1 /* Tokenizer.BooleanNone: inline vals cannot fold here */
    else scanner.scanBooleanValue()

  // Scanner-direct value scans: on success the literal sits in the scanner slots (`cur` aims at
  // them whenever the stream is between tokens) and is semantically consumed — no pending token.
  // [[adoptScannedToken]] turns the scanned literal into the pending current token instead, for
  // error paths that must report it exactly like the generic token path.

  /** 0 = nothing consumed, 1 = positive literal scanned, 2 = negated literal scanned */
  protected final inline def tryScanSignedNumberDirect(): Int =
    if hasToken then 0 /* Tokenizer.NumberNone: inline vals cannot fold here */
    else scanner.scanSignedNumberValue()

  protected final inline def tryScanStringDirect(): Boolean =
    !hasToken && scanner.scanStringValue()

  /** Char-level read of an escape-free, non-concatenated string literal's content as a slice — see
    * [[Tokenizer.scanStringContentSlice]]. On success the slice compares via [[sliceNameEquals]]
    * (and materializes via [[sliceContentString]] for error text only) until the next scan; on
    * false nothing is consumed and the general string decode sees the identical characters.
    */
  protected final inline def tryReadStringContentSlice(): Boolean =
    !hasToken && scanner.scanStringContentSlice()

  /** materializes the last [[tryReadStringContentSlice]] result — error paths only */
  protected final def sliceContentString(): String =
    new String(scanner.chars, scanner.out.start, scanner.out.end - scanner.out.start)

  /** the kind of the literal scanned by the last successful direct scan */
  protected final def scannedKind(): Int = scanner.out.kind

  /** the string payload of the literal scanned by the last successful direct scan */
  protected final def scannedStringValue(): String = scanner.out.str.nn

  /** makes the directly scanned literal the pending current token (error reporting) */
  protected final def adoptScannedToken(): Unit =
    cur = scanner.out
    hasToken = true

  /** start offset of the last successful char-level read — for error spans */
  protected final def charScanOffset(): Int = scanner.charScanStart

  // payload accessors — happy path, unboxed where primitive. Int and Long take the sign decoded
  // from the preceding token: the value is interpreted here, in place from the input slice, so
  // MinValue (whose magnitude overflows the positive range) parses once the sign is known.
  // Callers reach these after inspecting currentKind(), so the token is already scanned.
  protected def currentName(): String        = nameAt(cur)
  protected def currentStringValue(): String = cur.str.nn
  protected def currentCharValue(): Char     = cur.num.toChar
  protected def currentFloatValue(): Float   = cur.dbl.toFloat
  protected def currentDoubleValue(): Double = cur.dbl

  protected def currentNameMatches(expected: String): Boolean =
    val cached = cur.str
    if cached != null then cached == expected
    else sliceMatches(cur, expected)

  private def nameAt(slots: TokenSlots): String =
    val cached = slots.str
    if cached != null then cached
    else
      val name = new String(scanner.chars, slots.start, slots.end - slots.start)
      slots.str = name
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
        val cached = cur.str
        if cached != null then cached == expected
        else sliceMatches(cur, expected)
      case TokenKind.VectorId     => expected == "Vector"
      case TokenKind.EmptyTupleId => expected == "EmptyTuple"
      case TokenKind.TupleId      => expected == "Tuple"
      case TokenKind.Plus         => expected == "+"
      case TokenKind.Minus        => expected == "-"
      case _                      => false

  private inline def sliceMatches(slots: TokenSlots, expected: String): Boolean =
    // field names are typically a handful of chars: a plain loop beats regionMatches' setup
    val from = slots.start
    val len  = slots.end - from
    if len != expected.length then false
    else
      var i  = 0
      var ok = true
      while ok && i < len do
        if scanner.chars(from + i) != expected.charAt(i) then ok = false
        else i += 1
      ok

  protected def currentIntValue(negative: Boolean): Int =
    val digits = cur.accDigits
    if digits >= 1 && digits <= Tokenizer.MaxIntDigits then
      val negated = cur.num
      val limit   = if negative then Int.MinValue.toLong else -Int.MaxValue.toLong
      if negated >= limit then (if negative then negated else -negated).toInt
      else Tokenizer.intValueAt(scanner.chars, cur.start, cur.end, negative) // exact overflow
    else Tokenizer.intValueAt(scanner.chars, cur.start, cur.end, negative)

  /** raw source text of the current token — the raw-number decode's literal capture */
  protected def currentRawText(): String =
    new String(scanner.chars, cur.start, cur.end - cur.start)

  protected def currentLongValue(negative: Boolean): Long =
    val digits = cur.accDigits
    // up to 18 digits the negated magnitude cannot overflow, so no range check is needed
    if digits >= 1 && digits <= Tokenizer.MaxAccumulatedDigits then
      if negative then cur.num else -cur.num
    else Tokenizer.longValueAt(scanner.chars, cur.start, cur.end, negative)

  // error-path materialization
  protected def spanAt(offset: Int): DecodeError.Span =
    Tokenizer.spanAt(scanner.chars, scanner.inputLength, offset)

  protected def currentSpan(): DecodeError.Span =
    ensureToken()
    spanAt(cur.start)

  protected def describeCurrent(): String =
    ensureToken()
    describeSlot(cur)

  private def describeSlot(slots: TokenSlots): String =
    def raw = new String(scanner.chars, slots.start, slots.end - slots.start)
    slots.kind match
      case TokenKind.PackageKw    => "'package'"
      case TokenKind.ValKw        => "'val'"
      case TokenKind.VectorId     => "'Vector'"
      case TokenKind.TrueKw       => "'true'"
      case TokenKind.FalseKw      => "'false'"
      case TokenKind.NullKw       => "'null'"
      case TokenKind.EmptyTupleId => "'EmptyTuple'"
      case TokenKind.TupleId      => "'Tuple'"
      case TokenKind.Keyword      => s"'$raw'"
      case TokenKind.Identifier   => s"identifier '${nameAt(slots)}'"
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
      case TokenKind.LBracket     => "'['"
      case TokenKind.RBracket     => "']'"
      case TokenKind.Arrow        => "'->'"
      case TokenKind.LBrace       => "'{'"
      case TokenKind.RBrace       => "'}'"
      case _                      => "end of input"
}
