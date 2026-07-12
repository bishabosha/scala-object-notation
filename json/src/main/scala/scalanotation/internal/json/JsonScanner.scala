package scalanotation.internal.json

import scalanotation.internal.PushSlots

import java.nio.charset.StandardCharsets

/** Lexical error in the JSON input, converted to a [[scalanotation.DecodeError]] at the decode
  * entry point (mirrors the core TokenizeException protocol). Stackless: the exception is control
  * flow for the cold error path.
  */
private[json] final class JsonParseException(val message: String, val offset: Int)
    extends RuntimeException(message, null, false, false)

private[json] object JsonScanner:
  /** results of [[JsonScanner.tryReadSeparator]] */
  inline val SeparatorComma   = 0
  inline val SeparatorClosing = 1
  inline val SeparatorNone    = -1

  /** results of [[JsonScanner.tryReadBoolean]] */
  inline val BooleanTrue  = 1
  inline val BooleanFalse = 0
  inline val BooleanNone  = -1

  /** Hard cap on the pooled String-input transcoding buffer: a pooled scanner retains at most 16
    * KB; larger (or non-ASCII) inputs transcode into a transient array discarded after use.
    */
  inline val MaxPooledInputBytes = 16384

  /** exact powers of ten for the one-multiply double conversion (all are exact doubles) */
  private[json] val Pow10: Array[Double] =
    Array.tabulate(23)(i => math.pow(10d, i.toDouble))

  /** exact powers of ten in Float range (1e0..1e10 are exact floats) */
  private[json] val Pow10F: Array[Float] =
    Array.tabulate(11)(i => math.pow(10d, i.toDouble).toFloat)

/** Byte-level scanning kernel for the JSON decoder: the input is always a UTF-8 byte array (a
  * String input transcodes on reset), the cursor is a plain Int, and every scan primitive is
  * probe-first — the expected byte is checked directly at the cursor before any whitespace
  * machinery, and a miss backs out to the whitespace skip and re-probes once. There is exactly one
  * decode path: values are scanned where the schema expects them, with no token objects.
  */
private[json] abstract class JsonScanner extends PushSlots:
  import JsonScanner.*

  protected var input: Array[Byte] = Array.emptyByteArray
  protected var limit: Int         = 0
  protected var pos: Int           = 0

  /** pooled transcode buffer for String inputs — see [[JsonScanner.MaxPooledInputBytes]] */
  private var pooledInputBytes: Array[Byte] = Array.emptyByteArray

  /** reusable char buffer for decoding escaped string content */
  private var stringChars: Array[Char] = new Array[Char](64)

  protected final def resetScanner(bytes: Array[Byte]): Unit =
    input = bytes
    limit = bytes.length
    pos = 0

  /** Re-aims the scanner at a String input without materializing a byte copy for ASCII inputs that
    * fit the pooled buffer: chars widen scalar into the pooled bytes; the first non-ASCII char
    * delegates the whole input to the JDK UTF-8 encoder.
    */
  protected final def resetScannerString(text: String): Unit =
    val length = text.length
    if length <= MaxPooledInputBytes then
      var buffer = pooledInputBytes
      if buffer.length < length then
        buffer = new Array[Byte](math.min(math.max(length, 256), MaxPooledInputBytes))
        pooledInputBytes = buffer
      var i     = 0
      var ascii = true
      while ascii && i < length do
        val ch = text.charAt(i)
        if ch < 0x80 then
          buffer(i) = ch.toByte
          i += 1
        else ascii = false
      if ascii then
        input = buffer
        limit = length
        pos = 0
      else resetScanner(text.getBytes(StandardCharsets.UTF_8))
    else resetScanner(text.getBytes(StandardCharsets.UTF_8))

  // --- whitespace ---

  protected final def skipWs(): Unit =
    var p   = pos
    val in  = input
    val lim = limit
    while p < lim && {
        val b = in(p)
        b == ' ' || b == '\t' || b == '\n' || b == '\r'
      }
    do p += 1
    pos = p

  private inline def isWsByte(b: Byte): Boolean =
    b == ' ' || b == '\t' || b == '\n' || b == '\r'

  /** the offset of the next content byte (whitespace consumed) — for error spans */
  protected final def currentOffset(): Int =
    skipWs()
    pos

  // --- punctuation ---

  /** consumes `expected` (skipping leading whitespace); false consumes nothing */
  protected final def tryReadPunct(expected: Byte): Boolean =
    var p = pos
    if p < limit && input(p) == expected then
      pos = p + 1
      true
    else
      skipWs()
      p = pos
      if p < limit && input(p) == expected then
        pos = p + 1
        true
      else false

  /** Consumes `,` or the closing byte in one probe-first scan. [[SeparatorNone]] consumes nothing.
    */
  protected final def tryReadSeparator(closing: Byte): Int =
    var p = pos
    if p < limit then
      var b = input(p)
      if b == ',' then
        pos = p + 1
        return SeparatorComma
      if b == closing then
        pos = p + 1
        return SeparatorClosing
      if isWsByte(b) then
        skipWs()
        p = pos
        if p < limit then
          b = input(p)
          if b == ',' then
            pos = p + 1
            return SeparatorComma
          if b == closing then
            pos = p + 1
            return SeparatorClosing
    SeparatorNone

  // --- fused field header ---

  /** Matches the exact plan-cached header bytes (`"name":`) at the cursor: true consumes the name
    * and colon; false consumes at most leading whitespace, so the cold resolver re-reads the same
    * bytes.
    */
  protected final def expectFieldHeader(header: Array[Byte]): Boolean =
    val in = input
    var p  = pos
    if p >= limit || in(p) != '"' then
      skipWs()
      p = pos
      if p >= limit || in(p) != '"' then return false
    val end = p + header.length
    if end > limit then return false
    var i = 0
    while i < header.length && in(p + i) == header(i) do i += 1
    if i == header.length then
      pos = end
      true
    else false

  // --- strings ---

  /** content span of the most recent [[scanStringSlice]] */
  protected var sliceStart: Int = 0
  protected var sliceEnd: Int   = 0

  /** whether the most recent slice contains backslash escapes */
  protected var sliceEscaped: Boolean = false

  /** whether the most recent slice contains non-ASCII bytes */
  protected var sliceNonAscii: Boolean = false

  /** offset of the opening quote of the most recent slice — for error spans */
  protected var sliceQuoteOffset: Int = 0

  /** True when a string literal starts at the cursor (after whitespace): scans it, recording the
    * content span and its escape/ASCII flags without materializing. False consumes nothing.
    */
  protected final def tryScanStringSlice(): Boolean =
    var p = pos
    if p >= limit || input(p) != '"' then
      skipWs()
      p = pos
      if p >= limit || input(p) != '"' then return false
    sliceQuoteOffset = p
    p += 1
    val in       = input
    val lim      = limit
    val start    = p
    var escaped  = false
    var nonAscii = false
    var done     = false
    while !done do
      if p >= lim then throw JsonParseException("Unterminated string literal", sliceQuoteOffset)
      val b = in(p)
      if b == '"' then done = true
      else if b == '\\' then
        escaped = true
        p += 1
        if p >= lim then throw JsonParseException("Unterminated string literal", sliceQuoteOffset)
        p += 1 // skip the escaped byte; \uXXXX hex bytes are all non-quote ASCII, scanned plainly
      else if (b & 0xff) < 0x20 then
        throw JsonParseException("Unescaped control character in string literal", p)
      else
        if b < 0 then nonAscii = true
        p += 1
    sliceStart = start
    sliceEnd = p
    sliceEscaped = escaped
    sliceNonAscii = nonAscii
    pos = p + 1 // past the closing quote
    true

  /** compares the most recent (escape-free) slice content against `content` byte-for-byte */
  protected final def sliceEquals(content: Array[Byte]): Boolean =
    val length = sliceEnd - sliceStart
    if length != content.length then false
    else
      val in    = input
      val start = sliceStart
      var i     = 0
      while i < length && in(start + i) == content(i) do i += 1
      i == length

  /** materializes the most recent slice as a String, decoding escapes if present */
  protected final def materializeSlice(): String =
    if !sliceEscaped then
      if !sliceNonAscii then
        new String(input, sliceStart, sliceEnd - sliceStart, StandardCharsets.ISO_8859_1)
      else new String(input, sliceStart, sliceEnd - sliceStart, StandardCharsets.UTF_8)
    else decodeEscapedSlice()

  /** decodes an escaped slice: escape-free runs bulk-decode, escapes append decoded chars */
  private def decodeEscapedSlice(): String =
    val in      = input
    val end     = sliceEnd
    val builder = new java.lang.StringBuilder(end - sliceStart)
    var p       = sliceStart
    var run     = p
    while p < end do
      if in(p) == '\\' then
        if p > run then appendRun(builder, run, p)
        p += 1
        p = appendEscape(builder, p)
        run = p
      else p += 1
    if p > run then appendRun(builder, run, p)
    builder.toString

  private def appendRun(builder: java.lang.StringBuilder, from: Int, until: Int): Unit =
    // a run may contain non-ASCII UTF-8; decode the run in one step
    var ascii = true
    var i     = from
    while ascii && i < until do
      if input(i) < 0 then ascii = false
      i += 1
    if ascii then
      i = from
      while i < until do
        builder.append(input(i).toChar)
        i += 1
    else builder.append(new String(input, from, until - from, StandardCharsets.UTF_8))

  /** appends the escape starting at `p` (the byte after the backslash), returning the next position
    */
  private def appendEscape(builder: java.lang.StringBuilder, p: Int): Int =
    if p >= limit then throw JsonParseException("Unterminated string literal", sliceQuoteOffset)
    (input(p): @annotation.switch) match
      case '"' =>
        builder.append('"'); p + 1
      case '\\' =>
        builder.append('\\'); p + 1
      case '/' =>
        builder.append('/'); p + 1
      case 'b' =>
        builder.append('\b'); p + 1
      case 'f' =>
        builder.append('\f'); p + 1
      case 'n' =>
        builder.append('\n'); p + 1
      case 'r' =>
        builder.append('\r'); p + 1
      case 't' =>
        builder.append('\t'); p + 1
      case 'u' =>
        if p + 4 >= limit then throw JsonParseException("Unterminated unicode escape", p - 1)
        builder.append(parseHex4(p + 1).toChar)
        p + 5
      case other =>
        throw JsonParseException(s"Invalid escape character '${(other & 0xff).toChar}'", p - 1)

  private def parseHex4(p: Int): Int =
    var value = 0
    var i     = 0
    while i < 4 do
      val b     = input(p + i)
      val digit =
        if b >= '0' && b <= '9' then b - '0'
        else if b >= 'a' && b <= 'f' then b - 'a' + 10
        else if b >= 'A' && b <= 'F' then b - 'A' + 10
        else throw JsonParseException(s"Invalid hex digit '${(b & 0xff).toChar}'", p + i)
      value = (value << 4) | digit
      i += 1
    value

  /** re-scans the most recent slice as a Char value; -1 when the content is not exactly one char */
  protected final def sliceAsChar(): Int =
    if !sliceEscaped && !sliceNonAscii then
      if sliceEnd - sliceStart == 1 then input(sliceStart) & 0xff
      else -1
    else
      val text = materializeSlice()
      if text.length == 1 then text.charAt(0).toInt else -1

  // --- numbers ---

  // Result state of the most recent scanNumber: the negated significant digits (negated so
  // Long.MinValue accumulates without overflow), digit count, whether the literal is integral
  // (no fraction and no exponent), the effective decimal exponent (exponent minus fraction
  // digits), whether the accumulator is incomplete (too many digits — slow-path interpretation
  // required), the sign, and the literal's span for slow-path re-interpretation.
  protected var numNegAcc: Long      = 0L
  protected var numDigits: Int       = 0
  protected var numIntegral: Boolean = true
  protected var numE10: Int          = 0
  protected var numDirty: Boolean    = false
  protected var numNeg: Boolean      = false
  protected var numStart: Int        = 0
  protected var numEnd: Int          = 0

  /** True when a number starts at the cursor (after whitespace): scans one JSON number into the
    * number state. False consumes nothing. Malformed numbers throw [[JsonParseException]].
    */
  protected final def tryScanNumber(): Boolean =
    var p = pos
    if p >= limit || !isNumberStart(input(p)) then
      skipWs()
      p = pos
      if p >= limit || !isNumberStart(input(p)) then return false
    scanNumberAt(p)
    true

  private inline def isNumberStart(b: Byte): Boolean =
    (b >= '0' && b <= '9') || b == '-'

  private def scanNumberAt(startPos: Int): Unit =
    val in  = input
    val lim = limit
    var p   = startPos
    numStart = p
    var neg = false
    if in(p) == '-' then
      neg = true
      p += 1
      if p >= lim || in(p) < '0' || in(p) > '9' then
        throw JsonParseException("Expected a digit after '-'", p)
    var acc      = 0L
    var digits   = 0
    var fracLen  = 0
    var integral = true
    // integer part: 0 | [1-9][0-9]*
    if in(p) == '0' then
      p += 1
      if p < lim && in(p) >= '0' && in(p) <= '9' then
        throw JsonParseException("Leading zeros are not allowed", numStart)
    else
      while p < lim && in(p) >= '0' && in(p) <= '9' do
        if digits < 18 then acc = acc * 10 - (in(p) - '0')
        digits += 1
        p += 1
    // fraction
    if p < lim && in(p) == '.' then
      integral = false
      p += 1
      if p >= lim || in(p) < '0' || in(p) > '9' then
        throw JsonParseException("Expected a digit after '.'", p)
      while p < lim && in(p) >= '0' && in(p) <= '9' do
        val d = in(p) - '0'
        if digits >= 18 then
          // the accumulator is full: dirty below, the slow path re-interprets the raw text
          digits += 1
        else if acc == 0L && d == 0 then
          // leading fraction zeros are positional only: they extend the scale without
          // consuming mantissa digit budget
          fracLen += 1
        else
          acc = acc * 10 - d
          digits += 1
          fracLen += 1
        p += 1
    val dirty = digits > 17
    // exponent
    var expValue = 0
    if p < lim && (in(p) == 'e' || in(p) == 'E') then
      integral = false
      p += 1
      var expNeg = false
      if p < lim && (in(p) == '+' || in(p) == '-') then
        expNeg = in(p) == '-'
        p += 1
      if p >= lim || in(p) < '0' || in(p) > '9' then
        throw JsonParseException("Expected a digit in the exponent", p)
      while p < lim && in(p) >= '0' && in(p) <= '9' do
        if expValue < 100000 then expValue = expValue * 10 + (in(p) - '0')
        p += 1
      if expNeg then expValue = -expValue
    numNegAcc = acc
    numDigits = digits
    numIntegral = integral
    numE10 = expValue - fracLen
    numDirty = dirty
    numNeg = neg
    numEnd = p
    pos = p

  /** the raw text of the most recent number literal (ASCII by construction) */
  protected final def rawNumberString(): String =
    new String(input, numStart, numEnd - numStart, StandardCharsets.ISO_8859_1)

  /** Interprets the most recent number as an Int. The Boolean result reports range/shape fit; the
    * value lands in the int slot.
    */
  protected final def numberAsInt(): Boolean =
    if !numIntegral || numDigits > 10 then false
    else
      val negLimit = if numNeg then Int.MinValue.toLong else -Int.MaxValue.toLong
      if numNegAcc < negLimit then false
      else
        pushInt((if numNeg then numNegAcc else -numNegAcc).toInt)
        true

  /** Interprets the most recent number as a Long — see [[numberAsInt]]. */
  protected final def numberAsLong(): Boolean =
    if !numIntegral then false
    else if numDigits <= 18 then
      pushLong(if numNeg then numNegAcc else -numNegAcc)
      true
    else if numDigits == 19 then
      // the accumulator only carries 18 digits; the full literal re-interprets exactly once
      try
        pushLong(java.lang.Long.parseLong(rawNumberString()))
        true
      catch case _: NumberFormatException => false
    else false

  /** Interprets the most recent number as a Double, using the exact power-of-ten conversion when
    * the mantissa and scale allow (one multiply or divide, bit-for-bit with parseDouble), and the
    * JDK parser otherwise. The value lands in the double slot.
    */
  protected final def numberAsDouble(): Unit =
    val mantissa = -numNegAcc
    val e10      = numE10
    if !numDirty && mantissa <= (1L << 53) && e10 >= -22 && e10 <= 22 then
      var value = mantissa.toDouble
      if e10 > 0 then value *= Pow10(e10)
      else if e10 < 0 then value /= Pow10(-e10)
      pushDouble(if numNeg then -value else value)
    else pushDouble(java.lang.Double.parseDouble(rawNumberString()))

  /** Interprets the most recent number as a Float — the float-range mirror of [[numberAsDouble]].
    */
  protected final def numberAsFloat(): Unit =
    val mantissa = -numNegAcc
    val e10      = numE10
    if !numDirty && mantissa <= (1L << 24) && e10 >= -10 && e10 <= 10 then
      var value = mantissa.toFloat
      if e10 > 0 then value *= Pow10F(e10)
      else if e10 < 0 then value /= Pow10F(-e10)
      pushFloat(if numNeg then -value else value)
    else pushFloat(java.lang.Float.parseFloat(rawNumberString()))

  // --- keywords ---

  /** consumes `true` or `false` (whitespace-skipping, probe-first); [[BooleanNone]] consumes
    * nothing
    */
  protected final def tryReadBoolean(): Int =
    var p = pos
    if p < limit then
      var b = input(p)
      if isWsByte(b) then
        skipWs()
        p = pos
        if p >= limit then return BooleanNone
        b = input(p)
      if b == 't' then
        expectLiteral(p, "true")
        return BooleanTrue
      if b == 'f' then
        expectLiteral(p, "false")
        return BooleanFalse
    BooleanNone

  /** consumes `null` (whitespace-skipping, probe-first); false consumes nothing */
  protected final def tryReadNull(): Boolean =
    var p = pos
    if p < limit then
      var b = input(p)
      if isWsByte(b) then
        skipWs()
        p = pos
        if p >= limit then return false
        b = input(p)
      if b == 'n' then
        expectLiteral(p, "null")
        return true
    false

  private def expectLiteral(start: Int, literal: String): Unit =
    if start + literal.length > limit then
      throw JsonParseException(s"Invalid literal, expected '$literal'", start)
    var i = 1
    while i < literal.length do
      if input(start + i) != literal.charAt(i) then
        throw JsonParseException(s"Invalid literal, expected '$literal'", start)
      i += 1
    pos = start + literal.length

  // --- descriptions and spans (error path only) ---

  protected final def atEof(): Boolean =
    skipWs()
    pos >= limit

  /** describes the value starting at the cursor, for ExpectedType errors */
  protected final def describeCurrent(): String =
    skipWs()
    if pos >= limit then "end of input"
    else
      (input(pos): @annotation.switch) match
        case '{'                                                             => "an object"
        case '['                                                             => "an array"
        case '"'                                                             => "a string"
        case 't' | 'f'                                                       => "a boolean"
        case 'n'                                                             => "null"
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => "a number"
        case other => s"'${(other & 0xff).toChar}'"

  protected final def currentSpan(): scalanotation.DecodeError.Span =
    spanAt(currentOffset())

  /** Line/column for `offset`, computed by decoding the input on this cold error path only. The
    * column counts chars of the decoded text, matching the core decoder's byte-input spans.
    */
  protected final def spanAt(offset: Int): scalanotation.DecodeError.Span =
    JsonSpans.spanAt(input, limit, offset)

private[json] object JsonSpans:
  def spanAt(input: Array[Byte], limit: Int, offset: Int): scalanotation.DecodeError.Span =
    val bounded = math.min(math.max(offset, 0), limit)
    var line    = 1
    var column  = 1
    var i       = 0
    while i < bounded do
      val b = input(i)
      if b == '\n' then
        line += 1
        column = 1
      else if b >= 0 || (b & 0xc0) == 0xc0 then
        // count code-point starts only, so multi-byte UTF-8 advances the column once
        column += 1
      i += 1
    scalanotation.DecodeError.Span(bounded, line, column)
