package scalanotation.internal.json

import scalanotation.internal.PushSlots

import java.nio.charset.StandardCharsets

/** Lexical error in the JSON input, converted to a [[scalanotation.DecodeError]] at the decode
  * entry point (mirrors the core TokenizeException protocol). Stackless: the exception is control
  * flow for the cold error path. The offset is absolute in the whole input (buffer-relative
  * positions shift in streaming mode).
  */
private[json] final class JsonParseException(val message: String, val offset: Long)
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

  /** Hard cap on the pooled buffer (String-input transcoding and stream refills): a pooled scanner
    * retains at most 16 KB; larger inputs or tokens use a transient array discarded after use.
    */
  inline val MaxPooledInputBytes = 16384

  /** exact powers of ten for the one-multiply double conversion (all are exact doubles) */
  private[json] val Pow10: Array[Double] =
    Array.tabulate(23)(i => math.pow(10d, i.toDouble))

  /** exact powers of ten in Float range (1e0..1e10 are exact floats) */
  private[json] val Pow10F: Array[Float] =
    Array.tabulate(11)(i => math.pow(10d, i.toDouble).toFloat)

/** Byte-level scanning kernel for the JSON decoder: the input is a UTF-8 byte window (the whole
  * input, a pooled String transcode, or a streaming refill buffer), the cursor is a plain Int, and
  * every scan primitive is probe-first — the expected byte is checked directly at the cursor before
  * any whitespace machinery, and a miss backs out to the whitespace skip and re-probes once. There
  * is exactly one decode path: values are scanned where the schema expects them, with no token
  * objects.
  *
  * Streaming: with an attached [[java.io.InputStream]], the `pos >= limit` exhaustion branches —
  * and only they — attempt [[refillKeeping]], which discards consumed bytes, shifts the in-progress
  * token to the buffer start (adjusting the cursor and the slice/number span state), and reads
  * more. Whole-input modes have no stream, so every refill answers "no more" and the scan shapes
  * behave exactly as before. Error offsets are absolute ([[bufferBase]] plus a buffer position);
  * line/column accounting for discarded bytes happens at compaction time.
  */
private[json] abstract class JsonScanner extends PushSlots:
  import JsonScanner.*

  protected var input: Array[Byte] = Array.emptyByteArray
  protected var limit: Int         = 0
  protected var pos: Int           = 0

  /** the streaming source, or null in whole-input modes (and once a stream is exhausted) */
  private var stream: java.io.InputStream | Null = null

  /** absolute offset of `input(0)` in the whole input — 0 until stream compaction discards */
  protected var bufferBase: Long = 0L

  // line/column accounting for the discarded prefix, so error spans stay exact after compaction:
  // full newlines before bufferBase, and the 1-based code-point column AT bufferBase
  private var discardedLines: Int  = 0
  private var discardedColumn: Int = 1

  /** pooled buffer for String transcoding and stream refills — see [[MaxPooledInputBytes]] */
  private var pooledInputBytes: Array[Byte] = Array.emptyByteArray

  protected final def resetScanner(bytes: Array[Byte]): Unit =
    input = bytes
    limit = bytes.length
    pos = 0
    stream = null
    bufferBase = 0L
    discardedLines = 0
    discardedColumn = 1

  /** Re-aims the scanner at a String input without materializing a byte copy for ASCII inputs that
    * fit the pooled buffer: one fused pass probes and widens chars into the pooled bytes (portable
    * — the JDK's faster latin-1 transfers do not exist on every platform); the first non-ASCII char
    * delegates the whole input to the JDK UTF-8 encoder.
    */
  protected final def resetScannerString(text: String): Unit =
    val length = text.length
    if length <= MaxPooledInputBytes then
      var buffer = pooledInputBytes
      if buffer.length < length then
        buffer = new Array[Byte](math.min(math.max(length, 256), MaxPooledInputBytes))
        pooledInputBytes = buffer
      var i        = 0
      var ch: Char = 0
      while i < length && { ch = text.charAt(i); ch < 0x80 } do
        buffer(i) = ch.toByte
        i += 1
      if i == length then
        resetScanner(buffer)
        limit = length
      else resetScanner(text.getBytes(StandardCharsets.UTF_8))
    else resetScanner(text.getBytes(StandardCharsets.UTF_8))

  /** Re-aims the scanner at a streaming input: scanning refills the pooled buffer on demand, so a
    * body larger than the buffer decodes without ever being materialized whole.
    */
  protected final def resetScannerStream(in: java.io.InputStream): Unit =
    var buffer = pooledInputBytes
    if buffer.length < MaxPooledInputBytes then
      buffer = new Array[Byte](MaxPooledInputBytes)
      pooledInputBytes = buffer
    resetScanner(buffer)
    limit = 0
    stream = in

  /** Discards consumed bytes before `protect`, shifts the retained span to the buffer start, and
    * reads more from the stream (growing the buffer when the protected span fills it). Returns the
    * shift distance — callers subtract it from any live buffer positions; the cursor and the
    * slice/number span state are adjusted here. New data is signalled by `limit` growing; at end of
    * stream (or in whole-input modes) `limit` is unchanged.
    */
  protected final def refillKeeping(protect: Int): Int =
    val in = stream
    if in == null then 0
    else
      var shift = 0
      if protect > 0 then
        accountDiscarded(protect)
        val retained = limit - protect
        System.arraycopy(input, protect, input, 0, retained)
        bufferBase += protect
        limit = retained
        pos = math.max(pos - protect, 0)
        sliceStart -= protect
        sliceEnd -= protect
        sliceQuoteOffset -= protect
        numStart -= protect
        numEnd -= protect
        shift = protect
      else if limit == input.length then
        // the protected span fills the whole buffer: grow (transient beyond the pooled cap)
        val grown = java.util.Arrays.copyOf(input, math.max(input.length * 2, MaxPooledInputBytes))
        input = grown
        if grown.length <= MaxPooledInputBytes then pooledInputBytes = grown
      var n = 0
      while n == 0 do n = in.read(input, limit, input.length - limit)
      if n < 0 then stream = null
      else limit += n
      shift

  /** [[refillKeeping]] from the cursor — for scan points with no token in progress. True when a
    * byte is available at the (possibly shifted) cursor.
    */
  private def refillAtPos(): Boolean =
    refillKeeping(pos)
    pos < limit

  /** line/column accounting for a discarded `[0, until)` prefix — see [[spanAt]] */
  private def accountDiscarded(until: Int): Unit =
    var i = 0
    while i < until do
      val b = input(i)
      if b == '\n' then
        discardedLines += 1
        discardedColumn = 1
      else if b >= 0 || (b & 0xc0) == 0xc0 then
        // count code-point starts only, so multi-byte UTF-8 advances the column once
        discardedColumn += 1
      i += 1

  // --- whitespace ---

  /** Consumes whitespace; afterwards either a content byte is at the cursor or the input is
    * exhausted (streams refill as needed, so `pos >= limit` after this means true end of input).
    */
  protected final def skipWs(): Unit =
    var p        = pos
    var scanning = true
    while scanning do
      val in  = input
      val lim = limit
      while p < lim && {
          val b = in(p)
          b == ' ' || b == '\t' || b == '\n' || b == '\r'
        }
      do p += 1
      if p < lim then scanning = false
      else
        pos = p
        if !refillAtPos() then scanning = false
        p = pos
    pos = p

  private inline def isWsByte(b: Byte): Boolean =
    b == ' ' || b == '\t' || b == '\n' || b == '\r'

  /** the absolute offset of the next content byte (whitespace consumed) — for error spans */
  protected final def currentOffset(): Long =
    skipWs()
    bufferBase + pos

  /** the absolute offset of buffer position `local` */
  protected final def absoluteOffset(local: Int): Long =
    bufferBase + local

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
      val b = input(p)
      if b == ',' then
        pos = p + 1
        return SeparatorComma
      if b == closing then
        pos = p + 1
        return SeparatorClosing
      if !isWsByte(b) then return SeparatorNone
    skipWs()
    p = pos
    if p < limit then
      val b = input(p)
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
    var p = pos
    if p >= limit || input(p) != '"' then
      skipWs()
      p = pos
      if p >= limit || input(p) != '"' then return false
    // the header must be visible whole; streams refill (keeping the unconsumed name) until it
    // is, or until the input can no longer contain it
    while p + header.length > limit do
      val available = limit - p
      p -= refillKeeping(p)
      if limit - p == available then return false
    val in = input
    // the leading quote was just probed; compare the name bytes and colon
    var i = 1
    while i < header.length && in(p + i) == header(i) do i += 1
    if i == header.length then
      pos = p + header.length
      true
    else
      pos = p
      false

  // --- strings ---

  /** content span of the most recent [[tryScanStringSlice]] */
  protected var sliceStart: Int = 0
  protected var sliceEnd: Int   = 0

  /** whether the most recent slice contains backslash escapes */
  protected var sliceEscaped: Boolean = false

  /** whether the most recent slice contains non-ASCII bytes */
  protected var sliceNonAscii: Boolean = false

  /** buffer offset of the opening quote of the most recent slice — capture [[sliceQuoteAbsolute]]
    * before any further scanning when saving it for error spans
    */
  protected var sliceQuoteOffset: Int = 0

  protected final def sliceQuoteAbsolute(): Long =
    bufferBase + sliceQuoteOffset

  /** True when a string literal starts at the cursor (after whitespace): scans it, recording the
    * content span and its escape/ASCII flags without materializing. False consumes nothing. A
    * streaming refill keeps the whole literal in the buffer (growing it for oversized strings).
    */
  protected final def tryScanStringSlice(): Boolean =
    var quoteAt = pos
    if quoteAt >= limit || input(quoteAt) != '"' then
      skipWs()
      quoteAt = pos
      if quoteAt >= limit || input(quoteAt) != '"' then return false
    var p        = quoteAt + 1
    var start    = p
    var escaped  = false
    var nonAscii = false
    var done     = false
    while !done do
      val in       = input
      val lim      = limit
      var scanning = true
      while scanning && p < lim do
        val b = in(p)
        if b == '"' then
          done = true
          scanning = false
        else if b == '\\' then
          escaped = true
          if p + 1 < lim then p += 2
          else scanning = false // boundary mid-escape: leave p at the backslash and refill
        else if (b & 0xff) < 0x20 then
          throw JsonParseException("Unescaped control character in string literal", bufferBase + p)
        else
          if b < 0 then nonAscii = true
          p += 1
      if !done then
        // the literal touches the window end: refill keeping it (streams only), else fail
        val before = limit - quoteAt
        val shift  = refillKeeping(quoteAt)
        quoteAt -= shift
        start -= shift
        p -= shift
        if limit - quoteAt == before then
          throw JsonParseException("Unterminated string literal", bufferBase + quoteAt)
    sliceStart = start
    sliceEnd = p
    sliceEscaped = escaped
    sliceNonAscii = nonAscii
    sliceQuoteOffset = quoteAt
    pos = p + 1 // past the closing quote
    true

  /** Hash of the most recent (escape-free) slice content, matching [[JsonFieldPlans.contentHash]]
    * over the plan-cached name bytes — the key into the per-schema name dispatch table.
    */
  protected final def sliceHash(): Int =
    val in  = input
    val end = sliceEnd
    var h   = 0
    var i   = sliceStart
    while i < end do
      h = h * 31 + in(i)
      i += 1
    h

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
    * — the whole slice is in the buffer, so no bounds can be exceeded before the closing quote that
    * follows it
    */
  private def appendEscape(builder: java.lang.StringBuilder, p: Int): Int =
    if p >= limit then throw JsonParseException("Unterminated string literal", sliceQuoteAbsolute())
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
        if p + 4 >= limit then
          throw JsonParseException("Unterminated unicode escape", bufferBase + p - 1)
        builder.append(parseHex4(p + 1).toChar)
        p + 5
      case other =>
        throw JsonParseException(
          s"Invalid escape character '${(other & 0xff).toChar}'",
          bufferBase + p - 1
        )

  private def parseHex4(p: Int): Int =
    var value = 0
    var i     = 0
    while i < 4 do
      val b     = input(p + i)
      val digit =
        if b >= '0' && b <= '9' then b - '0'
        else if b >= 'a' && b <= 'f' then b - 'a' + 10
        else if b >= 'A' && b <= 'F' then b - 'A' + 10
        else
          throw JsonParseException(s"Invalid hex digit '${(b & 0xff).toChar}'", bufferBase + p + i)
      value = (value << 4) | digit
      i += 1
    value

  /** re-scans the most recent slice as a Char value; -1 when the content is not exactly one char
    */
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

  protected final def numberStartAbsolute(): Long =
    bufferBase + numStart

  /** True when a number starts at the cursor (after whitespace): scans one JSON number into the
    * number state. False consumes nothing. Malformed numbers throw [[JsonParseException]].
    */
  protected final def tryScanNumber(): Boolean =
    var p = pos
    if p >= limit || !isNumberStart(input(p)) then
      skipWs()
      p = pos
      if p >= limit || !isNumberStart(input(p)) then return false
    // whole-input decodes always take the direct scan; a stream retries on a refilled window
    if scanNumberAt(p, atInputEnd = stream == null) < 0 then scanNumberRetry(p)
    true

  private inline def isNumberStart(b: Byte): Boolean =
    (b >= '0' && b <= '9') || b == '-'

  /** Cold continuation when a number literal touches the window end while more input may exist: a
    * number is a short token, so each refill simply retries the whole scan on the (shifted,
    * extended) window.
    */
  private def scanNumberRetry(startPos: Int): Unit =
    var s        = startPos
    var scanning = true
    while scanning do
      val available = limit - s
      s -= refillKeeping(s)
      if limit - s == available then
        // the window end is the true end of input: the literal may end there
        scanNumberAt(s, atInputEnd = true)
        scanning = false
      else if scanNumberAt(s, atInputEnd = stream == null) >= 0 then scanning = false

  /** the single-window number scan: returns the end position, or -1 when the literal touches the
    * window end while more input may exist (the caller refills and retries)
    */
  private def scanNumberAt(startPos: Int, atInputEnd: Boolean): Int =
    val in  = input
    val lim = limit
    var p   = startPos
    var neg = false
    if in(p) == '-' then
      neg = true
      p += 1
      if p >= lim then
        if !atInputEnd then return -1
        throw JsonParseException("Expected a digit after '-'", bufferBase + p)
      if in(p) < '0' || in(p) > '9' then
        throw JsonParseException("Expected a digit after '-'", bufferBase + p)
    var acc      = 0L
    var digits   = 0
    var fracLen  = 0
    var integral = true
    // integer part: 0 | [1-9][0-9]*
    if in(p) == '0' then
      p += 1
      if p >= lim && !atInputEnd then return -1
      if p < lim && in(p) >= '0' && in(p) <= '9' then
        throw JsonParseException("Leading zeros are not allowed", bufferBase + startPos)
    else
      var b: Byte = 0
      while p < lim && { b = in(p); b >= '0' && b <= '9' } do
        if digits < 18 then acc = acc * 10 - (b - '0')
        digits += 1
        p += 1
      if p >= lim && !atInputEnd then return -1
    // fraction
    if p < lim && in(p) == '.' then
      integral = false
      p += 1
      if p >= lim then
        if !atInputEnd then return -1
        throw JsonParseException("Expected a digit after '.'", bufferBase + p)
      if in(p) < '0' || in(p) > '9' then
        throw JsonParseException("Expected a digit after '.'", bufferBase + p)
      var fb: Byte = 0
      while p < lim && { fb = in(p); fb >= '0' && fb <= '9' } do
        val d = fb - '0'
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
      if p >= lim && !atInputEnd then return -1
    val dirty = digits > 17
    // exponent
    var expValue = 0
    if p < lim && (in(p) == 'e' || in(p) == 'E') then
      integral = false
      p += 1
      if p >= lim && !atInputEnd then return -1
      var expNeg = false
      if p < lim && (in(p) == '+' || in(p) == '-') then
        expNeg = in(p) == '-'
        p += 1
        if p >= lim && !atInputEnd then return -1
      if p >= lim || in(p) < '0' || in(p) > '9' then
        throw JsonParseException("Expected a digit in the exponent", bufferBase + p)
      var eb: Byte = 0
      while p < lim && { eb = in(p); eb >= '0' && eb <= '9' } do
        if expValue < 100000 then expValue = expValue * 10 + (eb - '0')
        p += 1
      if p >= lim && !atInputEnd then return -1
      if expNeg then expValue = -expValue
    numNegAcc = acc
    numDigits = digits
    numIntegral = integral
    numE10 = expValue - fracLen
    numDirty = dirty
    numNeg = neg
    numStart = startPos
    numEnd = p
    pos = p
    p

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
    if p >= limit || isWsByte(input(p)) then
      skipWs()
      p = pos
      if p >= limit then return BooleanNone
    val b = input(p)
    if b == 't' then
      expectLiteral(p, "true")
      BooleanTrue
    else if b == 'f' then
      expectLiteral(p, "false")
      BooleanFalse
    else BooleanNone

  /** consumes `null` (whitespace-skipping, probe-first); false consumes nothing */
  protected final def tryReadNull(): Boolean =
    var p = pos
    if p >= limit || isWsByte(input(p)) then
      skipWs()
      p = pos
      if p >= limit then return false
    if input(p) == 'n' then
      expectLiteral(p, "null")
      true
    else false

  private def expectLiteral(start0: Int, literal: String): Unit =
    var start = start0
    while start + literal.length > limit do
      val available = limit - start
      start -= refillKeeping(start)
      if limit - start == available then
        throw JsonParseException(s"Invalid literal, expected '$literal'", bufferBase + start)
    var i = 1
    while i < literal.length do
      if input(start + i) != literal.charAt(i) then
        throw JsonParseException(s"Invalid literal, expected '$literal'", bufferBase + start)
      i += 1
    pos = start + literal.length

  // --- descriptions and spans (error path only) ---

  protected final def atEof(): Boolean =
    skipWs()
    pos >= limit

  /** the byte starting the next value (whitespace consumed, streams refilled), or -1 at end of
    * input
    */
  protected final def peekByteOrEof(): Int =
    skipWs()
    if pos < limit then input(pos) & 0xff else -1

  /** describes the value starting at the cursor, for ExpectedType errors */
  protected final def describeCurrent(): String =
    (peekByteOrEof(): @annotation.switch) match
      case -1                                                              => "end of input"
      case '{'                                                             => "an object"
      case '['                                                             => "an array"
      case '"'                                                             => "a string"
      case 't' | 'f'                                                       => "a boolean"
      case 'n'                                                             => "null"
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => "a number"
      case other                                                           => s"'${other.toChar}'"

  protected final def currentSpan(): scalanotation.DecodeError.Span =
    spanAt(currentOffset())

  /** Line/column for the absolute `offset`, computed on this cold error path only. The column
    * counts code points, matching the core decoder's byte-input spans. Positions that scrolled out
    * of a streaming buffer report the buffer-start position (best effort).
    */
  protected final def spanAt(offset: Long): scalanotation.DecodeError.Span =
    val local  = math.min(math.max(offset - bufferBase, 0L), limit.toLong).toInt
    var line   = 1 + discardedLines
    var column = discardedColumn
    var i      = 0
    while i < local do
      val b = input(i)
      if b == '\n' then
        line += 1
        column = 1
      else if b >= 0 || (b & 0xc0) == 0xc0 then column += 1
      i += 1
    val absolute = math.min(bufferBase + local, Int.MaxValue.toLong).toInt
    scalanotation.DecodeError.Span(absolute, line, column)
