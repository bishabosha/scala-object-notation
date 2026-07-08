package scalanotation.internal

/** Byte-level parser combinators that inline to the low-level code they replace.
  *
  * A scan is a chain of combinator calls threading a cursor — a plain `Int` offset into [[bytes]],
  * or [[Scan.Failed]] — with each stage bound to a `val`:
  *
  * {{{
  * val name = identifier(index)
  * val eq   = wholeOperator(name, '=')
  * if eq >= 0 then ...
  * }}}
  *
  * Everything erases at compile time. The combinators are `inline` methods mixed into the scanner
  * (a `this` call needs no inliner proxy), cursor arguments are substituted directly, and string
  * literals unroll through type-level string ops (see [[keyword]] — no macros) — so a chain
  * compiles to the same straight-line comparisons and while-loops the scanner ops were previously
  * written as by hand, with no closures, no allocation, and near-handcrafted bytecode size (which
  * keeps the scans within the JIT's inlining budgets — the shape that value-class or given-based
  * designs measurably lose).
  *
  * The buffer is UTF-8 bytes, scanned in place: a byte input is the caller's array (zero copy), a
  * String input is encoded once into a pooled buffer. The grammar is ASCII, so every structural
  * decision reads one byte and compares against an ASCII literal — a byte from a multi-byte code
  * point is always negative (lead bytes `0xC0..0xFF`, continuations `0x80..0xBF`), so it never
  * equals an ASCII byte and the ASCII tests simply skip over it. Only identifier and unicode-digit
  * classification consults the actual code point, decoded cold from the bytes ([[codePointAt]]).
  *
  * Failure is the [[Scan.Failed]] cursor: combinators short-circuit on it, so a chain is written
  * without intermediate checks and inspected once at the end. Nothing commits until the caller
  * writes the final cursor back (`index = p`), which makes backtracking a non-event — abandon the
  * cursor and the input was never touched.
  *
  * Combinators consume plain input only; anything needing the general reader (comments, unicode
  * whitespace, escapes) is expected to fail the chain and divert to the token path, which then sees
  * the identical bytes.
  */
private[internal] trait Scan:
  import scala.compiletime.constValue
  import scala.compiletime.ops.int.+
  import scala.compiletime.ops.string.{CharAt, Length}
  import Scan.Failed

  /** the input being scanned — pooled buffers (String inputs) may be longer than the input, so
    * combinators bound every read by [[inputLength]], never by the array length
    */
  private[internal] def bytes: Array[Byte]

  /** the number of live bytes in [[bytes]] */
  private[internal] def inputLength: Int

  // Single-hop predicate forwarders: a plain virtual call stays 5 bytes at every inlined use
  // (the JIT sees through it), where inlining the syntax-object access would cost double.

  protected def identifierPart(ch: Char): Boolean = IdentifierSyntax.isIdentifierPart(ch)

  protected def operatorPart(ch: Char): Boolean = IdentifierSyntax.isOperatorPart(ch)

  protected def identifierStart(ch: Char): Boolean = IdentifierSyntax.isIdentifierStart(ch)

  /** the number of bytes in the UTF-8 sequence led by `b`; 1 for an ASCII (non-negative) byte */
  protected inline def utf8Length(b: Byte): Int =
    if b >= 0 then 1
    else
      val u = b & 0xff
      if u < 0xe0 then 2 else if u < 0xf0 then 3 else 4

  /** Decodes the code point at `i` — cold, for identifier and unicode-digit classification only.
    * ASCII bytes are their own code point; multi-byte sequences are assembled from the well-formed
    * UTF-8 the input is (a byte input the caller supplied, or a String re-encoded here), so no
    * validation is repeated. A truncated tail at the buffer end contributes zero bits.
    */
  protected def codePointAt(i: Int): Int =
    val b0 = bytes(i) & 0xff
    if b0 < 0x80 then b0
    else
      val limit                    = inputLength
      inline def cont(k: Int): Int = if i + k < limit then bytes(i + k) & 0x3f else 0
      if b0 < 0xe0 then ((b0 & 0x1f) << 6) | cont(1)
      else if b0 < 0xf0 then ((b0 & 0x0f) << 12) | (cont(1) << 6) | cont(2)
      else ((b0 & 0x07) << 18) | (cont(1) << 12) | (cont(2) << 6) | cont(3)

  private inline def asciiIdentifierStart(b: Byte): Boolean =
    (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || b == '_' || b == '$'

  private inline def asciiIdentifierPart(b: Byte): Boolean =
    (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9')
      || b == '_' || b == '$'

  /** Whether an identifier STARTS at `i` — ASCII fast, else the decoded code point via
    * `Character.isLetter`.
    */
  protected inline def identifierStartsAt(i: Int): Boolean =
    val b = bytes(i)
    if b >= 0 then asciiIdentifierStart(b)
    else Character.isLetter(codePointAt(i))

  /** Whether the identifier CONTINUES at `i` — ASCII fast, else the decoded code point (letter or
    * digit).
    */
  protected inline def identifierContinuesAt(i: Int): Boolean =
    val b = bytes(i)
    if b >= 0 then asciiIdentifierPart(b)
    else
      val cp = codePointAt(i)
      Character.isLetter(cp) || Character.isDigit(cp)

  /** Whether an identifier ENDS at `i` — the boundary check after every matched name. Real input's
    * legal boundary chars (`,`, `)`, space, `=`) sit below 64, so one bit test on the low
    * identifier-part mask answers inline; other ASCII bytes fall to the explicit part check, and a
    * non-ASCII byte (an identifier continues) decodes its code point. Callers ensure `i` is in
    * range.
    */
  protected inline def identifierEndsAt(i: Int): Boolean =
    val b = bytes(i)
    if b >= 0 then
      if b < 64 then ((0x03ff001000000000L >>> b) & 1L) == 0L // bits: digits and '$'
      else !asciiIdentifierPart(b)
    else !identifierContinuesAt(i)

  /** The outlined form of [[plainTrivia]] for probe-first miss paths: it runs only when a gap
    * actually exists, and keeping the walk out of the probing ops preserves their JIT inlining
    * budgets.
    */
  protected final def plainGap(p: Int): Int = plainTrivia(p)

  /** Walks plain whitespace (`' '` and `'\t'..'\r'`) — never fails. Comments, unicode spaces and
    * separator controls stop the walk; combinators never enter the general trivia reader.
    */
  protected inline def plainTrivia(p: Int): Int =
    val text  = bytes
    val limit = inputLength
    var i     = p
    while i < limit && { val b = text(i); b == ' ' || (b >= '\t' && b <= '\r') } do i += 1
    i

  /** Consumes `expected` only when it is a whole operator token — the following byte must not
    * extend it (`=` matches, `==`/`=>` do not).
    */
  protected inline def wholeOperator(p: Int, inline expected: Char): Int =
    if p >= 0 && p < inputLength && bytes(p) == expected
      && (p + 1 >= inputLength || !operatorPartByte(bytes(p + 1)))
    then p + 1
    else Failed

  /** operator-part test on a raw byte: operators are ASCII, so a non-ASCII (negative) byte is never
    * one
    */
  protected inline def operatorPartByte(b: Byte): Boolean =
    b >= 0 && operatorPart(b.toChar)

  /** The unrolled byte compares of [[keyword]] — one compare-or-fail branch per literal char,
    * bounds pre-checked. Threads the cursor rather than a conjunction, so the expansion is a plain
    * branch chain.
    */
  private inline def litChars[S <: String, I <: Int](
      inline text: Array[Byte],
      inline at: Int
  ): Int =
    inline if constValue[I] < constValue[Length[S]] then
      if text(at + constValue[I]) == constValue[CharAt[S, I]] then litChars[S, I + 1](text, at)
      else Failed
    else at + constValue[Length[S]]

  /** Walks bytes satisfying `pred` — never fails; inspect the stop byte via [[peek]]. The predicate
    * is an inline function literal that beta-reduces into the loop. Predicates test ASCII bytes
    * only; a multi-byte code point's bytes are all negative and pass any `!= <ascii>` test, so the
    * walk steps over them byte by byte (its result is a byte slice, materialized via UTF-8 later).
    */
  protected inline def takeWhile(p: Int)(inline pred: Byte => Boolean): Int =
    val text  = bytes
    val limit = inputLength
    var i     = p
    while i < limit && pred(text(i)) do i += 1
    i

  /** Walks one run of the decimal-number grammar: ASCII digits call `onDigit`, and the dirty shapes
    * — a `_` separator or a unicode digit — call `onDirty` with whether it was a separator. Returns
    * the cursor at the first byte that continues neither; never fails. The callbacks are inline
    * function literals that beta-reduce into the loop, so accumulation stays at the call site
    * (mutating its locals) while the run's grammar lives here.
    */
  protected inline def digitRun(p: Int)(inline onDigit: Char => Unit)(
      inline onDirty: Boolean => Unit
  ): Int =
    val text    = bytes
    val limit   = inputLength
    var i       = p
    var walking = true
    while walking && i < limit do
      val b = text(i)
      if b >= '0' && b <= '9' then
        onDigit(b.toChar)
        i += 1
      else if b == '_' then
        onDirty(true)
        i += 1
      else if b < 0 && Character.isDigit(codePointAt(i)) then
        onDirty(false)
        i += utf8Length(b)
      else walking = false
    i

  /** the byte under the cursor, or `' '` at the end of input — never consumes. A non-ASCII byte is
    * returned as-is (negative), so it compares unequal to any ASCII literal.
    */
  protected inline def peek(p: Int): Byte =
    if p >= 0 && p < inputLength then bytes(p) else ' '

  // Fused compositions. Each is exactly a chain of the primitives above collapsed to one buffer
  // bind: compositions that sit on the hottest scans stay this way so the emitted method remains
  // within the JIT's inlining budgets (a chain of fine-grained ops measurably falls out of them).

  /** A whole keyword `S` from `p`: every byte of the literal, then an identifier boundary. The
    * literal unrolls at compile time — type-level string ops, no macros — so `keyword["true"](p)`
    * inlines to a single bounds check, four unguarded byte compares and the boundary check.
    * Keywords are ASCII, so their char length equals their byte length.
    */
  protected inline def keyword[S <: String & Singleton](p: Int): Int =
    val text  = bytes
    val limit = inputLength
    if p < 0 || p + constValue[Length[S]] > limit then Failed
    else
      val at = litChars[S, 0](text, p)
      if at < 0 || (at < limit && !identifierEndsAt(at)) then Failed
      else at

  /** A whole plain identifier from `p`: a start code point, its part code points, and the trailing
    * '_'-then-operator continuation — exactly what the generic identifier scan consumes. ASCII runs
    * advance a byte at a time; a non-ASCII code point advances its full UTF-8 length.
    */
  protected inline def identifier(p: Int): Int =
    val limit = inputLength
    if p < 0 || p >= limit || !identifierStartsAt(p) then Failed
    else
      var i = p + utf8Length(bytes(p))
      while i < limit && identifierContinuesAt(i) do i += utf8Length(bytes(i))
      if bytes(i - 1) == '_' then while i < limit && operatorPartByte(bytes(i)) do i += 1
      i

  /** A whole escape-free `"` string literal from `p`: the cursor past the closing quote, with the
    * content between `p + 1` and the returned cursor minus one. An escape, a missing opening quote,
    * or the end of input fails — the general string reader sees identical bytes. Non-ASCII content
    * bytes are never `"` or `\\`, so the slice spans them and UTF-8 decoding materializes it.
    */
  protected inline def stringSlice(p: Int): Int =
    val text  = bytes
    val limit = inputLength
    if p < 0 || p >= limit || text(p) != '"' then Failed
    else
      var i = p + 1
      var b = ' '.toByte
      while i < limit && { b = text(i); b != '"' && b != '\\' } do i += 1
      if i >= limit || b == '\\' then Failed else i + 1

private[internal] object Scan:
  /** the failed cursor: any negative offset, produced and short-circuited by every combinator */
  inline val Failed = -1
