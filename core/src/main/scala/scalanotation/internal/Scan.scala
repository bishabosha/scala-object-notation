package scalanotation.internal

/** Char-level parser combinators that inline to the low-level code they replace.
  *
  * A scan is a chain of combinator calls threading a cursor — a plain `Int` offset into [[chars]],
  * or [[Scan.Failed]] — with each stage bound to a `val`:
  *
  * {{{
  * val name = wordAfterTrivia(index, expected)
  * val eq   = operatorAfterTrivia(name, '=')
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
  * Failure is the [[Scan.Failed]] cursor: combinators short-circuit on it, so a chain is written
  * without intermediate checks and inspected once at the end. Nothing commits until the caller
  * writes the final cursor back (`index = p`), which makes backtracking a non-event — abandon the
  * cursor and the input was never touched.
  *
  * Combinators consume plain input only; anything needing the general reader (comments, unicode
  * whitespace, escapes) is expected to fail the chain and divert to the token path, which then sees
  * the identical characters.
  */
private[internal] trait Scan:
  import scala.compiletime.constValue
  import scala.compiletime.ops.int.+
  import scala.compiletime.ops.string.{CharAt, Length}
  import Scan.Failed

  /** the input being scanned — pooled buffers may be longer than the input, so combinators bound
    * every read by [[inputLength]], never by the array length
    */
  private[internal] def chars: Array[Char]

  /** the number of live chars in [[chars]] */
  private[internal] def inputLength: Int

  // Single-hop predicate forwarders: a plain virtual call stays 5 bytes at every inlined use
  // (the JIT sees through it), where inlining the syntax-object access would cost double.

  protected def identifierPart(ch: Char): Boolean = IdentifierSyntax.isIdentifierPart(ch)

  protected def operatorPart(ch: Char): Boolean = IdentifierSyntax.isOperatorPart(ch)

  protected def identifierStart(ch: Char): Boolean = IdentifierSyntax.isIdentifierStart(ch)

  /** The outlined form of [[plainTrivia]] for probe-first miss paths: it runs only when a gap
    * actually exists, and keeping the walk out of the probing ops preserves their JIT inlining
    * budgets.
    */
  protected final def plainGap(p: Int): Int = plainTrivia(p)

  /** Walks plain whitespace (`' '` and `'\t'..'\r'`) — never fails. Comments, unicode spaces and
    * separator controls stop the walk; combinators never enter the general trivia reader.
    */
  protected inline def plainTrivia(p: Int): Int =
    val text  = chars
    val limit = inputLength
    var i     = p
    while i < limit && { val ch = text(i); ch == ' ' || (ch >= '\t' && ch <= '\r') } do i += 1
    i

  /** Consumes `expected` only when it is a whole operator token — the following char must not
    * extend it (`=` matches, `==`/`=>` do not).
    */
  protected inline def wholeOperator(p: Int, inline expected: Char): Int =
    if p >= 0 && p < inputLength && chars(p) == expected
      && (p + 1 >= inputLength || !operatorPart(chars(p + 1)))
    then p + 1
    else Failed

  /** The unrolled char compares of [[keyword]] — one compare-or-fail branch per literal char,
    * bounds pre-checked. Threads the cursor rather than a conjunction, so the expansion is a plain
    * branch chain.
    */
  private inline def litChars[S <: String, I <: Int](
      inline text: Array[Char],
      inline at: Int
  ): Int =
    inline if constValue[I] < constValue[Length[S]] then
      if text(at + constValue[I]) == constValue[CharAt[S, I]] then litChars[S, I + 1](text, at)
      else Failed
    else at + constValue[Length[S]]

  /** Walks chars satisfying `pred` — never fails; inspect the stop char via [[peek]]. The predicate
    * is an inline function literal that beta-reduces into the loop.
    */
  protected inline def takeWhile(p: Int)(inline pred: Char => Boolean): Int =
    val text  = chars
    val limit = inputLength
    var i     = p
    while i < limit && pred(text(i)) do i += 1
    i

  /** Walks one run of the decimal-number grammar: ASCII digits call `onDigit`, and the dirty shapes
    * — a `_` separator or a unicode digit — call `onDirty` with whether it was a separator. Returns
    * the cursor at the first char that continues neither; never fails. The callbacks are inline
    * function literals that beta-reduce into the loop, so accumulation stays at the call site
    * (mutating its locals) while the run's grammar lives here.
    */
  protected inline def digitRun(p: Int)(inline onDigit: Char => Unit)(
      inline onDirty: Boolean => Unit
  ): Int =
    val text    = chars
    val limit   = inputLength
    var i       = p
    var walking = true
    while walking && i < limit do
      val ch = text(i)
      if ch >= '0' && ch <= '9' then
        onDigit(ch)
        i += 1
      else if ch == '_' then
        onDirty(true)
        i += 1
      // '\u007f' == IdentifierSyntax.MaxAscii (inline vals cannot fold in this inline body)
      else if ch > '\u007f' && Character.isDigit(ch) then
        onDirty(false)
        i += 1
      else walking = false
    i

  /** the char under the cursor, or `' '` at the end of input — never consumes */
  protected inline def peek(p: Int): Char =
    if p >= 0 && p < inputLength then chars(p) else ' '

  // Fused compositions. Each is exactly a chain of the primitives above collapsed to one buffer
  // bind: compositions that sit on the hottest scans stay this way so the emitted method remains
  // within the JIT's inlining budgets (a chain of fine-grained ops measurably falls out of them).

  /** A whole keyword `S` from `p`: every char of the literal, then an identifier boundary. The
    * literal unrolls at compile time — type-level string ops, no macros — so `keyword["true"](p)`
    * inlines to a single bounds check, four unguarded char compares and the boundary check.
    */
  protected inline def keyword[S <: String & Singleton](p: Int): Int =
    val text  = chars
    val limit = inputLength
    if p < 0 || p + constValue[Length[S]] > limit then Failed
    else
      val at = litChars[S, 0](text, p)
      if at < 0 || (at < limit && identifierPart(text(at))) then Failed
      else at

  /** A whole plain identifier from `p`: a start char, its part chars, and the trailing
    * '_'-then-operator continuation — exactly what the generic identifier scan consumes.
    */
  protected inline def identifier(p: Int): Int =
    val text  = chars
    val limit = inputLength
    if p < 0 || p >= limit || !identifierStart(text(p)) then Failed
    else
      var i = p + 1
      while i < limit && identifierPart(text(i)) do i += 1
      if text(i - 1) == '_' then while i < limit && operatorPart(text(i)) do i += 1
      i

  /** [[plainTrivia]], then every char of `expected`, then an identifier boundary — the leading form
    * of a field header. On success the word spans the returned cursor minus `expected.length` to
    * the cursor.
    */
  protected inline def wordAfterTrivia(p: Int, expected: Array[Char]): Int =
    val text  = chars
    val limit = inputLength
    var i     = p
    // probe-first: the word's first char is never a trivia char, so a direct hit at the cursor
    // needs no whitespace walk; only a first-char miss walks plain whitespace
    if !(i < limit && text(i) == expected(0)) then i = plainGap(i)
    val len = expected.length
    if i + len > limit then Failed
    else
      var j = 0
      while j < len && text(i) == expected(j) do
        i += 1
        j += 1
      if j == len && (i >= limit || !identifierPart(text(i))) then i
      else Failed

  /** [[plainTrivia]] then [[wholeOperator]] — the operator sits at the returned cursor minus 1 */
  protected inline def operatorAfterTrivia(p: Int, inline expected: Char): Int =
    val text  = chars
    val limit = inputLength
    var i     = p
    if !(i < limit && text(i) == expected) then i = plainGap(i)
    if i < limit && text(i) == expected
      && (i + 1 >= limit || !operatorPart(text(i + 1)))
    then i + 1
    else Failed

  /** A whole escape-free `"` string literal from `p`: the cursor past the closing quote, with the
    * content between `p + 1` and the returned cursor minus one. An escape, a missing opening quote,
    * or the end of input fails — the general string reader sees identical characters.
    */
  protected inline def stringSlice(p: Int): Int =
    val text  = chars
    val limit = inputLength
    if p < 0 || p >= limit || text(p) != '"' then Failed
    else
      var i  = p + 1
      var ch = ' '
      while i < limit && { ch = text(i); ch != '"' && ch != '\\' } do i += 1
      if i >= limit || ch == '\\' then Failed else i + 1

private[internal] object Scan:
  /** the failed cursor: any negative offset, produced and short-circuited by every combinator */
  inline val Failed = -1
