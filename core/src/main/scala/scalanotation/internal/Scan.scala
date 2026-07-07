package scalanotation.internal

/** Char-level parser combinators that inline to the low-level code they replace.
  *
  * A scan is a chain of combinator calls threading a cursor — a plain `Int` offset into [[chars]],
  * or [[Scan.Failed]] — with each stage bound to a `val`:
  *
  * {{{
  * val from = plainTrivia(index)
  * val name = identEnd(exact(from, expected))
  * if name >= 0 then ...
  * }}}
  *
  * Everything erases at compile time. The combinators are `inline` methods mixed into the scanner
  * (a `this` call needs no inliner proxy), cursor arguments are substituted directly, and predicate
  * parameters are inline function literals that beta-reduce — so a chain compiles to the same
  * straight-line comparisons and while-loops the scanner ops were previously written as by hand,
  * with no closures, no allocation, and near-handcrafted bytecode size (which keeps the scans
  * within the JIT's inlining budgets — the shape that value-class or given-based designs measurably
  * lose).
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

  /** Walks plain whitespace (`' '` and `'\t'..'\r'`) — never fails. Comments, unicode spaces and
    * separator controls stop the walk; combinators never enter the general trivia reader.
    */
  protected inline def plainTrivia(p: Int): Int =
    val text  = chars
    val limit = inputLength
    var i     = p
    while i < limit && { val ch = text(i); ch == ' ' || (ch >= '\t' && ch <= '\r') } do i += 1
    i

  /** consumes exactly `expected`, else fails */
  protected inline def oneChar(p: Int, inline expected: Char): Int =
    if p >= 0 && p < inputLength && chars(p) == expected then p + 1 else Failed

  /** consumes a char satisfying `pred`, else fails */
  protected inline def charWhere(p: Int)(inline pred: Char => Boolean): Int =
    if p >= 0 && p < inputLength && pred(chars(p)) then p + 1 else Failed

  /** Consumes `expected` only when it is a whole operator token — the following char must not
    * extend it (`=` matches, `==`/`=>` do not).
    */
  protected inline def wholeOperator(p: Int, inline expected: Char): Int =
    if p >= 0 && p < inputLength && chars(p) == expected
      && (p + 1 >= inputLength || !operatorPart(chars(p + 1)))
    then p + 1
    else Failed

  /** Consumes every char of the string literal `S`, else fails. The literal unrolls at compile time
    * — type-level string ops, no macros — so `lit["true"](p)` inlines to a single bounds check and
    * four unguarded char compares.
    */
  protected inline def lit[S <: String & Singleton](p: Int): Int =
    val text  = chars
    val limit = inputLength
    if p < 0 || p + constValue[Length[S]] > limit then Failed
    else litChars[S, 0](text, p)

  /** The unrolled char compares of [[lit]] — one compare-or-fail branch per literal char, bounds
    * pre-checked. Threads the cursor rather than a conjunction, so the expansion is a plain branch
    * chain.
    */
  private inline def litChars[S <: String, I <: Int](
      inline text: Array[Char],
      inline at: Int
  ): Int =
    inline if constValue[I] < constValue[Length[S]] then
      if text(at + constValue[I]) == constValue[CharAt[S, I]] then litChars[S, I + 1](text, at)
      else Failed
    else at + constValue[Length[S]]

  /** consumes every char of `expected`, else fails */
  protected inline def exact(p: Int, expected: Array[Char]): Int =
    if p < 0 then p
    else
      val text  = chars
      val limit = inputLength
      val len   = expected.length
      if p + len > limit then Failed
      else
        var i  = 0
        var at = p
        while i < len && text(at) == expected(i) do
          i += 1
          at += 1
        if i == len then at else Failed

  /** fails unless the identifier ends here — the next char must not be an identifier part */
  protected inline def identEnd(p: Int): Int =
    if p < 0 || (p < inputLength && identifierPart(chars(p))) then Failed
    else p

  /** walks chars satisfying `pred` — never fails; inspect the stop char via [[peek]] */
  protected inline def takeWhile(p: Int)(inline pred: Char => Boolean): Int =
    val text  = chars
    val limit = inputLength
    var i     = p
    while i < limit && pred(text(i)) do i += 1
    i

  /** the char under the cursor, or `' '` at the end of input — never consumes */
  protected inline def peek(p: Int): Char =
    if p >= 0 && p < inputLength then chars(p) else ' '

  // Fused compositions. Each is exactly a chain of the primitives above collapsed to one buffer
  // bind: compositions that sit on the hottest scans stay this way so the emitted method remains
  // within the JIT's inlining budgets (a chain of fine-grained ops measurably falls out of them).

  /** [[exact]] then [[identEnd]] — a whole plain-identifier word matching `expected` */
  protected inline def word(p: Int, expected: Array[Char]): Int =
    if p < 0 then p
    else
      val text  = chars
      val limit = inputLength
      val len   = expected.length
      if p + len > limit then Failed
      else
        var i  = 0
        var at = p
        while i < len && text(at) == expected(i) do
          i += 1
          at += 1
        if i == len && (at >= limit || !identifierPart(text(at))) then at
        else Failed

  /** [[lit]] then [[identEnd]] — a whole keyword `S` */
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

  /** [[plainTrivia]] then [[word]] — the leading form of a field header. On success the word spans
    * the returned cursor minus `expected.length` to the cursor.
    */
  protected inline def wordAfterTrivia(p: Int, expected: Array[Char]): Int =
    val text  = chars
    val limit = inputLength
    var i     = p
    while i < limit && { val ch = text(i); ch == ' ' || (ch >= '\t' && ch <= '\r') } do i += 1
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
    while i < limit && { val ch = text(i); ch == ' ' || (ch >= '\t' && ch <= '\r') } do i += 1
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
