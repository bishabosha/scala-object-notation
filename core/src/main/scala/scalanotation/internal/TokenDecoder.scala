package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

import scala.compiletime.uninitialized
import scalanotation.schema.RawSchema

private[scalanotation] object TokenDecoder:

  /** The sole implementation of the opaque [[scalanotation.BatchContext]]: a wrapper over a pool of
    * decoder instances. The pooled contexts amortize decoder construction, which dominates the
    * fixed cost of small decodes; a reentrant decode (e.g. from a user-supplied schema mapping)
    * borrows a second instance instead of corrupting the active one.
    */
  private[scalanotation] enum PoolHolder:
    case RealPoolHolder(pool: PublicInternal.Pool[TokenDecoder])
    case NoPoolHolder

  private def pooled(): TokenDecoder =
    TokenDecoder("", debug = false, slotsPooling = true, scanOnInit = false)

  private def oneShot(input: String, debug: Boolean): TokenDecoder =
    TokenDecoder(input, debug, slotsPooling = false, scanOnInit = true)

  private object PoolDecoderAlloc extends Internal.Alloc[TokenDecoder]:
    def alloc(): TokenDecoder            = TokenDecoder.pooled()
    def prepare(t: TokenDecoder): t.type = t // re-aimed by reset(input, debug) after borrowing

  private[scalanotation] val gcContext: PoolHolder =
    PoolHolder.NoPoolHolder // one-shot decodes allocate a new decoder for each call

  private[scalanotation] def localContext(): PoolHolder =
    given Internal.Alloc[TokenDecoder] = PoolDecoderAlloc
    PoolHolder.RealPoolHolder(Internal.LocalPool[TokenDecoder]())

  private[scalanotation] def sharedContext(capacityHint: Int): PoolHolder =
    given Internal.Alloc[TokenDecoder] = PoolDecoderAlloc
    PoolHolder.RealPoolHolder(Internal.SharedPool[TokenDecoder](capacityHint))

  private inline def withPooled[A](ctx: PoolHolder, input: String, debug: Boolean)(
      inline use: TokenDecoder => A
  ): A =
    def useDecoder(decoder: TokenDecoder): A = use(decoder)
    ctx match
      case PoolHolder.RealPoolHolder(pool) =>
        pool.withBorrowed { decoder =>
          useDecoder(decoder.reset(input, debug))
        }
      case PoolHolder.NoPoolHolder => useDecoder(TokenDecoder.oneShot(input, debug))

  private[scalanotation] def decode[T](
      input: String,
      debugTokens: Boolean,
      rootName: String,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeRoot(decoder, rootName, packageName))

  private[scalanotation] def decodeAnyRoot[T](
      input: String,
      debugTokens: Boolean,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[Expr.SourceFile[T], DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeAnyRoot(decoder, packageName))

  private[scalanotation] def decodeExpression[T](
      input: String,
      debugTokens: Boolean,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeExpression(decoder))

  /** tokens are scanned lazily while decoding, so malformed input can surface anywhere in the
    * decode as a [[TokenizeException]] — converted to a [[DecodeError]] here.
    */
  private inline def catchingTokenErrors[A](input: String)(
      inline body: => Result[A, DecodeError]
  ): Result[A, DecodeError] =
    try body
    catch
      case e: TokenizeException =>
        Result.Err(DecodeError.TokenFormat(e.message).atToken(Tokenizer.spanAt(input, e.offset)))

  private[scalanotation] def isNullable(schema: RawSchema[?]): Boolean =
    schema match
      case RawSchema.Option(_)       => true
      case RawSchema.Mapped(base, _) => isNullable(base)
      case RawSchema.Ref(_, target)  => isNullable(target())
      case _                         => false

/** Reusable buffer for named-tuple parse results: the closing token is recorded as an unboxed Int
  * offset; a Span is only materialized if an error is constructed.
  */
private final class NamedTupleParseResult() {
  var fieldCount: Int          = uninitialized
  var fieldName: String | Null = uninitialized
  var closingOffset: Int       = uninitialized

  def push(fieldCount: Int, fieldName: String | Null, closingOffset: Int): this.type =
    this.fieldCount = fieldCount
    this.fieldName = fieldName
    this.closingOffset = closingOffset
    this
}

private final class TokenDecoder private (
    input: String,
    debug: Boolean,
    private[internal] val slotsPooling: Boolean,
    scanOnInit: Boolean
) extends TokenStream(input, debug, cacheNamesOnInit = slotsPooling, scanOnInit),
      SchemaDecoders {
  def this(input: String, debug: Boolean) =
    this(input, debug, slotsPooling = true, scanOnInit = true)

  protected val namedTupleParseResult: NamedTupleParseResult = new NamedTupleParseResult()

  /** Clears decode state and re-aims at a new input, for reuse via [[TokenDecoder.withPooled]]. */
  def reset(input: String, debug: Boolean): this.type =
    resetStream(input, debug)
    resetSlots()
    namedTupleParseResult.fieldName = null
    this

  def decodeRoot[T](
      schema: Reader[T],
      rootName: String,
      packageName: String
  ): Result[T, DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      expectVal().check
      expectIdentifier().check
      val declaredName = pullStringStrict()
      if declaredName != rootName then raise(DecodeError.UnexpectedRoot(declaredName))
      expectEquals().check
      decodeBase(schema.schema).check
      val value = pullAny()
      expectEof().check
      value.asInstanceOf[T]

  def decodeAnyRoot[T](
      schema: Reader[T],
      packageName: String
  ): Result[Expr.SourceFile[T], DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      expectVal().check
      expectIdentifier().check
      val declaredName = pullStringStrict()
      expectEquals().check
      decodeBase(schema.schema).check
      val value = pullAny().asInstanceOf[T]
      expectEof().check
      Expr.SourceFile(Map(declaredName -> value))

  def decodeExpression[T](schema: Reader[T]): Result[T, DecodeError] =
    Result:
      decodeBase(schema.schema).check
      val value = pullAny()
      expectEof().check
      value.asInstanceOf[T]

}
