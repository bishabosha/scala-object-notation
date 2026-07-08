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

  private def oneShotBytes(input: Array[Byte], debug: Boolean): TokenDecoder =
    val decoder = TokenDecoder("", debug, slotsPooling = false, scanOnInit = false)
    decoder.resetBytes(input, debug)

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

  private inline def withPooledBytes[A](ctx: PoolHolder, input: Array[Byte], debug: Boolean)(
      inline use: TokenDecoder => A
  ): A =
    def useDecoder(decoder: TokenDecoder): A = use(decoder)
    ctx match
      case PoolHolder.RealPoolHolder(pool) =>
        pool.withBorrowed { decoder =>
          useDecoder(decoder.resetBytes(input, debug))
        }
      case PoolHolder.NoPoolHolder => useDecoder(TokenDecoder.oneShotBytes(input, debug))

  private[scalanotation] def decode[T](
      input: String,
      debugTokens: Boolean,
      rootName: String,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeRoot(decoder, rootName, packageName))

  private[scalanotation] def decodeExperimental[T](
      input: String,
      debugTokens: Boolean,
      rootName: String,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(
        _.decodeExperimentalRoot(decoder, rootName, packageName)
      )

  private[scalanotation] def decodeAnyRoot[T](
      input: String,
      debugTokens: Boolean,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[Expr.SourceFile[T], DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeAnyRoot(decoder, packageName))

  private[scalanotation] def decodeExperimentalAnyRoot[T](
      input: String,
      debugTokens: Boolean,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[Expr.SourceFile[T], DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeExperimentalAnyRoot(decoder, packageName))

  private[scalanotation] def decodeExpression[T](
      input: String,
      debugTokens: Boolean,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeExpression(decoder))

  private[scalanotation] def decodeExpressionBytes[T](
      input: Array[Byte],
      debugTokens: Boolean,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrorsBytes(input):
      withPooledBytes(ctx, input, debugTokens)(_.decodeExpression(decoder))

  private[scalanotation] def decodeExperimentalExpression[T](
      input: String,
      debugTokens: Boolean,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeExperimentalExpression(decoder))

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

  /** [[catchingTokenErrors]] for byte inputs: the error offset is a byte offset into the input, so
    * the span is computed over the bytes directly (no decode)
    */
  private inline def catchingTokenErrorsBytes[A](input: Array[Byte])(
      inline body: => Result[A, DecodeError]
  ): Result[A, DecodeError] =
    try body
    catch
      case e: TokenizeException =>
        Result.Err(
          DecodeError
            .TokenFormat(e.message)
            .atToken(Tokenizer.spanAt(input, input.length, e.offset))
        )

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
) extends TokenStream(input, debug, scanOnInit),
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

  /** [[reset]] over a UTF-8 byte input — see [[Tokenizer.resetBytes]]. */
  def resetBytes(input: Array[Byte], debug: Boolean): this.type =
    resetStreamBytes(input, debug)
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

  def decodeExperimentalRoot[T](
      schema: Reader[T],
      rootName: String,
      packageName: String
  ): Result[T, DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      acceptExperimentalImports().check
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

  def decodeExperimentalAnyRoot[T](
      schema: Reader[T],
      packageName: String
  ): Result[Expr.SourceFile[T], DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      acceptExperimentalImports().check
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

  def decodeExperimentalExpression[T](schema: Reader[T]): Result[T, DecodeError] =
    Result:
      acceptExperimentalImports().check
      decodeBase(schema.schema).check
      val value = pullAny()
      expectEof().check
      value.asInstanceOf[T]

  private def acceptExperimentalImports(): Result[Unit, DecodeError] = Result.task:
    var done = false
    while !done do
      currentKind() match
        case TokenKind.Keyword if currentNameMatches("import") =>
          val importOffset = currentOffset()
          advance()
          acceptExperimentalImport(importOffset).check
          acceptStatementSeparator()
        case _ =>
          done = true

  private def acceptExperimentalImport(
      importOffset: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      acceptImportIdentifier("language", importOffset).check
      expectImportDot(importOffset).check
      acceptImportIdentifier("experimental", importOffset).check
      expectImportDot(importOffset).check
      if currentKind() == TokenKind.LBrace then acceptGroupedExperimentalImport(importOffset).check
      else acceptExperimentalImportSelector(importOffset).check
      rejectImportSelectorTail(importOffset).check

  private def acceptGroupedExperimentalImport(
      importOffset: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      advance()
      var done = false
      while !done do
        if currentKind() == TokenKind.RBrace then
          raise(DecodeError.ExpectedIdentifier(describeCurrent()).atToken(currentSpan()))
        acceptExperimentalImportSelector(importOffset).check
        currentKind() match
          case TokenKind.Comma =>
            advance()
            if currentKind() == TokenKind.RBrace then done = true
          case TokenKind.RBrace =>
            done = true
          case _ =>
            raise(unsupportedExperimentalImport(importOffset))
      advance()

  private def acceptExperimentalImportSelector(
      importOffset: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      currentKind() match
        case TokenKind.Identifier if currentNameMatches("dedentedStringLiterals") =>
          addExperimentalFlags(ExperimentalFlags.AllowSIP72)
          advance()
        case TokenKind.Identifier if currentNameMatches("collectionLiterals") =>
          addExperimentalFlags(ExperimentalFlags.AllowCollectionLiterals)
          advance()
        case TokenKind.Identifier =>
          raise(unsupportedExperimentalImport(importOffset))
        case _ =>
          raise(DecodeError.ExpectedIdentifier(describeCurrent()).atToken(currentSpan()))

  private def rejectImportSelectorTail(importOffset: Int): Result[Unit, DecodeError] =
    Result.task:
      currentKind() match
        case TokenKind.Dot | TokenKind.Comma =>
          raise(unsupportedExperimentalImport(importOffset))
        case _ => ()

  private def acceptImportIdentifier(
      expected: String,
      importOffset: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      currentKind() match
        case TokenKind.Identifier if currentNameMatches(expected) =>
          advance()
        case TokenKind.Identifier =>
          raise(unsupportedExperimentalImport(importOffset))
        case _ =>
          raise(DecodeError.ExpectedIdentifier(describeCurrent()).atToken(currentSpan()))

  private def expectImportDot(importOffset: Int): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.Dot then advance()
      else raise(unsupportedExperimentalImport(importOffset))

  private def unsupportedExperimentalImport(importOffset: Int): DecodeError =
    DecodeError
      .Custom(
        "Unsupported experimental import; only import language.experimental.dedentedStringLiterals and import language.experimental.collectionLiterals are supported"
      )
      .atToken(spanAt(importOffset))
}
