package scalanotation.internal.json

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.internal.Internal
import scalanotation.internal.PublicInternal
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

/** The JSON decoder: one instance per in-flight decode, reusable through the pooled contexts. */
private[scalanotation] final class JsonDecoder private (
    private[internal] val slotsPooling: Boolean
) extends JsonSchemaDecoders:

  /** clears decode state and re-aims at a new String input, for pooled reuse */
  def reset(input: String): this.type =
    resetScannerString(input)
    resetSlots()
    this

  /** [[reset]] over a UTF-8 byte input */
  def resetBytes(input: Array[Byte]): this.type =
    resetScanner(input)
    resetSlots()
    this

  def decodeValue[T](reader: Reader[T]): Result[T, DecodeError] =
    try
      Result:
        decodeBase(reader.schema).check
        val value = pullAny()
        if !atEof() then raise(DecodeError.ExpectedEof(describeCurrent()).atToken(currentSpan()))
        value.asInstanceOf[T]
    catch
      case e: JsonParseException =>
        Result.Err(DecodeError.TokenFormat(e.message).atToken(spanAt(e.offset)))

private[scalanotation] object JsonDecoder:

  /** The sole implementation behind [[scalanotation.json.JsonBatchContext]]: a wrapper over a pool
    * of decoder instances — the JSON mirror of the core TokenDecoder.PoolHolder.
    */
  private[scalanotation] enum PoolHolder:
    case RealPoolHolder(pool: PublicInternal.Pool[JsonDecoder])
    case NoPoolHolder

  private object PoolDecoderAlloc extends Internal.Alloc[JsonDecoder]:
    def alloc(): JsonDecoder            = new JsonDecoder(slotsPooling = true)
    def prepare(t: JsonDecoder): t.type = t // re-aimed by reset after borrowing

  private[scalanotation] val gcContext: PoolHolder =
    PoolHolder.NoPoolHolder // one-shot decodes allocate a new decoder for each call

  private[scalanotation] def localContext(): PoolHolder =
    given Internal.Alloc[JsonDecoder] = PoolDecoderAlloc
    PoolHolder.RealPoolHolder(Internal.LocalPool[JsonDecoder]())

  private[scalanotation] def sharedContext(capacityHint: Int): PoolHolder =
    given Internal.Alloc[JsonDecoder] = PoolDecoderAlloc
    PoolHolder.RealPoolHolder(Internal.SharedPool[JsonDecoder](capacityHint))

  private[scalanotation] def decode[T](
      input: String,
      reader: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    ctx match
      case PoolHolder.RealPoolHolder(pool) =>
        pool.withBorrowed(decoder => decoder.reset(input).decodeValue(reader))
      case PoolHolder.NoPoolHolder =>
        new JsonDecoder(slotsPooling = false).reset(input).decodeValue(reader)

  private[scalanotation] def decodeBytes[T](
      input: Array[Byte],
      reader: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    ctx match
      case PoolHolder.RealPoolHolder(pool) =>
        pool.withBorrowed(decoder => decoder.resetBytes(input).decodeValue(reader))
      case PoolHolder.NoPoolHolder =>
        new JsonDecoder(slotsPooling = false).resetBytes(input).decodeValue(reader)
