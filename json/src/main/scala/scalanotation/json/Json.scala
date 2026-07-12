package scalanotation.json

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.TextFormat
import scalanotation.Writer
import scalanotation.internal.json.JsonDecoder
import scalanotation.internal.json.JsonEncode
import steps.result.Result

import java.nio.charset.StandardCharsets

/** JSON reading and writing over the scala-object-notation type classes: any [[Reader]],
  * [[Writer]], or [[scalanotation.ReadWriter]] — derived or hand-built — decodes from and encodes
  * to JSON through the same [[scalanotation.schema.RawSchema]] the core notation uses.
  */
object Json:

  /** Reads a value from JSON text. */
  def readAs[T: Reader as reader](input: String): Result[T, DecodeError] =
    JsonDecoder.decode(input, reader)(using JsonDecoder.gcContext)

  /** Reads a value from UTF-8 encoded JSON bytes — the decoder's native input mode. */
  def readAs[T: Reader as reader](input: Array[Byte]): Result[T, DecodeError] =
    JsonDecoder.decodeBytes(input, reader)(using JsonDecoder.gcContext)

  /** Variants of the read methods that reuse decoder machinery across calls, as configured by the
    * given [[JsonBatchContext]] — see its factories for the pooling options. Results are identical
    * to the plain methods; only the allocation behaviour differs.
    */
  object batched:
    def readAs[T](input: String)(
        using reader: Reader[T],
        ctx: JsonBatchContext
    ): Result[T, DecodeError] =
      JsonDecoder.decode(input, reader)(using ctx.holder)

    def readAs[T](input: Array[Byte])(
        using reader: Reader[T],
        ctx: JsonBatchContext
    ): Result[T, DecodeError] =
      JsonDecoder.decodeBytes(input, reader)(using ctx.holder)

  /** The default output format: standard compact JSON — no whitespace. */
  val compact: TextFormat = TextFormat.compact(spacing = 0)

  def write[T: Writer as writer](value: T, format: TextFormat = compact): String =
    val out = JsonEncode.Output()
    JsonEncode.renderText(writer.schema, value, out, 0)(using format)
    out.result()

  def writePretty[T: Writer](value: T, indent: Int = 2, spacing: Int = 1): String =
    write(value, TextFormat.pretty(indent, spacing))

  def writeBytes[T: Writer](value: T, format: TextFormat = compact): Array[Byte] =
    write(value, format).getBytes(StandardCharsets.UTF_8)
