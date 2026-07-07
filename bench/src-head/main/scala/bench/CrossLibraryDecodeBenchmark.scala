package bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import zio.blocks.schema.Schema
import zio.blocks.schema.json.JsonFormat

import scalanotation.BatchContext
import scalanotation.Configured
import scalanotation.Reader
import scalanotation.Readers
import scalanotation.TypedFactory
import scalanotation.macros.TypedFactories

case class OrderRecord(id: Long, sku: String, qty: Int, price: Double, active: Boolean)
object OrderRecord:
  given TypedFactory[OrderRecord] = TypedFactories.derived
  given Configured[OrderRecord]   = Configured.typed
  given Reader[OrderRecord]       = Reader.configured.derived

case class OrderBatch(orders: Vector[OrderRecord])
object OrderBatch:
  given TypedFactory[OrderBatch] = TypedFactories.derived
  given Configured[OrderBatch]   = Configured.typed
  given Reader[OrderBatch]       = Reader.configured.derived

/** Cross-library comparison: SON typed batched decoding against jsoniter-scala, uPickle, and
  * zio-blocks decoding the equivalent JSON. SON reads a String; the JSON libraries are measured
  * both from a String (input parity) and — where supported — from a byte array (their fastest
  * entry point). Codecs, schemas, and readers are built once per fork so every side measures
  * decoding only.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class CrossLibraryDecodeBenchmark:
  // --- flat record: 3 fields, ints outside the Integer cache ---
  private val sonFlatInput  = """(x = 1234, y = -56789, label = "hello")"""
  private val jsonFlatInput = """{"x":1234,"y":-56789,"label":"hello"}"""
  private val jsonFlatBytes = jsonFlatInput.getBytes(StandardCharsets.UTF_8)

  // --- 10 primitive fields ---
  private val sonPrimitive10Input =
    """(f1 = 1001, f2 = -2002, f3 = 3003, f4 = -4004, f5 = 5005, f6 = -6006, f7 = 7007, f8 = -8008, f9 = 9009, f10 = -10010)"""
  private val jsonPrimitive10Input =
    """{"f1":1001,"f2":-2002,"f3":3003,"f4":-4004,"f5":5005,"f6":-6006,"f7":7007,"f8":-8008,"f9":9009,"f10":-10010}"""
  private val jsonPrimitive10Bytes = jsonPrimitive10Input.getBytes(StandardCharsets.UTF_8)

  // --- batch of 100 mixed-field records ---
  private def sonOrder(i: Int): String =
    s"""(id = ${i * 1000L}, sku = "sku-$i", qty = ${i % 10 + 1}, price = ${i * 100}.99, active = ${i % 2 == 0})"""
  private def jsonOrder(i: Int): String =
    s"""{"id":${i * 1000L},"sku":"sku-$i","qty":${i % 10 + 1},"price":${i * 100}.99,"active":${i % 2 == 0}}"""

  private val sonOrdersInput =
    s"(orders = Vector(${(1 to 100).map(sonOrder).mkString(", ")}))"
  private val jsonOrdersInput =
    s"""{"orders":[${(1 to 100).map(jsonOrder).mkString(",")}]}"""
  private val jsonOrdersBytes = jsonOrdersInput.getBytes(StandardCharsets.UTF_8)

  // codecs, schemas, and readers derived once — every side measures pure decoding
  private given JsonValueCodec[TypedFlatClass]        = JsonCodecMaker.make
  private given JsonValueCodec[TypedPrimitive10Class] = JsonCodecMaker.make
  private given JsonValueCodec[OrderBatch]            = JsonCodecMaker.make

  private given upickle.default.ReadWriter[TypedFlatClass]        = upickle.default.macroRW
  private given upickle.default.ReadWriter[TypedPrimitive10Class] = upickle.default.macroRW
  private given upickle.default.ReadWriter[OrderRecord]           = upickle.default.macroRW
  private given upickle.default.ReadWriter[OrderBatch]            = upickle.default.macroRW

  private given Schema[OrderRecord] = Schema.derived

  private val zioFlatCodec        = Schema.derived[TypedFlatClass].derive(JsonFormat)
  private val zioPrimitive10Codec = Schema.derived[TypedPrimitive10Class].derive(JsonFormat)
  private val zioOrdersCodec      = Schema.derived[OrderBatch].derive(JsonFormat)

  // benchmarks are single-threaded per State(Scope.Thread), so the local context is safe
  private given ctx: BatchContext = BatchContext.local()

  @Benchmark def sonFlat: Any =
    Readers.batched.readAs[TypedFlatClass](sonFlatInput)
  @Benchmark def jsoniterFlatString: Any =
    readFromString[TypedFlatClass](jsonFlatInput)
  @Benchmark def jsoniterFlatBytes: Any =
    readFromArray[TypedFlatClass](jsonFlatBytes)
  @Benchmark def upickleFlatString: Any =
    upickle.default.read[TypedFlatClass](jsonFlatInput)
  @Benchmark def zioBlocksFlatString: Any =
    zioFlatCodec.decode(jsonFlatInput)
  @Benchmark def zioBlocksFlatBytes: Any =
    zioFlatCodec.decode(jsonFlatBytes)

  @Benchmark def sonPrimitive10: Any =
    Readers.batched.readAs[TypedPrimitive10Class](sonPrimitive10Input)
  @Benchmark def jsoniterPrimitive10String: Any =
    readFromString[TypedPrimitive10Class](jsonPrimitive10Input)
  @Benchmark def jsoniterPrimitive10Bytes: Any =
    readFromArray[TypedPrimitive10Class](jsonPrimitive10Bytes)
  @Benchmark def upicklePrimitive10String: Any =
    upickle.default.read[TypedPrimitive10Class](jsonPrimitive10Input)
  @Benchmark def zioBlocksPrimitive10String: Any =
    zioPrimitive10Codec.decode(jsonPrimitive10Input)
  @Benchmark def zioBlocksPrimitive10Bytes: Any =
    zioPrimitive10Codec.decode(jsonPrimitive10Bytes)

  // compact SON rendering (no spaces), comparable in density to the compact JSON inputs
  private val sonFlatCompactInput      = sonFlatInput.replace(" = ", "=").replace(", ", ",")
  private val sonPrimitive10CompactInput =
    sonPrimitive10Input.replace(" = ", "=").replace(", ", ",")
  private val sonOrdersCompactInput = sonOrdersInput.replace(" = ", "=").replace(", ", ",")

  @Benchmark def sonFlatCompact: Any =
    Readers.batched.readAs[TypedFlatClass](sonFlatCompactInput)
  @Benchmark def sonPrimitive10Compact: Any =
    Readers.batched.readAs[TypedPrimitive10Class](sonPrimitive10CompactInput)
  @Benchmark def sonOrders100Compact: Any =
    Readers.batched.readAs[OrderBatch](sonOrdersCompactInput)

  @Benchmark def sonOrders100: Any =
    Readers.batched.readAs[OrderBatch](sonOrdersInput)
  @Benchmark def jsoniterOrders100String: Any =
    readFromString[OrderBatch](jsonOrdersInput)
  @Benchmark def jsoniterOrders100Bytes: Any =
    readFromArray[OrderBatch](jsonOrdersBytes)
  @Benchmark def upickleOrders100String: Any =
    upickle.default.read[OrderBatch](jsonOrdersInput)
  @Benchmark def zioBlocksOrders100String: Any =
    zioOrdersCodec.decode(jsonOrdersInput)
  @Benchmark def zioBlocksOrders100Bytes: Any =
    zioOrdersCodec.decode(jsonOrdersBytes)
