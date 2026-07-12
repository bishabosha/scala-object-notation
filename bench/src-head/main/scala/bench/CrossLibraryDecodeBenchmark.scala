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
import scalanotation.DefaultValues
import scalanotation.Reader
import scalanotation.Readers
import scalanotation.TypedFactory
import scalanotation.macros.Defaults
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

// config-like skippable record: optional fields interleave the required ones, so inputs that
// omit them exercise the record loop's slow name resolver on every following field
case class SparseRecord(
    id: Long,
    note: Option[String] = None,
    factor: Option[Double] = None,
    sku: String = "",
    retries: Option[Int] = None,
    qty: Int = 0,
    flag: Option[Boolean] = None,
    price: Double = 0.0,
    alias: Option[String] = None,
    active: Boolean = false
)
object SparseRecord:
  given TypedFactory[SparseRecord] = TypedFactories.derived
  given Configured[SparseRecord]   = Configured.skippable
  given Reader[SparseRecord]       = Reader.configured.derived

case class SparseBatch(records: Vector[SparseRecord])
object SparseBatch:
  given TypedFactory[SparseBatch] = TypedFactories.derived
  given Configured[SparseBatch]   = Configured.typed
  given Reader[SparseBatch]       = Reader.configured.derived

// The sparse shape with default arguments instead of Options — the SAME input payload decodes
// through defaults fills rather than None fills. Every library gathers the constructor defaults
// natively (SON Defaults.derived, jsoniter codec maker, uPickle macroRW, zio-blocks Schema
// derivation), so all sides fill the five omitted fields per record.
case class DefaultedRecord(
    id: Long,
    note: String = "n/a",
    factor: Double = 1.5,
    sku: String = "",
    retries: Int = 3,
    qty: Int = 0,
    flag: Boolean = true,
    price: Double = 0.0,
    alias: String = "anon",
    active: Boolean = false
)
object DefaultedRecord:
  given TypedFactory[DefaultedRecord]  = TypedFactories.derived
  given DefaultValues[DefaultedRecord] = Defaults.derived
  given Configured[DefaultedRecord]    = Configured.typed.withDefaultValues
  given Reader[DefaultedRecord]        = Reader.configured.derived

case class DefaultedBatch(records: Vector[DefaultedRecord])
object DefaultedBatch:
  given TypedFactory[DefaultedBatch] = TypedFactories.derived
  given Configured[DefaultedBatch]   = Configured.typed
  given Reader[DefaultedBatch]       = Reader.configured.derived

// mixed-case enum batch — each library decodes its own default sum encoding, with SON in its
// typed-factory configuration (like OrderRecord)
enum Shape:
  case Circle(radius: Double)
  case Rect(width: Double, height: Double)
  case Label(text: String)
object Shape:
  given TypedFactory[Shape]             = TypedFactories.derived
  given Configured[Shape]               = Configured.default[Shape].withTypedFactories
  given scalanotation.ReadWriter[Shape] = scalanotation.ReadWriter.configured.derived

case class ShapeBatch(shapes: Vector[Shape])
object ShapeBatch:
  given TypedFactory[ShapeBatch]             = TypedFactories.derived
  given Configured[ShapeBatch]               = Configured.typed
  given scalanotation.ReadWriter[ShapeBatch] = scalanotation.ReadWriter.configured.derived

// The same shapes under a discriminator-field encoding — SON's counterpart of jsoniter's and
// uPickle's default ADT encodings, decoded through the partial named-tuple path.
enum ShapeK:
  case Circle(radius: Double)
  case Rect(width: Double, height: Double)
  case Label(text: String)
object ShapeK:
  given TypedFactory[ShapeK] = TypedFactories.derived
  given Configured[ShapeK]   = Configured.discriminator[ShapeK]("kind").withTypedFactories
  given scalanotation.ReadWriter[ShapeK] = scalanotation.ReadWriter.configured.derived

case class ShapeKBatch(shapes: Vector[ShapeK])
object ShapeKBatch:
  given TypedFactory[ShapeKBatch]             = TypedFactories.derived
  given Configured[ShapeKBatch]               = Configured.typed
  given scalanotation.ReadWriter[ShapeKBatch] = scalanotation.ReadWriter.configured.derived

/** Cross-library comparison: SON typed batched decoding against jsoniter-scala, uPickle, and
  * zio-blocks decoding the equivalent JSON. SON reads a String; the JSON libraries are measured
  * both from a String (input parity) and — where supported — from a byte array (their fastest entry
  * point). Codecs, schemas, and readers are built once per fork so every side measures decoding
  * only.
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

  // The same order batch with each record's fields in a seeded-random order — JSON objects are
  // unordered, so this exercises every library's out-of-schema-order field resolution on
  // identical payloads. The reversed variant is the worst case for expected-order probing.
  private def jsonOrderFields(i: Int): List[String] =
    List(
      s""""id":${i * 1000L}""",
      s""""sku":"sku-$i"""",
      s""""qty":${i % 10 + 1}""",
      s""""price":${i * 100}.99""",
      s""""active":${i % 2 == 0}"""
    )
  private def jsonOrderShuffled(i: Int): String =
    new scala.util.Random(i * 31337).shuffle(jsonOrderFields(i)).mkString("{", ",", "}")
  private def jsonOrderReversed(i: Int): String =
    jsonOrderFields(i).reverse.mkString("{", ",", "}")

  private val jsonOrdersShuffledInput =
    s"""{"orders":[${(1 to 100).map(jsonOrderShuffled).mkString(",")}]}"""
  private val jsonOrdersShuffledBytes = jsonOrdersShuffledInput.getBytes(StandardCharsets.UTF_8)
  private val jsonOrdersReversedInput =
    s"""{"orders":[${(1 to 100).map(jsonOrderReversed).mkString(",")}]}"""
  private val jsonOrdersReversedBytes = jsonOrdersReversedInput.getBytes(StandardCharsets.UTF_8)

  // Each record carries its own seeded-random subset of the five optional/defaulted extras —
  // real sparse data omits fields at random, so no two records share a skip pattern. Every extra
  // draws independently AND with its own frequency (near-always, common, occasional, rare), so
  // the arriving-field patterns span many modes rather than one uniform coin. The extras sit at
  // their schema positions (what a writer would emit), and the same values serve the
  // Option-based sparse schema (bare values decode as Some) and the defaults schema alike.
  private def sparseFieldValues(i: Int): List[(String, String)] =
    val random          = new scala.util.Random(i * 7919)
    def keep(p: Double) = random.nextDouble() < p
    List(
      Some("id"                        -> s"${i * 1000L}"),
      Option.when(keep(0.8))("note"    -> s""""note-$i""""),
      Option.when(keep(0.15))("factor" -> s"${i % 7}.25"),
      Some("sku"                       -> s""""sku-$i""""),
      Option.when(keep(0.5))("retries" -> s"${i % 5}"),
      Some("qty"                       -> s"${i % 10 + 1}"),
      Option.when(keep(0.3))("flag"    -> s"${i % 3 == 0}"),
      Some("price"                     -> s"${i * 100}.99"),
      Option.when(keep(0.65))("alias"  -> s""""alias-$i""""),
      Some("active"                    -> s"${i % 2 == 0}")
    ).flatten

  private def sparseRecord(i: Int): String =
    sparseFieldValues(i).map((name, value) => s"$name = $value").mkString("(", ", ", ")")
  private val sonSparseInput =
    s"(records = Vector(${(1 to 100).map(sparseRecord).mkString(", ")}))"
  private def jsonSparse(i: Int): String =
    sparseFieldValues(i).map((name, value) => s"\"$name\":$value").mkString("{", ",", "}")
  private val jsonSparseInput =
    s"""{"records":[${(1 to 100).map(jsonSparse).mkString(",")}]}"""

  // --- batch of 100 mixed enum cases; each library decodes its own default sum encoding ---
  private val shapeBatch = ShapeBatch(
    (1 to 100).toVector.map { i =>
      (i % 3: @unchecked) match
        case 0 => Shape.Circle(i * 1.5)
        case 1 => Shape.Rect(i * 2.0, i * 3.0)
        case 2 => Shape.Label(s"shape-$i")
    }
  )

  private val shapeKBatch = ShapeKBatch(
    (1 to 100).toVector.map { i =>
      (i % 3: @unchecked) match
        case 0 => ShapeK.Circle(i * 1.5)
        case 1 => ShapeK.Rect(i * 2.0, i * 3.0)
        case 2 => ShapeK.Label(s"shape-$i")
    }
  )

  // codecs, schemas, and readers derived once — every side measures pure decoding
  private given JsonValueCodec[TypedFlatClass]        = JsonCodecMaker.make
  private given JsonValueCodec[TypedPrimitive10Class] = JsonCodecMaker.make
  private given JsonValueCodec[OrderBatch]            = JsonCodecMaker.make

  private given upickle.default.ReadWriter[TypedFlatClass]        = upickle.default.macroRW
  private given upickle.default.ReadWriter[TypedPrimitive10Class] = upickle.default.macroRW
  private given upickle.default.ReadWriter[OrderRecord]           = upickle.default.macroRW
  private given upickle.default.ReadWriter[OrderBatch]            = upickle.default.macroRW
  private given upickle.default.ReadWriter[SparseRecord]          = upickle.default.macroRW
  private given upickle.default.ReadWriter[SparseBatch]           = upickle.default.macroRW

  private given Schema[OrderRecord] = Schema.derived
  private given Schema[Shape]       = Schema.derived

  private given JsonValueCodec[ShapeBatch]             = JsonCodecMaker.make
  private given upickle.default.ReadWriter[Shape]      = upickle.default.ReadWriter.derived
  private given upickle.default.ReadWriter[ShapeBatch] = upickle.default.macroRW
  private val zioShapesCodec = Schema.derived[ShapeBatch].derive(JsonFormat)

  // inputs produced by each library's own writer, so every side decodes its idiomatic format
  private val sonShapesInput       = scalanotation.Writers.write(shapeBatch)
  private val jsonShapesInput      = writeToString(shapeBatch)
  private val upickleShapesInput   = upickle.default.write(shapeBatch)
  private val zioShapesInput       = zioShapesCodec.encode(shapeBatch)
  private val sonShapesBytesInput  = sonShapesInput.getBytes(StandardCharsets.UTF_8)
  private val sonShapesKInput      = scalanotation.Writers.write(shapeKBatch)
  private val sonShapesKBytesInput = sonShapesKInput.getBytes(StandardCharsets.UTF_8)
  private val jsonShapesBytesInput = jsonShapesInput.getBytes(StandardCharsets.UTF_8)

  private val zioFlatCodec           = Schema.derived[TypedFlatClass].derive(JsonFormat)
  private val zioPrimitive10Codec    = Schema.derived[TypedPrimitive10Class].derive(JsonFormat)
  private val zioOrdersCodec         = Schema.derived[OrderBatch].derive(JsonFormat)
  private given Schema[SparseRecord] = Schema.derived
  private val zioSparseCodec         = Schema.derived[SparseBatch].derive(JsonFormat)
  private given JsonValueCodec[SparseBatch] = JsonCodecMaker.make

  private given Schema[DefaultedRecord]        = Schema.derived
  private val zioDefaultedCodec                = Schema.derived[DefaultedBatch].derive(JsonFormat)
  private given JsonValueCodec[DefaultedBatch] = JsonCodecMaker.make
  private given upickle.default.ReadWriter[DefaultedRecord] = upickle.default.macroRW
  private given upickle.default.ReadWriter[DefaultedBatch]  = upickle.default.macroRW

  // benchmarks are single-threaded per State(Scope.Thread), so the local contexts are safe
  private given ctx: BatchContext                     = BatchContext.local()
  private given jsonCtx: scalanotation.json.JsonBatchContext =
    scalanotation.json.JsonBatchContext.local()

  // the JSON-module shape inputs are produced by its own writer, like every other library's
  private val sonJsonShapesInput       = scalanotation.json.Json.write(shapeBatch)
  private val sonJsonShapesBytesInput  = sonJsonShapesInput.getBytes(StandardCharsets.UTF_8)
  private val sonJsonShapesKInput      = scalanotation.json.Json.write(shapeKBatch)
  private val sonJsonShapesKBytesInput = sonJsonShapesKInput.getBytes(StandardCharsets.UTF_8)
  private val jsonSparseBytes          = jsonSparseInput.getBytes(StandardCharsets.UTF_8)

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

  // Compact SON rendering (minimal whitespace), comparable in density to the compact JSON
  // inputs. `=` directly followed by `-` would scan as one operator token (Scala tokenization),
  // so a single space stays in front of negative literals — the most compact valid form.
  // batched like every other SON benchmark — the one-shot reader allocates a fresh scanner per
  // call, which once masqueraded as a decode-loop allocation gap
  @Benchmark def sonSparse100: Any =
    Readers.batched.readAs[SparseBatch](sonSparseInput)
  @Benchmark def jsoniterSparse100String: Any =
    readFromString[SparseBatch](jsonSparseInput)
  @Benchmark def upickleSparse100String: Any =
    upickle.default.read[SparseBatch](jsonSparseInput)
  @Benchmark def zioBlocksSparse100String: Any =
    zioSparseCodec.decode(jsonSparseInput)

  // the same payload as the sparse benchmarks, decoded through the defaults-filling schemas —
  // every record fills note/factor/retries/flag/alias from its constructor defaults
  @Benchmark def sonDefaulted100: Any =
    Readers.batched.readAs[DefaultedBatch](sonSparseInput)
  @Benchmark def jsoniterDefaulted100String: Any =
    readFromString[DefaultedBatch](jsonSparseInput)
  @Benchmark def upickleDefaulted100String: Any =
    upickle.default.read[DefaultedBatch](jsonSparseInput)
  @Benchmark def zioBlocksDefaulted100String: Any =
    zioDefaultedCodec.decode(jsonSparseInput)

  private def compactSon(son: String): String =
    son.replace(" = ", "=").replace(", ", ",").replace("=-", "= -")

  private val sonFlatCompactInput        = compactSon(sonFlatInput)
  private val sonPrimitive10CompactInput = compactSon(sonPrimitive10Input)
  private val sonOrdersCompactInput      = compactSon(sonOrdersInput)

  // UTF-8 byte forms of the SON inputs — apples-to-apples with the jsoniter/zio byte benchmarks
  private val sonFlatBytesInput          = sonFlatInput.getBytes(StandardCharsets.UTF_8)
  private val sonPrimitive10BytesInput   = sonPrimitive10Input.getBytes(StandardCharsets.UTF_8)
  private val sonOrdersBytesInput        = sonOrdersInput.getBytes(StandardCharsets.UTF_8)
  private val sonOrdersCompactBytesInput =
    sonOrdersCompactInput.getBytes(StandardCharsets.UTF_8)

  // one-time guard: a benchmark over inputs that fail to decode measures fail-fast errors and
  // reports meaningless (fast) numbers — every SON input must round-trip Ok
  locally {
    def requireOk(name: String, result: Any): Unit = result match
      case steps.result.Result.Err(error) =>
        throw new IllegalStateException(s"benchmark input $name does not decode: $error")
      case _ => ()
    requireOk("sonFlat", Readers.batched.readAs[TypedFlatClass](sonFlatInput))
    requireOk("sonFlatCompact", Readers.batched.readAs[TypedFlatClass](sonFlatCompactInput))
    requireOk("sonPrimitive10", Readers.batched.readAs[TypedPrimitive10Class](sonPrimitive10Input))
    requireOk(
      "sonPrimitive10Compact",
      Readers.batched.readAs[TypedPrimitive10Class](sonPrimitive10CompactInput)
    )
    requireOk("sonOrders100", Readers.batched.readAs[OrderBatch](sonOrdersInput))
    requireOk("sonOrders100Compact", Readers.batched.readAs[OrderBatch](sonOrdersCompactInput))
    requireOk("sonFlatBytes", Readers.batched.readAs[TypedFlatClass](sonFlatBytesInput))
    requireOk(
      "sonPrimitive10Bytes",
      Readers.batched.readAs[TypedPrimitive10Class](sonPrimitive10BytesInput)
    )
    requireOk("sonOrders100Bytes", Readers.batched.readAs[OrderBatch](sonOrdersBytesInput))
    requireOk(
      "sonOrders100CompactBytes",
      Readers.batched.readAs[OrderBatch](sonOrdersCompactBytesInput)
    )
    requireOk("sonShapes100", Readers.batched.readAs[ShapeBatch](sonShapesInput))
    requireOk("sonShapes100Bytes", Readers.batched.readAs[ShapeBatch](sonShapesBytesInput))
    requireOk("sonShapesDisc100", Readers.batched.readAs[ShapeKBatch](sonShapesKInput))
    requireOk("sonShapesDisc100Bytes", Readers.batched.readAs[ShapeKBatch](sonShapesKBytesInput))

    requireOk("sonSparse100", Readers.batched.readAs[SparseBatch](sonSparseInput))
    requireOk("sonDefaulted100", Readers.batched.readAs[DefaultedBatch](sonSparseInput))

    // the defaults decode must fill omitted fields with the constructor defaults and keep the
    // provided ones — a guard on values, not just Ok, since a wrong fill would still "decode"
    val expectedDefaulted =
      DefaultedRecord(id = 5L, retries = 9, price = 1.5, active = true)
    Readers.batched.readAs[DefaultedBatch](
      "(records = Vector((id = 5, retries = 9, price = 1.5, active = true)))"
    ) match
      case steps.result.Result.Ok(batch) =>
        if batch.records.head != expectedDefaulted then
          throw new IllegalStateException(
            s"sonDefaulted100 decodes wrong values: ${batch.records.head}"
          )
      case err =>
        throw new IllegalStateException(s"sonDefaulted100 does not decode: $err")
  }

  @Benchmark def sonFlatCompact: Any =
    Readers.batched.readAs[TypedFlatClass](sonFlatCompactInput)
  @Benchmark def sonPrimitive10Compact: Any =
    Readers.batched.readAs[TypedPrimitive10Class](sonPrimitive10CompactInput)
  @Benchmark def sonOrders100Compact: Any =
    Readers.batched.readAs[OrderBatch](sonOrdersCompactInput)

  @Benchmark def sonFlatBytes: Any =
    Readers.batched.readAs[TypedFlatClass](sonFlatBytesInput)
  @Benchmark def sonPrimitive10Bytes: Any =
    Readers.batched.readAs[TypedPrimitive10Class](sonPrimitive10BytesInput)
  @Benchmark def sonOrders100Bytes: Any =
    Readers.batched.readAs[OrderBatch](sonOrdersBytesInput)
  @Benchmark def sonOrders100CompactBytes: Any =
    Readers.batched.readAs[OrderBatch](sonOrdersCompactBytesInput)

  @Benchmark def sonShapes100: Any =
    Readers.batched.readAs[ShapeBatch](sonShapesInput)
  @Benchmark def sonShapes100Bytes: Any =
    Readers.batched.readAs[ShapeBatch](sonShapesBytesInput)
  @Benchmark def sonShapesDisc100: Any =
    Readers.batched.readAs[ShapeKBatch](sonShapesKInput)
  @Benchmark def sonShapesDisc100Bytes: Any =
    Readers.batched.readAs[ShapeKBatch](sonShapesKBytesInput)
  @Benchmark def jsoniterShapes100String: Any =
    readFromString[ShapeBatch](jsonShapesInput)
  @Benchmark def jsoniterShapes100Bytes: Any =
    readFromArray[ShapeBatch](jsonShapesBytesInput)
  @Benchmark def upickleShapes100String: Any =
    upickle.default.read[ShapeBatch](upickleShapesInput)
  @Benchmark def zioBlocksShapes100String: Any =
    zioShapesCodec.decode(zioShapesInput)

  // --- the JSON module decoding the same JSON inputs as the reference libraries ---
  locally {
    def requireOk(name: String, result: Any): Unit = result match
      case steps.result.Result.Err(error) =>
        throw new IllegalStateException(s"benchmark input $name does not decode: $error")
      case _ => ()
    import scalanotation.json.Json
    requireOk("sonJsonFlat", Json.batched.readAs[TypedFlatClass](jsonFlatInput))
    requireOk("sonJsonFlatBytes", Json.batched.readAs[TypedFlatClass](jsonFlatBytes))
    requireOk("sonJsonPrimitive10", Json.batched.readAs[TypedPrimitive10Class](jsonPrimitive10Input))
    requireOk("sonJsonOrders100", Json.batched.readAs[OrderBatch](jsonOrdersInput))
    requireOk("sonJsonOrders100Bytes", Json.batched.readAs[OrderBatch](jsonOrdersBytes))
    requireOk("sonJsonSparse100", Json.batched.readAs[SparseBatch](jsonSparseInput))
    requireOk("sonJsonDefaulted100", Json.batched.readAs[DefaultedBatch](jsonSparseInput))
    requireOk("sonJsonShapes100", Json.batched.readAs[ShapeBatch](sonJsonShapesInput))
    requireOk("sonJsonShapesDisc100", Json.batched.readAs[ShapeKBatch](sonJsonShapesKInput))

    // the shuffled and reversed forms must decode to exactly the ordered batch
    val orderedBatch = Json.batched.readAs[OrderBatch](jsonOrdersInput)
    for (name, input) <- List(
        "sonJsonOrdersShuffled100" -> jsonOrdersShuffledInput,
        "sonJsonOrdersReversed100" -> jsonOrdersReversedInput
      )
    do
      val decoded = Json.batched.readAs[OrderBatch](input)
      requireOk(name, decoded)
      if decoded != orderedBatch then
        throw new IllegalStateException(s"$name decodes different values than the ordered form")
  }

  @Benchmark def sonJsonOrdersShuffled100: Any =
    scalanotation.json.Json.batched.readAs[OrderBatch](jsonOrdersShuffledInput)
  @Benchmark def sonJsonOrdersShuffled100Bytes: Any =
    scalanotation.json.Json.batched.readAs[OrderBatch](jsonOrdersShuffledBytes)
  @Benchmark def sonJsonOrdersReversed100Bytes: Any =
    scalanotation.json.Json.batched.readAs[OrderBatch](jsonOrdersReversedBytes)
  @Benchmark def jsoniterOrdersShuffled100String: Any =
    readFromString[OrderBatch](jsonOrdersShuffledInput)
  @Benchmark def jsoniterOrdersShuffled100Bytes: Any =
    readFromArray[OrderBatch](jsonOrdersShuffledBytes)
  @Benchmark def jsoniterOrdersReversed100Bytes: Any =
    readFromArray[OrderBatch](jsonOrdersReversedBytes)
  @Benchmark def zioBlocksOrdersShuffled100String: Any =
    zioOrdersCodec.decode(jsonOrdersShuffledInput)
  @Benchmark def zioBlocksOrdersShuffled100Bytes: Any =
    zioOrdersCodec.decode(jsonOrdersShuffledBytes)
  @Benchmark def zioBlocksOrdersReversed100Bytes: Any =
    zioOrdersCodec.decode(jsonOrdersReversedBytes)
  @Benchmark def upickleOrdersShuffled100String: Any =
    upickle.default.read[OrderBatch](jsonOrdersShuffledInput)

  @Benchmark def sonJsonFlat: Any =
    scalanotation.json.Json.batched.readAs[TypedFlatClass](jsonFlatInput)
  @Benchmark def sonJsonFlatBytes: Any =
    scalanotation.json.Json.batched.readAs[TypedFlatClass](jsonFlatBytes)
  @Benchmark def sonJsonPrimitive10: Any =
    scalanotation.json.Json.batched.readAs[TypedPrimitive10Class](jsonPrimitive10Input)
  @Benchmark def sonJsonPrimitive10Bytes: Any =
    scalanotation.json.Json.batched.readAs[TypedPrimitive10Class](jsonPrimitive10Bytes)
  @Benchmark def sonJsonOrders100: Any =
    scalanotation.json.Json.batched.readAs[OrderBatch](jsonOrdersInput)
  @Benchmark def sonJsonOrders100Bytes: Any =
    scalanotation.json.Json.batched.readAs[OrderBatch](jsonOrdersBytes)
  @Benchmark def sonJsonSparse100: Any =
    scalanotation.json.Json.batched.readAs[SparseBatch](jsonSparseInput)
  @Benchmark def sonJsonSparse100Bytes: Any =
    scalanotation.json.Json.batched.readAs[SparseBatch](jsonSparseBytes)
  @Benchmark def sonJsonDefaulted100: Any =
    scalanotation.json.Json.batched.readAs[DefaultedBatch](jsonSparseInput)
  @Benchmark def sonJsonShapes100: Any =
    scalanotation.json.Json.batched.readAs[ShapeBatch](sonJsonShapesInput)
  @Benchmark def sonJsonShapes100Bytes: Any =
    scalanotation.json.Json.batched.readAs[ShapeBatch](sonJsonShapesBytesInput)
  @Benchmark def sonJsonShapesDisc100: Any =
    scalanotation.json.Json.batched.readAs[ShapeKBatch](sonJsonShapesKInput)
  @Benchmark def sonJsonShapesDisc100Bytes: Any =
    scalanotation.json.Json.batched.readAs[ShapeKBatch](sonJsonShapesKBytesInput)

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
