package bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.Readers

// data shapes for the derived product benchmarks
case class FlatClass(x: Int, y: Int, label: String) derives Reader
case class NestedInner(enabled: Boolean, count: Int) derives Reader
case class NestedClass(name: String, value: Double, inner: NestedInner) derives Reader

object DecodeBenchmarkHelpers:
  type Flat   = (x: Int, y: Int, label: String)
  type Flat2x = (x: Int, y: Int, label: String, a: Int, b: Int, extra: String)
  type Nested = (
      name: String,
      value: Double,
      inner: (enabled: Boolean, count: Int)
  )
  type Nested2x = (
      name: String,
      value: Double,
      inner: (enabled: Boolean, count: Int),
      tag: String,
      meta: (active: Boolean, score: Int)
  )
  type WithVec = (label: String, nums: Vector[Int])
  // Named tuple with an Array — same input shape as the vector benchmarks
  type WithIntArray = (label: String, nums: Array[Int])
  val flatReader: Reader[Flat]                 = summon[Reader[Flat]]
  val flat2xReader: Reader[Flat2x]             = summon[Reader[Flat2x]]
  val nestedReader: Reader[Nested]             = summon[Reader[Nested]]
  val nested2xReader: Reader[Nested2x]         = summon[Reader[Nested2x]]
  val withVecReader: Reader[WithVec]           = summon[Reader[WithVec]]
  val withIntArrayReader: Reader[WithIntArray] = summon[Reader[WithIntArray]]
  val exprReader: Reader[Expr]                 = summon[Reader[Expr]]

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class DecodeBenchmark:
  import DecodeBenchmarkHelpers.*

  // Simple flat named tuple — 3 fields
  private val flatInput = """(x = 1, y = -2, label = "hello")"""

  // 2x: 6 fields
  private val flatInput2x = """(x = 1, y = -2, label = "hello", a = 3, b = -4, extra = "world")"""

  // Nested named tuple — 3 top-level fields, 1 nested object
  private val nestedInput =
    """(name = "test", value = 3.14, inner = (enabled = true, count = 42))"""

  // 2x: 5 top-level fields, 2 nested objects
  private val nestedInput2x =
    """(name = "test", value = 3.14, inner = (enabled = true, count = 42), tag = "x", meta = (active = false, score = 7))"""

  // Named tuple with a Vector — 20 elements
  private val vecInput =
    s"(label = \"bench\", nums = Vector(${(1 to 50).map(_ * 1000).mkString(", ")}))"

  // 2x: 40 elements (same type, longer input)
  private val vecInput2x =
    s"(label = \"bench\", nums = Vector(${(1 to 100).map(_ * 1000).mkString(", ")}))"

  // Declaration form inputs
  private val declFlatInput   = """val data = (x = 1, y = -2, label = "hello")"""
  private val declFlatInput2x =
    """val data = (x = 1, y = -2, label = "hello", a = 3, b = -4, extra = "world")"""
  private val declVecInput =
    s"val data = (label = \"bench\", nums = Vector(${(1 to 50).map(_ * 1000).mkString(", ")}))"
  private val declVecInput2x =
    s"val data = (label = \"bench\", nums = Vector(${(1 to 100).map(_ * 1000).mkString(", ")}))"

  // readers are derived once and reused, so the benchmarks measure decoding rather than
  // re-deriving the schema graph on every call
  private given Reader[Flat]         = flatReader
  private given Reader[Flat2x]       = flat2xReader
  private given Reader[Nested]       = nestedReader
  private given Reader[Nested2x]     = nested2xReader
  private given Reader[WithVec]      = withVecReader
  private given Reader[WithIntArray] = withIntArrayReader
  private given Reader[Expr]         = exprReader

  @Benchmark def flat: Any   = Readers.readAs[Flat](flatInput)
  @Benchmark def flat2x: Any = Readers.readAs[Flat2x](flatInput2x)

  @Benchmark def exprFlat: Any      = Readers.readAs[Expr](flatInput)
  @Benchmark def exprNested: Any    = Readers.readAs[Expr](nestedInput)
  @Benchmark def exprNested2x: Any  = Readers.readAs[Expr](nestedInput2x)
  @Benchmark def exprWithVec: Any   = Readers.readAs[Expr](vecInput)
  @Benchmark def exprWithVec2x: Any = Readers.readAs[Expr](vecInput2x)

  @Benchmark def flatClass: Any   = Readers.readAs[FlatClass](flatInput)
  @Benchmark def nestedClass: Any = Readers.readAs[NestedClass](nestedInput)

  @Benchmark def nested: Any   = Readers.readAs[Nested](nestedInput)
  @Benchmark def nested2x: Any = Readers.readAs[Nested2x](nestedInput2x)

  @Benchmark def withVec: Any   = Readers.readAs[WithVec](vecInput)
  @Benchmark def withVec2x: Any = Readers.readAs[WithVec](vecInput2x)

  @Benchmark def withIntArray: Any   = Readers.readAs[WithIntArray](vecInput)
  @Benchmark def withIntArray2x: Any = Readers.readAs[WithIntArray](vecInput2x)

  @Benchmark def declFlat: Any =
    Readers.readDeclAs[Flat](declFlatInput, rootName = "data")
  @Benchmark def declFlat2x: Any =
    Readers.readDeclAs[Flat2x](declFlatInput2x, rootName = "data")

  @Benchmark def declExprFlat: Any =
    Readers.readDeclAs[Expr](declFlatInput, rootName = "data")
  @Benchmark def declExprWithVec: Any =
    Readers.readDeclAs[Expr](declVecInput, rootName = "data")
  @Benchmark def declExprWithVec2x: Any =
    Readers.readDeclAs[Expr](declVecInput2x, rootName = "data")

  @Benchmark def declWithVec: Any =
    Readers.readDeclAs[WithVec](declVecInput, rootName = "data")
  @Benchmark def declWithVec2x: Any =
    Readers.readDeclAs[WithVec](declVecInput2x, rootName = "data")

  @Benchmark def withIntArray2xNorm: Any = (
    label = "bench",
    nums = Array(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000,
      14000, 15000, 16000, 17000, 18000, 19000, 20000, 21000, 22000, 23000, 24000, 25000, 26000,
      27000, 28000, 29000, 30000, 31000, 32000, 33000, 34000, 35000, 36000, 37000, 38000, 39000,
      40000, 41000, 42000, 43000, 44000, 45000, 46000, 47000, 48000, 49000, 50000, 51000, 52000,
      53000, 54000, 55000, 56000, 57000, 58000, 59000, 60000, 61000, 62000, 63000, 64000, 65000,
      66000, 67000, 68000, 69000, 70000, 71000, 72000, 73000, 74000, 75000, 76000, 77000, 78000,
      79000, 80000, 81000, 82000, 83000, 84000, 85000, 86000, 87000, 88000, 89000, 90000, 91000,
      92000, 93000, 94000, 95000, 96000, 97000, 98000, 99000, 100000)
  )
  @Benchmark def withVec2xNorm: Any = (
    label = "bench",
    nums = Vector(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000,
      14000, 15000, 16000, 17000, 18000, 19000, 20000, 21000, 22000, 23000, 24000, 25000, 26000,
      27000, 28000, 29000, 30000, 31000, 32000, 33000, 34000, 35000, 36000, 37000, 38000, 39000,
      40000, 41000, 42000, 43000, 44000, 45000, 46000, 47000, 48000, 49000, 50000, 51000, 52000,
      53000, 54000, 55000, 56000, 57000, 58000, 59000, 60000, 61000, 62000, 63000, 64000, 65000,
      66000, 67000, 68000, 69000, 70000, 71000, 72000, 73000, 74000, 75000, 76000, 77000, 78000,
      79000, 80000, 81000, 82000, 83000, 84000, 85000, 86000, 87000, 88000, 89000, 90000, 91000,
      92000, 93000, 94000, 95000, 96000, 97000, 98000, 99000, 100000)
  )
