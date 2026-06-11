package bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scalanotation.Readers

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class DecodeBenchmark:

  // Simple flat named tuple — 3 fields
  type Flat = (x: Int, y: Int, label: String)
  private val flatInput = """(x = 1, y = -2, label = "hello")"""

  // 2x: 6 fields
  type Flat2x = (x: Int, y: Int, label: String, a: Int, b: Int, extra: String)
  private val flatInput2x = """(x = 1, y = -2, label = "hello", a = 3, b = -4, extra = "world")"""

  // Nested named tuple — 3 top-level fields, 1 nested object
  type Nested = (
      name: String,
      value: Double,
      inner: (enabled: Boolean, count: Int)
  )
  private val nestedInput =
    """(name = "test", value = 3.14, inner = (enabled = true, count = 42))"""

  // 2x: 5 top-level fields, 2 nested objects
  type Nested2x = (
      name: String,
      value: Double,
      inner: (enabled: Boolean, count: Int),
      tag: String,
      meta: (active: Boolean, score: Int)
  )
  private val nestedInput2x =
    """(name = "test", value = 3.14, inner = (enabled = true, count = 42), tag = "x", meta = (active = false, score = 7))"""

  // Named tuple with a Vector — 20 elements
  type WithVec = (label: String, nums: Vector[Int])
  private val vecInput = s"(label = \"bench\", nums = Vector(${(1 to 20).mkString(", ")}))"

  // 2x: 40 elements (same type, longer input)
  private val vecInput2x = s"(label = \"bench\", nums = Vector(${(1 to 40).mkString(", ")}))"

  // Declaration form inputs
  private val declFlatInput   = """val data = (x = 1, y = -2, label = "hello")"""
  private val declFlatInput2x =
    """val data = (x = 1, y = -2, label = "hello", a = 3, b = -4, extra = "world")"""
  private val declVecInput =
    s"val data = (label = \"bench\", nums = Vector(${(1 to 20).mkString(", ")}))"
  private val declVecInput2x =
    s"val data = (label = \"bench\", nums = Vector(${(1 to 40).mkString(", ")}))"

  @Benchmark def flat: Any   = Readers.readAs[Flat](flatInput)
  @Benchmark def flat2x: Any = Readers.readAs[Flat2x](flatInput2x)

  @Benchmark def nested: Any   = Readers.readAs[Nested](nestedInput)
  @Benchmark def nested2x: Any = Readers.readAs[Nested2x](nestedInput2x)

  @Benchmark def withVec: Any   = Readers.readAs[WithVec](vecInput)
  @Benchmark def withVec2x: Any = Readers.readAs[WithVec](vecInput2x)

  @Benchmark def declFlat: Any   = Readers.readDeclAs[Flat](declFlatInput, rootName = "data")
  @Benchmark def declFlat2x: Any = Readers.readDeclAs[Flat2x](declFlatInput2x, rootName = "data")

  @Benchmark def declWithVec: Any   = Readers.readDeclAs[WithVec](declVecInput, rootName = "data")
  @Benchmark def declWithVec2x: Any = Readers.readDeclAs[WithVec](declVecInput2x, rootName = "data")
