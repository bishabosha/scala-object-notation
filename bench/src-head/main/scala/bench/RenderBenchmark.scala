package bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scalanotation.Configured
import scalanotation.ReadWriter
import scalanotation.TypedFactory
import scalanotation.Writer
import scalanotation.Writers
import scalanotation.macros.TypedFactories

// plain derived writer: fields are pulled boxed through Product.productElement
case class WriteFlatClass(x: Int, y: Int, label: String) derives Writer

// typed writer: the attached factory reads primitive fields unboxed via the typed accessors
case class TypedWriteFlatClass(x: Int, y: Int, label: String)
object TypedWriteFlatClass:
  given TypedFactory[TypedWriteFlatClass] = TypedFactories.derived
  given Configured[TypedWriteFlatClass]   = Configured.typed
  given ReadWriter[TypedWriteFlatClass]   = ReadWriter.configured.derived

case class WritePrimitive10Class(
    f1: Int,
    f2: Int,
    f3: Int,
    f4: Int,
    f5: Int,
    f6: Int,
    f7: Int,
    f8: Int,
    f9: Int,
    f10: Int
) derives Writer

case class TypedWritePrimitive10Class(
    f1: Int,
    f2: Int,
    f3: Int,
    f4: Int,
    f5: Int,
    f6: Int,
    f7: Int,
    f8: Int,
    f9: Int,
    f10: Int
)
object TypedWritePrimitive10Class:
  given TypedFactory[TypedWritePrimitive10Class] = TypedFactories.derived
  given Configured[TypedWritePrimitive10Class]   = Configured.typed
  given ReadWriter[TypedWritePrimitive10Class]   = ReadWriter.configured.derived

/** Write-side benchmarks: rendering values back to text. Covers the string-literal fast path,
  * unboxed numeric appends, the specialized indexed writes for primitive arrays, and typed field
  * access for case classes.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class RenderBenchmark:
  private type WithVec      = (label: String, nums: Vector[Int])
  private type WithIntArray = (label: String, nums: Array[Int])
  private type Strings      = (a: String, b: String, c: String)

  // int values outside the Integer cache, so eliminated boxing is visible in the gc profile
  private val flatValue        = WriteFlatClass(1234, -56789, "hello")
  private val typedFlatValue   = TypedWriteFlatClass(1234, -56789, "hello")
  private val primitive10Value =
    WritePrimitive10Class(1001, -2002, 3003, -4004, 5005, -6006, 7007, -8008, 9009, -10010)
  private val typedPrimitive10Value =
    TypedWritePrimitive10Class(1001, -2002, 3003, -4004, 5005, -6006, 7007, -8008, 9009, -10010)

  private val withVecValue: WithVec = (label = "bench", nums = Vector.tabulate(100)(_ * 1000))
  private val withIntArrayValue: WithIntArray =
    (label = "bench", nums = Array.tabulate(100)(_ * 1000))

  private val stringsValue: Strings =
    (
      a = "a plain string with no escapes at all, rendered via the bulk fast path",
      b = "a string \"with\" some\nescaped\tcharacters",
      c = "short"
    )

  @Benchmark def flatClass: String      = Writers.write(flatValue)
  @Benchmark def flatClassTyped: String = Writers.write(typedFlatValue)

  @Benchmark def primitive10Class: String      = Writers.write(primitive10Value)
  @Benchmark def primitive10ClassTyped: String = Writers.write(typedPrimitive10Value)

  @Benchmark def withIntVector: String = Writers.write(withVecValue)
  @Benchmark def withIntArray: String  = Writers.write(withIntArrayValue)

  @Benchmark def strings: String = Writers.write(stringsValue)
