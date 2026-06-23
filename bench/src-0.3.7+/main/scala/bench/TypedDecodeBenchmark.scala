package bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scalanotation.BatchContext
import scalanotation.Configured
import scalanotation.Reader
import scalanotation.Readers
import scalanotation.TypedFactory
import scalanotation.macros.TypedFactories

// a typed factory pulls each constructor argument from the decoder's typed slots, so the Int
// fields are never boxed at any point of the decode
case class TypedFlatClass(x: Int, y: Int, label: String)
object TypedFlatClass:
  given TypedFactory[TypedFlatClass] = TypedFactories.derived
  given Configured[TypedFlatClass]   = Configured.typed
  given Reader[TypedFlatClass]       = Reader.configured.derived

case class Primitive10Class(
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
) derives Reader

case class TypedPrimitive10Class(
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
object TypedPrimitive10Class:
  given TypedFactory[TypedPrimitive10Class] = TypedFactories.derived
  given Configured[TypedPrimitive10Class]   = Configured.typed
  given Reader[TypedPrimitive10Class]       = Reader.configured.derived

object TypedDecodeBenchmarkHelpers {
  type Flat        = (x: Int, y: Int, label: String)
  type Primitive10 = (
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
  type WithVec      = (label: String, nums: Vector[Int])
  type WithIntArray = (label: String, nums: Array[Int])
  val flatReader: Reader[Flat]                 = summon[Reader[Flat]]
  val primitive10Reader: Reader[Primitive10]   = summon[Reader[Primitive10]]
  val withVecReader: Reader[WithVec]           = summon[Reader[WithVec]]
  val withIntArrayReader: Reader[WithIntArray] = summon[Reader[WithIntArray]]
}

/** Benchmarks that exercise HEAD-only API ([[Configured.typed]], [[Readers.batched]]); kept out of
  * the shared bench sources, which must also compile against the previous published release. The
  * batched variants reuse a thread-confined decoder, which is the path where the pooled builder
  * slots and typed factories apply.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class TypedDecodeBenchmark:
  import TypedDecodeBenchmarkHelpers.*

  // int values outside the Integer cache, so eliminated boxing is visible in the gc profile
  private val flatInput        = """(x = 1234, y = -56789, label = "hello")"""
  private val primitive10Input =
    """(f1 = 1001, f2 = -2002, f3 = 3003, f4 = -4004, f5 = 5005, f6 = -6006, f7 = 7007, f8 = -8008, f9 = 9009, f10 = -10010)"""
  private val vecInput =
    s"(label = \"bench\", nums = Vector(${(1 to 50).map(_ * 1000).mkString(", ")}))"
  private val vecInput2x =
    s"(label = \"bench\", nums = Vector(${(1 to 100).map(_ * 1000).mkString(", ")}))"

  private given Reader[Flat]         = flatReader
  private given Reader[Primitive10]  = primitive10Reader
  private given Reader[WithVec]      = withVecReader
  private given Reader[WithIntArray] = withIntArrayReader

  // benchmarks are single-threaded per State(Scope.Thread), so the local context is safe
  private given ctx: BatchContext = BatchContext.local()

  @Benchmark def flatClassTyped: Any = Readers.readAs[TypedFlatClass](flatInput)

  @Benchmark def primitive10NamedTuple: Any =
    Readers.readAs[Primitive10](primitive10Input)
  @Benchmark def primitive10Class: Any =
    Readers.readAs[Primitive10Class](primitive10Input)
  @Benchmark def primitive10ClassTyped: Any =
    Readers.readAs[TypedPrimitive10Class](primitive10Input)

  @Benchmark def flatBatched: Any           = Readers.batched.readAs[Flat](flatInput)
  @Benchmark def flatClassBatched: Any      = Readers.batched.readAs[FlatClass](flatInput)
  @Benchmark def flatClassTypedBatched: Any =
    Readers.batched.readAs[TypedFlatClass](flatInput)
  @Benchmark def primitive10NamedTupleBatched: Any =
    Readers.batched.readAs[Primitive10](primitive10Input)
  @Benchmark def primitive10ClassBatched: Any =
    Readers.batched.readAs[Primitive10Class](primitive10Input)
  @Benchmark def primitive10ClassTypedBatched: Any =
    Readers.batched.readAs[TypedPrimitive10Class](primitive10Input)
  @Benchmark def withVecBatched: Any =
    Readers.batched.readAs[WithVec](vecInput)
  @Benchmark def withIntArrayBatched: Any =
    Readers.batched.readAs[WithIntArray](vecInput)
  @Benchmark def withVec2xBatched: Any =
    Readers.batched.readAs[WithVec](vecInput2x)
  @Benchmark def withIntArray2xBatched: Any =
    Readers.batched.readAs[WithIntArray](vecInput2x)
