package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import scalanotation.BatchContext
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.Readers
import scalanotation.Writer
import scalanotation.Writers

object ArbitraryMapBenchmarkHelpers:
  type IntStringMap = Map[Int, String]

  val mapReader: Reader[IntStringMap] = summon[Reader[IntStringMap]]
  val mapWriter: Writer[IntStringMap] = summon[Writer[IntStringMap]]

  def mapValue(size: Int): IntStringMap =
    (1 to size).iterator.map(i => i -> s"value_$i").toMap

  def mapInput(size: Int): String =
    (1 to size).iterator.map(i => s"""($i, "value_$i")""").mkString("Vector(", ", ", ")")

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ArbitraryMapBenchmark:
  import ArbitraryMapBenchmarkHelpers.*

  private val map8Input  = mapInput(8)
  private val map64Input = mapInput(64)

  private val map8Value  = mapValue(8)
  private val map64Value = mapValue(64)

  private val map8Expr: Expr  = Writers.writeExpr(map8Value)
  private val map64Expr: Expr = Writers.writeExpr(map64Value)

  private given Reader[IntStringMap] = mapReader
  private given Writer[IntStringMap] = mapWriter

  private given ctx: BatchContext = BatchContext.local()

  @Benchmark def decodeText8: Any =
    Readers.readAs[IntStringMap](map8Input)

  @Benchmark def decodeText64: Any =
    Readers.readAs[IntStringMap](map64Input)

  @Benchmark def decodeText8Batched: Any =
    Readers.batched.readAs[IntStringMap](map8Input)

  @Benchmark def decodeText64Batched: Any =
    Readers.batched.readAs[IntStringMap](map64Input)

  @Benchmark def decodeExpr8: Any =
    map8Expr.decodeAs[IntStringMap]

  @Benchmark def decodeExpr64: Any =
    map64Expr.decodeAs[IntStringMap]

  @Benchmark def writeExpr8: Any =
    Writers.writeExpr(map8Value)

  @Benchmark def writeExpr64: Any =
    Writers.writeExpr(map64Value)

  @Benchmark def writeText8: Any =
    Writers.write(map8Value)

  @Benchmark def writeText64: Any =
    Writers.write(map64Value)
