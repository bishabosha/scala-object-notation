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
class DedentedStringBenchmark:
  private val concatEscapesInput =
    "\"alpha\\n\" + \"beta\\n\" + \"gamma\\n\" + \"delta\""

  private val dedentedInput =
    """import language.experimental.dedentedStringLiterals
      |'''
      |alpha
      |beta
      |gamma
      |delta
      |'''
      |""".stripMargin

  @Benchmark def concatEscapes: Any =
    Readers.readAs[String](concatEscapesInput)

  @Benchmark def dedented: Any =
    Readers.experimental.readAs[String](dedentedInput)
