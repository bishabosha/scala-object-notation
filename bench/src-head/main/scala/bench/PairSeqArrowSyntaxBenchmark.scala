package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import scalanotation.BatchContext
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.Readers

object PairSeqArrowSyntaxBenchmarkHelpers:
  val arrowSyntaxInput: String =
    PairSeqTupleSyntaxBenchmarkHelpers.pairs.iterator
      .map((key, value) => s""""$key" -> $value""")
      .mkString(
        "import language.experimental.collectionLiterals\n[",
        ", ",
        "]"
      )

/** HEAD-only benchmark for the experimental collection-literal spelling. It mirrors
  * [[PairSeqTupleSyntaxBenchmark]] but decodes `["key" -> value, ...]` through
  * `Readers.experimental`.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class PairSeqArrowSyntaxBenchmark:
  import PairSeqArrowSyntaxBenchmarkHelpers.*
  import PairSeqTupleSyntaxBenchmarkHelpers.*

  private val input = arrowSyntaxInput

  private given Reader[Expr]             = exprReader
  private given Reader[StringIntPairSeq] = pairSeqReader

  private given ctx: BatchContext = BatchContext.local()

  @Benchmark def decodeExpr: Any =
    Readers.experimental.readAs[Expr](input)

  @Benchmark def decodeExprBatched: Any =
    Readers.experimental.batched.readAs[Expr](input)

  @Benchmark def decodeTyped: Any =
    Readers.experimental.readAs[StringIntPairSeq](input)

  @Benchmark def decodeTypedBatched: Any =
    Readers.experimental.batched.readAs[StringIntPairSeq](input)
