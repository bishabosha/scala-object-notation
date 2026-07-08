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

  val collectionTupleSyntaxInput: String =
    PairSeqTupleSyntaxBenchmarkHelpers.pairs.iterator
      .map((key, value) => s"""("$key", $value)""")
      .mkString(
        "import language.experimental.collectionLiterals\n[",
        ", ",
        "]"
      )

/** HEAD-only benchmark for the experimental collection-literal spelling. It mirrors
  * [[PairSeqTupleSyntaxBenchmark]] but decodes `["key" -> value, ...]` through
  * `Readers.experimental`. The `*TupleElements` variants use `[("key", value), ...]` under the same
  * experimental reader path, so they isolate `->` from import and bracket-literal costs.
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

  private val arrowInput           = arrowSyntaxInput
  private val collectionTupleInput = collectionTupleSyntaxInput

  private given Reader[Expr]             = exprReader
  private given Reader[StringIntPairSeq] = pairSeqReader

  private given ctx: BatchContext = BatchContext.local()

  @Benchmark def decodeExpr: Any =
    Readers.experimental.readAs[Expr](arrowInput)

  @Benchmark def decodeExprBatched: Any =
    Readers.experimental.batched.readAs[Expr](arrowInput)

  @Benchmark def decodeExprTupleElements: Any =
    Readers.experimental.readAs[Expr](collectionTupleInput)

  @Benchmark def decodeExprTupleElementsBatched: Any =
    Readers.experimental.batched.readAs[Expr](collectionTupleInput)

  @Benchmark def decodeTyped: Any =
    Readers.experimental.readAs[StringIntPairSeq](arrowInput)

  @Benchmark def decodeTypedBatched: Any =
    Readers.experimental.batched.readAs[StringIntPairSeq](arrowInput)

  @Benchmark def decodeTypedTupleElements: Any =
    Readers.experimental.readAs[StringIntPairSeq](collectionTupleInput)

  @Benchmark def decodeTypedTupleElementsBatched: Any =
    Readers.experimental.batched.readAs[StringIntPairSeq](collectionTupleInput)
