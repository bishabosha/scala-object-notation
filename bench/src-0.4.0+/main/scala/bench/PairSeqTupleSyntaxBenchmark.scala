package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import scalanotation.BatchContext
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.Readers

object PairSeqTupleSyntaxBenchmarkHelpers:
  final case class StringIntPairSeq(rows: Vector[(String, Int)])

  final class StringIntPairSeqState:
    val rows: scala.collection.mutable.Builder[(String, Int), Vector[(String, Int)]] =
      Vector.newBuilder[(String, Int)]
    var key: String = ""

  object StringIntPairSeqReader
      extends Reader.PairSeqBuilder[String, Int, StringIntPairSeqState, StringIntPairSeq]:
    def init(): StringIntPairSeqState =
      new StringIntPairSeqState

    def addKey(state: StringIntPairSeqState, key: String): StringIntPairSeqState =
      state.key = key
      state

    def addValue(state: StringIntPairSeqState, value: Int): StringIntPairSeqState =
      state.rows.addOne(state.key -> value)
      state

    def finish(state: StringIntPairSeqState): StringIntPairSeq =
      StringIntPairSeq(state.rows.result())

  val pairSeqReader: Reader[StringIntPairSeq] =
    Reader.pairSeq(
      summon[Reader[String]],
      summon[Reader[Int]],
      StringIntPairSeqReader
    )

  val exprReader: Reader[Expr] = summon[Reader[Expr]]

  val pairs: Vector[(String, Int)] =
    Vector(
      "abc" -> 123,
      "def" -> 456,
      "ghi" -> 789,
      "jkl" -> 1023,
      "mno" -> 2046,
      "pqr" -> 4092,
      "stu" -> 8184,
      "vwx" -> 16368,
      "yz0" -> 32736,
      "z99" -> 65472
    )

  val tupleSyntaxInput: String =
    pairs.iterator.map((key, value) => s"""("$key", $value)""").mkString("Vector(", ", ", ")")

/** Compatible with the published 0.4.0 benchmark target. This measures pair-sequence text decoding
  * using the existing `Vector((key, value), ...)` shape, both as raw `Expr` and through a typed
  * pair-sequence schema.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class PairSeqTupleSyntaxBenchmark:
  import PairSeqTupleSyntaxBenchmarkHelpers.*

  private val input = tupleSyntaxInput

  private given Reader[Expr]             = exprReader
  private given Reader[StringIntPairSeq] = pairSeqReader

  private given ctx: BatchContext = BatchContext.local()

  @Benchmark def decodeExpr: Any =
    Readers.readAs[Expr](input)

  @Benchmark def decodeExprBatched: Any =
    Readers.batched.readAs[Expr](input)

  @Benchmark def decodeTyped: Any =
    Readers.readAs[StringIntPairSeq](input)

  @Benchmark def decodeTypedBatched: Any =
    Readers.batched.readAs[StringIntPairSeq](input)
