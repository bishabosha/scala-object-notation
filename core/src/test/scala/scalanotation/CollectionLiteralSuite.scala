package scalanotation

import scalanotation.schema.RawSchema

import scala.collection.immutable.ListMap

import steps.result.Result

class CollectionLiteralSuite extends ScalanotationSuite:
  private val ExperimentalImport =
    "import language.experimental.collectionLiterals"
  private val GroupedExperimentalImport =
    "import language.experimental.{dedentedStringLiterals, collectionLiterals}"

  private final case class StringIntPairs(entries: ListMap[String, Int])

  private object StringIntPairs:
    final class State:
      val entries: scala.collection.mutable.Builder[(String, Int), ListMap[String, Int]] =
        ListMap.newBuilder[String, Int]
      var key: String = ""

    object Builder extends Reader.PairSeqBuilder[String, Int, State, StringIntPairs]:
      def init(): State =
        new State

      def addKey(repr: State, key: String): State =
        repr.key = key
        repr

      def addValue(repr: State, elem: Int): State =
        repr.entries.addOne(repr.key -> elem)
        repr

      def finish(repr: State): StringIntPairs =
        StringIntPairs(repr.entries.result())

    given Reader[StringIntPairs] =
      Reader.pairSeq(
        summon[Reader[String]],
        summon[Reader[Int]],
        Builder
      )

  test("tokenize collection literal punctuation"):
    assertEquals(tokenLabels("[] {} ->"), List("[", "]", "{", "}", "->", "eof"))

  test("read bracket sequence as Vector"):
    val input = s"$ExperimentalImport\n[1, 2, 3]"

    assertEquals(Readers.experimental.readAs[Vector[Int]](input), Result.Ok(Vector(1, 2, 3)))

  test("read bracket sequence with grouped experimental import"):
    val input = s"$GroupedExperimentalImport\n[1, 2, 3]"

    assertEquals(Readers.experimental.readAs[Vector[Int]](input), Result.Ok(Vector(1, 2, 3)))

  test("read bracket sequence with experimental batched reader"):
    given BatchContext = BatchContext.local()

    val input = s"$ExperimentalImport\n[1, 2, 3]"

    assertEquals(
      Readers.experimental.batched.readAs[Vector[Int]](input),
      Result.Ok(Vector(1, 2, 3))
    )

  test("collection literal import is required for bracket sequences"):
    Readers.experimental.readAs[Vector[Int]]("[1]") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedType("Vector[...]", "'['"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("read arrow pair as router tuple"):
    val input = s"""$ExperimentalImport
                   |1 -> "one"
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[Expr](input),
      Result.Ok(
        Expr.TupleExpr(
          IndexedSeq(
            Expr.IntConstant(1),
            Expr.StringConstant("one")
          )
        )
      )
    )

  test("read chained arrow pairs as left-associative router tuples"):
    val input = s"""$ExperimentalImport
                   |1 -> "abc" -> true
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[Expr](input),
      Result.Ok(
        Expr.TupleExpr(
          IndexedSeq(
            Expr.TupleExpr(
              IndexedSeq(
                Expr.IntConstant(1),
                Expr.StringConstant("abc")
              )
            ),
            Expr.BooleanConstant(true)
          )
        )
      )
    )

  test("read chained arrow pairs into nested tuple types"):
    val input = s"""$ExperimentalImport
                   |1 -> "abc" -> true
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[((Int, String), Boolean)](input),
      Result.Ok(((1, "abc"), true))
    )

  test("read bracket sequence through Expr router"):
    val input = s"""$ExperimentalImport
                   |[1, 2 -> "two", true]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[Expr](input),
      Result.Ok(
        Expr.VectorExpr(
          IndexedSeq(
            Expr.IntConstant(1),
            Expr.TupleExpr(
              IndexedSeq(
                Expr.IntConstant(2),
                Expr.StringConstant("two")
              )
            ),
            Expr.BooleanConstant(true)
          )
        )
      )
    )

  test("read nested arrow expressions inside bracket sequences"):
    val input = s"""$ExperimentalImport
                   |[
                   |  1 -> "one" -> true,
                   |  ["nested" -> 2, 3],
                   |  (4, 5 -> "five")
                   |]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[Expr](input),
      Result.Ok(
        Expr.VectorExpr(
          IndexedSeq(
            Expr.TupleExpr(
              IndexedSeq(
                Expr.TupleExpr(
                  IndexedSeq(
                    Expr.IntConstant(1),
                    Expr.StringConstant("one")
                  )
                ),
                Expr.BooleanConstant(true)
              )
            ),
            Expr.VectorExpr(
              IndexedSeq(
                Expr.TupleExpr(
                  IndexedSeq(
                    Expr.StringConstant("nested"),
                    Expr.IntConstant(2)
                  )
                ),
                Expr.IntConstant(3)
              )
            ),
            Expr.TupleExpr(
              IndexedSeq(
                Expr.IntConstant(4),
                Expr.TupleExpr(
                  IndexedSeq(
                    Expr.IntConstant(5),
                    Expr.StringConstant("five")
                  )
                )
              )
            )
          )
        )
      )
    )

  test("read arrow pair elements in PairSeq"):
    val input = s"""$ExperimentalImport
                   |["abc" -> 1, "def" -> 2]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[StringIntPairs](input),
      Result.Ok(StringIntPairs(ListMap("abc" -> 1, "def" -> 2)))
    )

  test("pairSeqAsMap Reader decodes collection literal arrows into Map"):
    type Data = ListMap[String, Int]

    given reader: Reader[Data] = Reader.pairSeqAsDict

    reader.schema match
      case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
        assert(pairSeq.read != null)
        assert(pairSeq.write == null)
        assertEquals(pairSeq.key.asInstanceOf[Any], RawSchema.String)
        assertEquals(pairSeq.value.asInstanceOf[Any], RawSchema.Int)
      case other =>
        fail(s"Expected a pair sequence schema, got ${other.describeSelf}")

    val input = s"""$ExperimentalImport
                   |["abc" -> 123, "def" -> 456]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[Data](input),
      Result.Ok(ListMap("abc" -> 123, "def" -> 456))
    )

  test("pairSeqAsMap Reader decodes collection literal arrows into Map [prio]"):
    case class Data(entries: ListMap[String, Int]) derives Reader
    object Data:
      private given innerReader: Reader[ListMap[String, Int]] = Reader.pairSeqAsDict

    summon[Reader[Data]].schema match
      case RawSchema.NamedTuple(fields = IArray(field)) =>
        field.schema match
          case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
            assert(pairSeq.read != null)
            assert(pairSeq.write == null)
            assertEquals(pairSeq.key.asInstanceOf[Any], RawSchema.String)
            assertEquals(pairSeq.value.asInstanceOf[Any], RawSchema.Int)
          case other =>
            fail(s"Expected a pair sequence schema, got ${other}")
      case other =>
        fail(s"Expected a named tuple schema, got ${other.describeSelf}")

    val input = s"""$ExperimentalImport
                   |(entries = ["abc" -> 123, "def" -> 456])
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[Data](input),
      Result.Ok(Data(ListMap("abc" -> 123, "def" -> 456)))
    )

  test("pairSeqAsMap Writer renders Map as pair sequence"):
    type Data = ListMap[String, Int]

    given writer: Writer[Data] = Writer.pairSeqAsDict

    writer.schema match
      case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
        assert(pairSeq.read == null)
        assert(pairSeq.write != null)
        assertEquals(pairSeq.key.asInstanceOf[Any], RawSchema.String)
        assertEquals(pairSeq.value.asInstanceOf[Any], RawSchema.Int)
      case other =>
        fail(s"Expected a pair sequence schema, got ${other.describeSelf}")

    val value = ListMap("abc" -> 123, "def" -> 456)

    assertEquals(
      Writers.write[Data](value),
      """Vector(("abc", 123), ("def", 456))"""
    )

  test(
    "default ReadWriter for non-string key map decodes collection literals and writes pair sequences"
  ):
    type Data = ListMap[Int, Boolean]

    val readWriter: ReadWriter[Data] = summon[ReadWriter[Data]]

    readWriter.schema match
      case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
        assert(pairSeq.read != null)
        assert(pairSeq.write != null)
        assertEquals(pairSeq.key.asInstanceOf[Any], RawSchema.Int)
        assertEquals(pairSeq.value.asInstanceOf[Any], RawSchema.Boolean)
      case other =>
        fail(s"Expected a pair sequence schema, got ${other.describeSelf}")

    val input = s"""$ExperimentalImport
                   |[123 -> true, 456 -> false]
                   |""".stripMargin
    val value = ListMap(123 -> true, 456 -> false)

    assertEquals(
      Readers.experimental.readAs[Data](input),
      Result.Ok(value)
    )

    val rendered = Writers.write[Data](value)

    assertEquals(rendered, """Vector((123, true), (456, false))""")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

  test("pairSeqAsDict ReadWriter decodes collection literals and writes pair sequences"):
    type Data = ListMap[String, Int]

    val rw             = ReadWriter.pairSeqAsDict[Data]()
    given Reader[Data] = rw.reader
    given Writer[Data] = rw.writer

    rw.reader.schema match
      case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
        assert(pairSeq.read != null)
        assert(pairSeq.write != null)
        assertEquals(pairSeq.key.asInstanceOf[Any], RawSchema.String)
        assertEquals(pairSeq.value.asInstanceOf[Any], RawSchema.Int)
      case other =>
        fail(s"Expected a pair sequence schema, got ${other.describeSelf}")

    val input = s"""$ExperimentalImport
                   |["abc" -> 123, "def" -> 456]
                   |""".stripMargin
    val value = ListMap("abc" -> 123, "def" -> 456)

    assertEquals(
      Readers.experimental.readAs[Data](input),
      Result.Ok(value)
    )

    val rendered = Writers.write[Data](value)

    assertEquals(rendered, """Vector(("abc", 123), ("def", 456))""")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

  test("read complex arrow values in PairSeq"):
    val input = s"""$ExperimentalImport
                   |[
                   |  (1, "one" -> true),
                   |  (2, [false, 3 -> "three"])
                   |]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[ListMap[Int, Expr]](input),
      Result.Ok(
        ListMap(
          1 -> Expr.TupleExpr(
            IndexedSeq(
              Expr.StringConstant("one"),
              Expr.BooleanConstant(true)
            )
          ),
          2 -> Expr.VectorExpr(
            IndexedSeq(
              Expr.BooleanConstant(false),
              Expr.TupleExpr(
                IndexedSeq(
                  Expr.IntConstant(3),
                  Expr.StringConstant("three")
                )
              )
            )
          )
        )
      )
    )

  test("read chained arrow pair elements as tuple keys in PairSeq"):
    val input = s"""$ExperimentalImport
                   |[
                   |  1 -> "one" -> true,
                   |  2 -> "two" -> false
                   |]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[ListMap[(Int, String), Boolean]](input),
      Result.Ok(
        ListMap(
          (1, "one") -> true,
          (2, "two") -> false
        )
      )
    )

  test("read arrow pair elements with router keys in PairSeq"):
    val input = s"""$ExperimentalImport
                   |[1 -> 2, "three" -> 4]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[ListMap[Expr, Int]](input),
      Result.Ok(
        ListMap(
          Expr.IntConstant(1)          -> 2,
          Expr.StringConstant("three") -> 4
        )
      )
    )

  test("read tuple pair elements with complex router keys in PairSeq"):
    val input = s"""$ExperimentalImport
                   |[(1 -> "one", 2)]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[ListMap[Expr, Int]](input),
      Result.Ok(
        ListMap(
          Expr.TupleExpr(
            IndexedSeq(
              Expr.IntConstant(1),
              Expr.StringConstant("one")
            )
          ) -> 2
        )
      )
    )

  test("read chained arrow pair element as Expr key in PairSeq"):
    val input = s"""$ExperimentalImport
                   |[1 -> "one" -> true]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[ListMap[Expr, Expr]](input),
      Result.Ok(
        ListMap(
          Expr.TupleExpr(
            IndexedSeq(
              Expr.IntConstant(1),
              Expr.StringConstant("one")
            )
          ) -> Expr.BooleanConstant(true)
        )
      )
    )

  test("read arrow pair elements with complex Expr keys and values in PairSeq"):
    val input = s"""$ExperimentalImport
                   |[
                   |  1 -> "one" -> true,
                   |  [2 -> "two", false] -> (kind = "vector-key", value = 3 -> "three"),
                   |  Tuple(4 -> "four") -> ["tuple-value", 5 -> "five"]
                   |]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[ListMap[Expr, Expr]](input),
      Result.Ok(
        ListMap(
          Expr.TupleExpr(
            IndexedSeq(
              Expr.IntConstant(1),
              Expr.StringConstant("one")
            )
          ) -> Expr.BooleanConstant(true),
          Expr.VectorExpr(
            IndexedSeq(
              Expr.TupleExpr(
                IndexedSeq(
                  Expr.IntConstant(2),
                  Expr.StringConstant("two")
                )
              ),
              Expr.BooleanConstant(false)
            )
          ) -> Expr.NamedTupleExpr(
            IndexedSeq(
              "kind"  -> Expr.StringConstant("vector-key"),
              "value" -> Expr.TupleExpr(
                IndexedSeq(
                  Expr.IntConstant(3),
                  Expr.StringConstant("three")
                )
              )
            )
          ),
          Expr.TupleExpr(
            IndexedSeq(
              Expr.TupleExpr(
                IndexedSeq(
                  Expr.IntConstant(4),
                  Expr.StringConstant("four")
                )
              )
            )
          ) -> Expr.VectorExpr(
            IndexedSeq(
              Expr.StringConstant("tuple-value"),
              Expr.TupleExpr(
                IndexedSeq(
                  Expr.IntConstant(5),
                  Expr.StringConstant("five")
                )
              )
            )
          )
        )
      )
    )

  test("read tuple pair elements with arrow Expr keys and values in PairSeq"):
    val input = s"""$ExperimentalImport
                   |[
                   |  (1 -> "one", "value" -> true),
                   |  (["key" -> 2], (nested = 3 -> "three"))
                   |]
                   |""".stripMargin

    assertEquals(
      Readers.experimental.readAs[ListMap[Expr, Expr]](input),
      Result.Ok(
        ListMap(
          Expr.TupleExpr(
            IndexedSeq(
              Expr.IntConstant(1),
              Expr.StringConstant("one")
            )
          ) -> Expr.TupleExpr(
            IndexedSeq(
              Expr.StringConstant("value"),
              Expr.BooleanConstant(true)
            )
          ),
          Expr.VectorExpr(
            IndexedSeq(
              Expr.TupleExpr(
                IndexedSeq(
                  Expr.StringConstant("key"),
                  Expr.IntConstant(2)
                )
              )
            )
          ) -> Expr.NamedTupleExpr(
            IndexedSeq(
              "nested" -> Expr.TupleExpr(
                IndexedSeq(
                  Expr.IntConstant(3),
                  Expr.StringConstant("three")
                )
              )
            )
          )
        )
      )
    )
