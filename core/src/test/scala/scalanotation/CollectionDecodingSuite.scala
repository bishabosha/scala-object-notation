package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.collection.immutable.ListMap
import scala.compiletime.testing.typeCheckErrors

class CollectionDecodingSuite extends ScalanotationSuite:
  private final class PrimitiveIntTable(
      private val keys: Array[Int],
      private val values: Array[Int],
      val size: Int,
      val stagedKeys: Int,
      val valueAdds: Int,
      val probes: Int
  ):
    def contains(key: Int): Boolean =
      findSlot(key) >= 0

    def valueOr(key: Int, fallback: Int): Int =
      val index = findSlot(key)
      if index >= 0 then values(index)
      else fallback

    private def findSlot(key: Int): Int =
      val mask = keys.length - 1
      var idx  = key & mask
      while keys(idx) != PrimitiveIntTable.EmptyKey do
        if keys(idx) == key then return idx
        idx = (idx + 1) & mask
      -1

  private object PrimitiveIntTable:
    final val EmptyKey = 0

  private final class PrimitiveIntTableBuilder:
    private val keys   = new Array[Int](8)
    private val values = new Array[Int](8)
    private var size0  = 0
    private var probes = 0

    var pendingKey: Int = 0
    var stagedKeys: Int = 0
    var valueAdds: Int  = 0

    def stageKey(key: Int): Unit =
      pendingKey = key
      stagedKeys += 1

    def putRaw(key: Int, value: Int): Unit =
      if key == PrimitiveIntTable.EmptyKey then
        throw IllegalArgumentException("0 is reserved as the empty key")
      val mask = keys.length - 1
      var idx  = key & mask
      var seen = 1
      while keys(idx) != PrimitiveIntTable.EmptyKey && keys(idx) != key do
        idx = (idx + 1) & mask
        seen += 1
      if keys(idx) == PrimitiveIntTable.EmptyKey then size0 += 1
      keys(idx) = key
      values(idx) = value
      probes += seen

    def addValue(value: Int): Unit =
      putRaw(pendingKey, value)
      valueAdds += 1

    def result(): PrimitiveIntTable =
      new PrimitiveIntTable(keys, values, size0, stagedKeys, valueAdds, probes)

  test("decode directly into a typed named tuple"):
    type Data =
      (x: (label: String, ys: Vector[Int]), y: Option[Int], ok: Boolean)

    val input =
      """val data = (
        |  x = (
        |    label = "abc" + "def",
        |    ys = Vector(-1, -0b0000_0011, -0x00_1A)
        |  ),
        |  y = 23,
        |  ok = true
        |)
        |""".stripMargin

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data =
      (
        x = (label = "abcdef", ys = Vector(-1, -3, -26)),
        y = Some(23),
        ok = true
      )

    assertEquals(decoded, Result.Ok(expected))

  test("decode null as None and values as Some"):
    type Data = (missing: Option[Int], present: Option[Int])

    val input =
      """val data = (
        |  missing = null,
        |  present = 41
        |)
        |""".stripMargin

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data = (missing = None, present = Some(41))

    assertEquals(decoded, Result.Ok(expected))

  test("report inner schema mismatches for Option values"):
    type Data = (x: Option[Int])

    val input    = "val data = (x = true)"
    val obtained = Readers.readDeclAs[Data](input, rootName = "data")

    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".x"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "'true'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("parse dense immutable arrays"):
    type Data = IArray[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case _: PublicInternal.BuildIntArray =>
            ()
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case IArray(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )

  test("parse dense arrays"):
    type Data = Array[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case _: PublicInternal.BuildIntArray =>
            ()
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case Array(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )

  test("parse arbitrary sequence") {
    type Data = List[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case read: PublicInternal.SeqFactoryVector[?, ?] =>
            assertEquals(read.getClass.getSimpleName, "SeqFactoryVector")
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case List(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )
  }

  test("parse specialized vector") {
    type Data = Vector[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case _: PublicInternal.BuildVector[?] =>
            ()
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case Vector(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )
  }

  test("parse arbitrary map") {
    type Data = mutable.LinkedHashMap[String, Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case dict: RawSchema.Dict =>
        dict.read match
          case read: PublicInternal.MapFactoryDict[?, ?] =>
            assertEquals(read.getClass.getSimpleName, "MapFactoryDict")
          case _ =>
            fail(s"Expected a dict reader builder, got ${dict.read}")
      case _ =>
        fail(s"Expected a dict reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = (x = 1, y = 2, z = 3)
        |""".stripMargin

    val decoded  = Readers.readDeclAs[Data](input, rootName = "data")
    val expected = mutable.LinkedHashMap("x" -> 1, "y" -> 2, "z" -> 3)
    assertEquals(
      expected,
      decoded.getOrElse(fail(s"Expected successful parse, got $decoded"))
    )
  }

  test("round-trip arbitrary immutable map keys as vector tuple pairs"):
    type Data = ListMap[Int, String]

    summon[ReadWriter[Data]].schema match
      case pairSeq: RawSchema.PairSeq =>
        assert(pairSeq.read != null)
        assert(pairSeq.write != null)
        assertEquals(pairSeq.key, RawSchema.Int)
        assertEquals(pairSeq.value, RawSchema.String)
      case other =>
        fail(s"Expected a pair sequence schema, got ${other.describeSelf}")

    val value: Data = ListMap(1 -> "one", 2 -> "two")
    val rendered    = Writers.write(value)
    val expr        = Writers.writeExpr(value)

    assertEquals(rendered, """Vector((1, "one"), (2, "two"))""")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))
    assertEquals(expr.decodeAs[Data], Result.Ok(value))

  test("round-trip arbitrary mutable map keys as vector tuple pairs"):
    type Data = mutable.LinkedHashMap[Int, String]

    val value: Data = mutable.LinkedHashMap(2 -> "two", 1 -> "one")
    val rendered    = Writers.write(value)
    val decoded     = Readers.readAs[Data](rendered)

    assertEquals(rendered, """Vector((2, "two"), (1, "one"))""")
    decoded match
      case Result.Ok(map) =>
        assert(map.isInstanceOf[mutable.LinkedHashMap[?, ?]])
        assertEquals(map.toVector, value.toVector)
      case Result.Err(error) =>
        fail(s"Expected successful parse, got $error")

  test("pair sequence readers can build primitive open-addressed tables statefully"):
    object PrimitiveHashTableRead
        extends Reader.PairSeqBuilder[Int, Int, PrimitiveIntTableBuilder, PrimitiveIntTable]:
      def init(): PrimitiveIntTableBuilder = new PrimitiveIntTableBuilder

      def addKey(state: PrimitiveIntTableBuilder, key: Int): PrimitiveIntTableBuilder =
        fail(s"Expected an Int key slot, got $key")

      override def addIntKey(
          state: PrimitiveIntTableBuilder,
          key: Int
      ): PrimitiveIntTableBuilder =
        state.stageKey(key)
        state

      def addValue(state: PrimitiveIntTableBuilder, elem: Int): PrimitiveIntTableBuilder =
        fail(s"Expected an Int value slot, got $elem")

      override def addIntValue(
          state: PrimitiveIntTableBuilder,
          elem: Int
      ): PrimitiveIntTableBuilder =
        state.addValue(elem)
        state

      def finish(state: PrimitiveIntTableBuilder): PrimitiveIntTable =
        state.result()

    given Reader[PrimitiveIntTable] = Reader.fromSchema(
      RawSchema.PairSeq(RawSchema.Int, RawSchema.Int, PrimitiveHashTableRead)
    )

    def assertDecoded(obtained: Result[PrimitiveIntTable, DecodeError]): Unit =
      obtained match
        case Result.Ok(table) =>
          assertEquals(table.size, 3)
          assertEquals(table.valueOr(1, -1), 10)
          assertEquals(table.valueOr(9, -1), 90)
          assertEquals(table.valueOr(17, -1), 170)
          assert(!table.contains(2))
          assertEquals(table.stagedKeys, 3)
          assertEquals(table.valueAdds, 3)
          assertEquals(table.probes, 6)
        case Result.Err(error) =>
          fail(s"Expected successful parse, got $error")

    val expr = Expr.VectorExpr(
      IndexedSeq(
        Expr.TupleExpr(IndexedSeq(Expr.IntConstant(1), Expr.IntConstant(10))),
        Expr.TupleExpr(IndexedSeq(Expr.IntConstant(9), Expr.IntConstant(90))),
        Expr.TupleExpr(IndexedSeq(Expr.IntConstant(17), Expr.IntConstant(170)))
      )
    )

    assertDecoded(Readers.readAs[PrimitiveIntTable]("Vector((1, 10), (9, 90), (17, 170))"))
    assertDecoded(expr.decodeAs[PrimitiveIntTable])

  test("public vector constructor builds a read-writer from a builder"):
    final case class IntList(values: Vector[Int], addIntCalls: Int)

    final class IntListState:
      val values: scala.collection.mutable.Builder[Int, Vector[Int]] =
        Vector.newBuilder[Int]
      var addIntCalls: Int = 0

    object IntListBuilder extends Reader.VectorBuilder[Int, IntListState, IntList]:
      def init(): IntListState =
        new IntListState

      def add(
          repr: IntListState,
          elem: Int
      ): IntListState =
        fail(s"Expected addInt to be called for vector element $elem")

      override def addInt(
          repr: IntListState,
          elem: Int
      ): IntListState =
        repr.values.addOne(elem)
        repr.addIntCalls += 1
        repr

      def finish(repr: IntListState): IntList =
        IntList(repr.values.result(), repr.addIntCalls)

    val readWriter =
      ReadWriter.vector(
        summon[ReadWriter[Int]],
        IntListBuilder,
        _.values.length,
        _.values.iterator
      )
    given ReadWriter[IntList] = readWriter

    readWriter.schema match
      case vector: RawSchema.Vector =>
        assert(vector.read.asInstanceOf[AnyRef] eq IntListBuilder)
      case other =>
        fail(s"Expected a vector schema, got ${other.describeSelf}")

    val value = IntList(Vector(1, 2, 3), addIntCalls = 0)

    assertEquals(Writers.write(value), "Vector(1, 2, 3)")
    assertEquals(
      Readers.readAs[IntList]("Vector(1, 2, 3)"),
      Result.Ok(IntList(Vector(1, 2, 3), addIntCalls = 3))
    )

  test("public pair sequence reader constructor builds a primitive open-addressed table"):
    object PrimitiveMapBuilder
        extends Reader.PairSeqBuilder[Int, Int, PrimitiveIntTableBuilder, PrimitiveIntTable]:
      def init(): PrimitiveIntTableBuilder =
        new PrimitiveIntTableBuilder

      def addKey(repr: PrimitiveIntTableBuilder, key: Int): PrimitiveIntTableBuilder =
        fail(s"Expected an Int key slot, got $key")

      override def addIntKey(repr: PrimitiveIntTableBuilder, key: Int): PrimitiveIntTableBuilder =
        repr.stageKey(key)
        repr

      def addValue(repr: PrimitiveIntTableBuilder, elem: Int): PrimitiveIntTableBuilder =
        fail(s"Expected an Int value slot, got $elem")

      override def addIntValue(
          repr: PrimitiveIntTableBuilder,
          elem: Int
      ): PrimitiveIntTableBuilder =
        repr.addValue(elem)
        repr

      def finish(repr: PrimitiveIntTableBuilder): PrimitiveIntTable =
        repr.result()

    val reader =
      Reader.pairSeq(
        summon[Reader[Int]],
        summon[Reader[Int]],
        PrimitiveMapBuilder
      )
    given Reader[PrimitiveIntTable] = reader

    reader.schema match
      case pairSeq: RawSchema.PairSeq =>
        assert(pairSeq.read.asInstanceOf[AnyRef] eq PrimitiveMapBuilder)
      case other =>
        fail(s"Expected a pair sequence schema, got ${other.describeSelf}")

    Readers.readAs[PrimitiveIntTable]("Vector((1, 10), (9, 90))") match
      case Result.Ok(decoded) =>
        assertEquals(decoded.size, 2)
        assertEquals(decoded.valueOr(1, -1), 10)
        assertEquals(decoded.valueOr(9, -1), 90)
        assert(!decoded.contains(17))
        assertEquals(decoded.stagedKeys, 2)
        assertEquals(decoded.valueAdds, 2)
        assertEquals(decoded.probes, 3)
      case Result.Err(error) =>
        fail(s"Expected successful parse, got $error")

  test("string-key maps keep named tuple dict syntax"):
    type Data = mutable.LinkedHashMap[String, Int]

    summon[ReadWriter[Data]].schema match
      case dict: RawSchema.Dict =>
        assert(dict.read != null)
        assert(dict.write != null)
      case other =>
        fail(s"Expected a dict schema, got ${other.describeSelf}")

    val value    = mutable.LinkedHashMap("x" -> 1, "y" -> 2)
    val rendered = Writers.write(value)

    assertEquals(rendered, "(x = 1, y = 2)")
    assertEquals(Readers.readAs[Data](rendered).map(_.toVector), Result.Ok(value.toVector))

  test("decode a dict with far more keys than the intern table holds"):
    // the intern table is only active in batched mode (one-shot decodes bypass it). With far more
    // distinct keys than table slots, keys collide and overwrite cached entries, so this fails if a
    // dropped entry corrupted a name or aliased two keys
    type Data = mutable.LinkedHashMap[String, Int]

    given BatchContext = BatchContext.local()

    val entries = (0 until 5000).map(i => s"field_$i" -> i)
    val input   = entries.map((k, v) => s"  $k = $v").mkString("val data = (\n", ",\n", "\n)\n")

    val decoded  = Readers.batched.readDeclAs[Data](input, rootName = "data")
    val expected = mutable.LinkedHashMap.from(entries)
    assertEquals(decoded, Result.Ok(expected))

  test("decode repeated field names through the warm intern cache"):
    // many records share one batch context, so the field names recur across decodes and hit the
    // warm cache; the hit path must yield value-equal names every time, even though the returned
    // instances need not be the same reference
    type Entry = (key: String, count: Int)

    given BatchContext = BatchContext.local()

    (0 until 200).foreach: i =>
      val input    = s"""val data = (key = "k$i", count = $i)"""
      val decoded  = Readers.batched.readDeclAs[Entry](input, rootName = "data")
      val expected = (key = s"k$i", count = i)
      assertEquals(decoded, Result.Ok(expected))

  test("decode vectors of nested named tuples"):
    type Entry = (name: String, value: Int)
    type Data  = (items: Vector[Entry], total: Long)

    val input =
      """val data = (
        |  items = Vector(
        |    (name = "a", value = 1),
        |    (name = "b", value = 2)
        |  ),
        |  total = 2L
        |)
        |""".stripMargin

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data =
      (
        items = Vector((name = "a", value = 1), (name = "b", value = 2)),
        total = 2L
      )

    assertEquals(decoded, Result.Ok(expected))

  test("report schema mismatches during decoding"):
    type Data = (x: Int)

    val input    = "val data = (x = true)"
    val obtained = Readers.readDeclAs[Data](input, rootName = "data")

    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".x"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "'true'")
        )
        assertEquals(
          error.span.map(span => (span.line, span.column)),
          Some((1, 17))
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("report full nested decode path through vectors"):
    type Data = (items: Vector[(value: Int)])

    val input =
      """val data = (
        |  items = Vector((value = true))
        |)
        |""".stripMargin

    val obtained = Readers.readDeclAs[Data](input, rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".items", "[0]", ".value"))
        assertEquals(true, error.format.contains("'.items[0].value'"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "'true'")
        )
        assertEquals(error.span.map(span => span.line), Some(2))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("report full nested decode path through vectors, validated"):
    type Data = (items: Vector[(value: Int)])

    val input =
      """val data = (
        |  items = Vector((value = true))
        |)
        |""".stripMargin

    val obtained  = Readers.readDeclAs[Expr](input, rootName = "data").get
    val validated = obtained.decodeAs[Data]
    validated match
      case Result.Err(error) =>
        assertEquals(error.path, List(".items", "[0]", ".value"))
        assertEquals(true, error.format.contains("'.items[0].value'"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "(true: Boolean)")
        )
        assertEquals(error.span, None)
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("nest Expr inside of structured type"):
    type Data = (x: Int, y: (q: Vector[Expr]))

    val input                                 = "val data = (x = 23, y = (q = Vector(41)))"
    val obtained                              = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = {
      case (
            x = 23,
            y = (q = Vector(Expr.IntConstant(41)))
          ) =>
        ()
    }
    assert(
      expected.isDefinedAt(
        obtained.getOrElse(fail(s"Expected successful decode, got $obtained"))
      )
    )

  test("reject swapped named tuple field order"):
    type Data = (x: Int, y: Boolean)

    val input = "val data = (y = true, x = 1)"

    val obtained = Readers.readDeclAs[Data](input, rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.FieldOrderMismatch("x", "y"))
        assertEquals(
          error.span.map(span => (span.line, span.column)),
          Some((1, 13))
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")
