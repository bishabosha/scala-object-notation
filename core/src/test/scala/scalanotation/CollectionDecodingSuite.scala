package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class CollectionDecodingSuite extends ScalanotationSuite:
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
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assert(read.builder.isInstanceOf[PublicInternal.BuildIntArray])
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
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assert(read.builder.isInstanceOf[PublicInternal.BuildIntArray])
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
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assertEquals(read.builder.getClass.getSimpleName, "SeqFactoryVector")
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
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assert(read.builder.isInstanceOf[PublicInternal.BuildVector[?]])
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
          case read: RawSchema.DictRead.FromReaderBuilder[?, ?, ?] =>
            assertEquals(read.builder.getClass.getSimpleName, "MapFactoryDict")
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
