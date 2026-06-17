package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class TupleSuite extends ScalanotationSuite:
  test("tuple typeclass instances round-trip through Expr and text"):
    type Data = (Int, String, (ok: Boolean), Vector[Long])
    val value: Data = (1, "two", (ok = true), Vector(3L, 4L))

    summon[Reader[Data]].schema match
      case tuple: RawSchema.Tuple =>
        assertEquals(tuple.slots.length, 4)
        tuple.read match
          case _: PublicInternal.BuildTupleSlots[?] =>
            ()
          case _ =>
            fail(s"Expected a tuple reader builder, got ${tuple.read}")
      case other =>
        fail(s"Expected a tuple reader, got ${other.describeSelf}")

    summon[Writer[Data]].schema match
      case tuple: RawSchema.Tuple =>
        assertEquals(tuple.slots.length, 4)
        assertEquals(tuple.write, RawSchema.TupleWrite.productLike)
      case other =>
        fail(s"Expected a tuple writer, got ${other.describeSelf}")

    val expr         = Writers.writeExpr(value)
    val expectedExpr = Expr.TupleExpr(
      IndexedSeq(
        Expr.IntConstant(1),
        Expr.StringConstant("two"),
        Expr.NamedTupleExpr(IndexedSeq("ok" -> Expr.BooleanConstant(true))),
        Expr.VectorExpr(IndexedSeq(Expr.LongConstant(3L), Expr.LongConstant(4L)))
      )
    )
    val rendered = Writers.write(value)

    assertEquals(expr, expectedExpr)
    assertEquals(expr.decodeAs[Data], Result.Ok(value))
    assertEquals(rendered, """(1, "two", (ok = true), Vector(3L, 4L))""")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))
    assertEquals(summon[ReadWriter[Data]].schema.describeSelf, "(..., ..., ..., ...)")

  test("mapped tuple ReadWriter round-trips through Expr and text"):
    final case class Pair(count: Int, label: String)

    given ReadWriter[Pair] =
      summon[ReadWriter[(Int, String)]].bimapResult { case (count, label) =>
        Result.Ok(Pair(count, label))
      }(pair => (pair.count, pair.label))

    val value    = Pair(7, "seven")
    val expr     = Writers.writeExpr(value)
    val rendered = Writers.write(value)

    assertEquals(
      expr,
      Expr.TupleExpr(IndexedSeq(Expr.IntConstant(7), Expr.StringConstant("seven")))
    )
    assertEquals(expr.decodeAs[Pair], Result.Ok(value))
    assertEquals(rendered, """(7, "seven")""")
    assertEquals(Readers.readAs[Pair](rendered), Result.Ok(value))

  test("tuple typeclass instances support arities above Tuple22"):
    type Data =
      (
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
      )
    val value: Data =
      (
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
      )
    val rendered = Writers.write(value)

    summon[Reader[Data]].schema match
      case tuple: RawSchema.Tuple =>
        assertEquals(tuple.slots.length, 23)
      case other =>
        fail(s"Expected a tuple reader, got ${other.describeSelf}")

    assertEquals(
      Writers.writeExpr(value),
      Expr.TupleExpr((1 to 23).map(Expr.IntConstant.apply).toIndexedSeq)
    )
    assertEquals(
      rendered,
      "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)"
    )
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

  test("tuple decodes support EmptyTuple and singleton syntax"):
    assertEquals(Readers.readAs[EmptyTuple]("EmptyTuple"), Result.Ok(EmptyTuple))
    assertEquals(Writers.write(EmptyTuple), "EmptyTuple")

    val singleton: Int *: EmptyTuple = 1 *: EmptyTuple
    assertEquals(
      Readers.readAs[Int *: EmptyTuple]("Tuple(1)"),
      Result.Ok(singleton)
    )
    assertEquals(Writers.write(singleton), "Tuple(1)")
    assertEquals(
      Writers.write(Expr.TupleExpr(IndexedSeq(Expr.TupleExpr(IndexedSeq(Expr.IntConstant(1)))))),
      "Tuple(Tuple(1))"
    )
    assertEquals(
      Writers.write((1 *: EmptyTuple) *: EmptyTuple),
      "Tuple(Tuple(1))"
    )
    assertEquals(
      Readers.readAs[(String, Int)]("""("foo" + "bar", 7)"""),
      Result.Ok(("foobar", 7))
    )
    assertEquals(
      Readers.readAs[(Int *: EmptyTuple, String)](
        """(Tuple(1), "abc")"""
      ),
      Result.Ok((1 *: EmptyTuple, "abc"))
    )

  test("nested tuple decodes support comma syntax"):
    type Data = ((Int, String), (Boolean, Long), (Int, (String, Boolean)))
    val expected: Data = ((1, "two"), (true, 4L), (5, ("six", false)))

    assertEquals(
      Readers.readAs[Data]("""((1, "two"), (true, 4L), (5, ("six", false)))"""),
      Result.Ok(expected)
    )

  test("parenthesized single values are not tuple decodes"):
    Readers.readAs[(Int, String)]("(1)") match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("(..., ...)", "(...)")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    val singletonReader = Reader.fromSchema[Int *: EmptyTuple](
      RawSchema.Tuple(
        IArray(RawSchema.Int),
        PublicInternal.BuildTuple[Int *: EmptyTuple],
        write = null
      )
    )

    Readers.readAs[Int *: EmptyTuple]("(1)")(using singletonReader) match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Tuple(...)", "(...)")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    assertEquals(
      Readers.readAs[Int *: EmptyTuple]("Tuple(1)")(using singletonReader),
      Result.Ok(1 *: EmptyTuple)
    )

    Readers.readAs[Int *: EmptyTuple]("(1, 2)")(using singletonReader) match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.FieldCountMismatch(1, 2))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("no tuple reader is derived when a slot lacks evidence"):
    val errors = typeCheckErrors(
      "class Box\nsummon[scalanotation.Reader[(Int, String, Box)]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("[2]"))
    assert(clue(errors.head.message).contains("Box"))

  test("tuple typeclass instances are derived for EmptyTuple and singleton tuples"):
    assertEquals(summon[Reader[EmptyTuple]].schema.describeSelf, "EmptyTuple")
    assertEquals(summon[Writer[EmptyTuple]].schema.describeSelf, "EmptyTuple")
    assertEquals(summon[ReadWriter[EmptyTuple]].schema.describeSelf, "EmptyTuple")

    assertEquals(summon[Reader[Int *: EmptyTuple]].schema.describeSelf, "Tuple(...)")
    assertEquals(summon[Writer[Int *: EmptyTuple]].schema.describeSelf, "Tuple(...)")
    assertEquals(summon[ReadWriter[Int *: EmptyTuple]].schema.describeSelf, "Tuple(...)")
