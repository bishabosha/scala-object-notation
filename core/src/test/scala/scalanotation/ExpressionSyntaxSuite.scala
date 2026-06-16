package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class ExpressionSyntaxSuite extends ScalanotationSuite:
  test("read the sample named tuple file"):
    val input =
      """val data = (
        |  x = (
        |    ls = Vector("abc" + "def", 'b', 123, 3.1, 4.1f, 23L),
        |    ys = Vector(-1, -0b0000_0011, -0x00_1A),
        |  ),
        |  y = null
        |)
        |""".stripMargin

    val parsed = Readers.quick.readDecls(input)

    val expected = Expr.SourceFile(
      Map(
        "data" -> Expr.NamedTupleExpr(
          IndexedSeq(
            "x" -> Expr.NamedTupleExpr(
              IndexedSeq(
                "ls" -> Expr.VectorExpr(
                  IndexedSeq(
                    Expr.StringConstant("abcdef"),
                    Expr.CharConstant('b'),
                    Expr.IntConstant(123),
                    Expr.DoubleConstant(3.1d),
                    Expr.FloatConstant(4.1f),
                    Expr.LongConstant(23L)
                  )
                ),
                "ys" -> Expr.VectorExpr(
                  IndexedSeq(
                    Expr.IntConstant(-1),
                    Expr.IntConstant(-0b0000_0011),
                    Expr.IntConstant(-0x00_1a)
                  )
                )
              )
            ),
            "y" -> Expr.NullConstant
          )
        )
      )
    )
    assertEquals(parsed, expected)

  test("read just expression"):
    val input                           = "(a = true, b = false, c = -12, d = -1.5f)"
    val parsed                          = Readers.quick.read(input)
    val Expr.NamedTupleExpr(fieldExprs) = parsed.runtimeChecked
    assertEquals(fieldExprs.length, 4)

  test("read tuple literal expression"):
    val input  = """(1, "two", (ok = true), Vector(3, 4))"""
    val parsed = Readers.quick.read(input)

    val expected = Expr.TupleExpr(
      IndexedSeq(
        Expr.IntConstant(1),
        Expr.StringConstant("two"),
        Expr.NamedTupleExpr(IndexedSeq("ok" -> Expr.BooleanConstant(true))),
        Expr.VectorExpr(IndexedSeq(Expr.IntConstant(3), Expr.IntConstant(4)))
      )
    )

    assertEquals(parsed, expected)
    assertEquals(parsed.render, input)

  test("read singleton tuple literal expression"):
    val input  = """Tuple("abc")"""
    val parsed = Readers.quick.read(input)

    assertEquals(
      parsed,
      Expr.TupleExpr(
        IndexedSeq(
          Expr.StringConstant("abc")
        )
      )
    )
    assertEquals(parsed.render, input)
    assertEquals(
      Readers.quick.read("""Tuple(EmptyTuple)"""),
      Expr.TupleExpr(
        IndexedSeq(
          Expr.TupleExpr(IndexedSeq.empty)
        )
      )
    )
    assertEquals(
      Readers.quick.read("""Tuple((1, 2))"""),
      Expr.TupleExpr(
        IndexedSeq(
          Expr.TupleExpr(
            IndexedSeq(
              Expr.IntConstant(1),
              Expr.IntConstant(2)
            )
          )
        )
      )
    )

  test("describe tuple literal expressions with counted slots"):
    val obtained = Readers.quick.read("""(1, "two", true)""").decodeAs[Int]

    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedType("Int", "(..., ..., ...)"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("reject parenthesized single expressions"):
    Readers.readAs[Expr]("(1)") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedType("Tuple[...]", "(...)"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    Readers.readAs[Expr]("((1))") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedType("Tuple[...]", "(...)"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("reject singleton comma tuple literal expressions"):
    val inputs = List("(1,)")

    inputs.foreach { input =>
      Readers.readAs[Expr](input) match
        case Result.Err(error) =>
          assertEquals(error.rootCause, DecodeError.FieldCountMismatch(2, 1))
        case Result.Ok(value) => fail(s"Expected a decode failure for $input, got $value")
    }

  test("reject unit syntax as a tuple literal expression"):
    val obtained = Readers.readAs[Expr]("()")

    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.UnitValueNotAllowed())
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("read EmptyTuple as an expression"):
    val obtained = Readers.readAs[Expr]("EmptyTuple")

    assertEquals(obtained, Result.Ok(Expr.TupleExpr(IndexedSeq.empty)))
