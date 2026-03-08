package miniparser

import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

class ParserSuite extends FunSuite:
  test("parse the sample named tuple file"):
    val input =
      """val data = (
        |  x = (
        |    ls = Vector("abc" + "def", 'b', 123, 3.1, 4.1f, 23L),
        |    ys = Vector(-1, -0b0000_0011, -0x00_1A),
        |  ),
        |  y = null
        |)
        |""".stripMargin

    val parsed = Parser.parse(input)

    val expected: PartialFunction[SourceFile, Unit] = {
      case SourceFile(
        ValDecl(
          "data",
          Expr.NamedTupleExpr(
            IArray("x", "y"),
            IArray(
              Expr.NamedTupleExpr(
                IArray("ls", "ys"),
                IArray(
                  Expr.VectorExpr(
                    IArray(
                      Expr.StringConstant("abcdef"),
                      Expr.CharConstant('b'),
                      Expr.IntConstant(123),
                      Expr.DoubleConstant(3.1d),
                      Expr.FloatConstant(4.1f),
                      Expr.LongConstant(23L)
                    )
                  ),
                  Expr.VectorExpr(
                    IArray(
                      Expr.IntConstant(-1),
                      Expr.IntConstant(-0b0000_0011),
                      Expr.IntConstant(-0x00_1A)
                    )
                  )
                )
              ),
              Expr.NullConstant
            )
          )
        )
      ) => () }


    assert(expected.isDefinedAt(parsed))

  test("tokenize booleans and negative numbers"):
    val input = "val data = (a = true, b = false, c = -12, d = -1.5f)"
    val parsed = Parser.parse(input)

    val Expr.NamedTupleExpr(_, elements) = parsed.declaration.value: @unchecked
    assertEquals(elements.length, 4)

  test("decode directly into a typed named tuple"):
    type Data =
      (x: (label: String, ys: Vector[Int]), y: Null, ok: Boolean)

    val input =
      """val data = (
        |  ok = true,
        |  y = null,
        |  x = (
        |    ys = Vector(-1, -0b0000_0011, -0x00_1A),
        |    label = "abc" + "def"
        |  )
        |)
        |""".stripMargin

    val decoded = Parser.parseNamedTupleAs[Data](input)
    val expected: Data =
      (x = (label = "abcdef", ys = Vector(-1, -3, -26)), y = null, ok = true)

    assertEquals(decoded, Right(expected))

  test("decode vectors of nested named tuples"):
    type Entry = (name: String, value: Int)
    type Data = (items: Vector[Entry], total: Long)

    val input =
      """val data = (
        |  total = 2L,
        |  items = Vector(
        |    (value = 1, name = "a"),
        |    (name = "b", value = 2)
        |  )
        |)
        |""".stripMargin

    val decoded = Parser.parseNamedTupleAs[Data](input)
    val expected: Data =
      (items = Vector((name = "a", value = 1), (name = "b", value = 2)), total = 2L)

    assertEquals(decoded, Right(expected))

  test("report schema mismatches during decoding"):
    type Data = (x: Int)

    val input = "val data = (x = true)"

    assertEquals(
      Parser.parseNamedTupleAs[Data](input),
      Left(DecodeError.FieldError("x", DecodeError.ExpectedInt(Expr.BooleanConstant(true))))
    )

  test("no decoder is derived for Any"):
    val errors = typeCheckErrors("summon[miniparser.AstDecoder[Any]]")
    assert(errors.nonEmpty)

  test("no decoder is derived for Vector[Any]"):
    val errors = typeCheckErrors("summon[miniparser.AstDecoder[Vector[Any]]]")
    assert(errors.nonEmpty)
