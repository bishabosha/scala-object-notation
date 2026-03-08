package miniparser

import munit.FunSuite

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
