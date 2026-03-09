package miniparser

import munit.FunSuite

class MainSuite extends FunSuite:
  test("render json export"):
    val ast = Parser.parse(
      """val data = (
        |  x = Vector(1, 2),
        |  y = null,
        |  ok = true,
        |  label = "abc"
        |)
        |""".stripMargin
    )

    val obtained = ujson.read(Main.render(ast, name = "data", exportJson = true, preserveNums = false))
    val expected = ujson.Obj(
      "x" -> ujson.Arr(1, 2),
      "y" -> ujson.Null,
      "ok" -> ujson.Bool(true),
      "label" -> ujson.Str("abc")
    )

    assertEquals(obtained, expected)
