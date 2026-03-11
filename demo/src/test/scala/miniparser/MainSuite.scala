package scalanotation

import munit.FunSuite
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Tag

class MainSuite extends FunSuite:
  test("render json export"):
    val ast = Parser.quick.parse(
      """val data = (
        |  x = Vector(1, 2),
        |  y = null,
        |  ok = true,
        |  label = "abc"
        |)
        |""".stripMargin
    )

    val obtained = ujson.read(Main.render(ast, name = "data", exportJson = true, exportYaml = false, preserveNums = false).get)
    val expected = ujson.Obj(
      "x" -> ujson.Arr(1, 2),
      "y" -> ujson.Null,
      "ok" -> ujson.Bool(true),
      "label" -> ujson.Str("abc")
    )

    assertEquals(obtained, expected)

  test("render yaml export"):
    val ast = Parser.quick.parse(
      """val data = (
        |  x = Vector(1, 2),
        |  y = null,
        |  ok = true,
        |  label = "abc"
        |)
        |""".stripMargin
    )

    def trimLines(s: String): String = s.linesIterator.map(_.trim).mkString("\n")
    val obtained = Main.render(ast, name = "data", exportJson = false, exportYaml = true, preserveNums = false).get
    val expected =
      """x:
        |  - 1
        |  - 2
        |y: !!null
        |ok: true
        |label: abc
        |""".stripMargin

    assertEquals(trimLines(obtained), trimLines(expected))

  test("yaml export uses explicit scalar tags"):
    val ast = Parser.quick.parse(
      """val data = (
        |  i = 1,
        |  l = 2L,
        |  d = 3.5,
        |  f = 4.5f,
        |  b = true,
        |  s = "abc",
        |  n = null
        |)
        |""".stripMargin
    )

    val node = Main.exprToYamlNode(ast.declaration.value)
    val Node.MappingNode(mappings, _) = node: @unchecked

    val byName = mappings.collect {
      case (Node.ScalarNode(name, _), value @ Node.ScalarNode(_, _)) => name -> value
    }.toMap

    assertEquals(byName("i").tag, Tag.int)
    assertEquals(byName("l").tag, Tag.int)
    assertEquals(byName("d").tag, Tag.float)
    assertEquals(byName("f").tag, Tag.float)
    assertEquals(byName("b").tag, Tag.boolean)
    assertEquals(byName("s").tag, Tag.str)
    assertEquals(byName("n").tag, Tag.nullTag)
