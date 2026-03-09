package miniparser

import munit.FunSuite
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Tag

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

    val obtained = ujson.read(Main.render(ast, name = "data", exportJson = true, exportYaml = false, preserveNums = false))
    val expected = ujson.Obj(
      "x" -> ujson.Arr(1, 2),
      "y" -> ujson.Null,
      "ok" -> ujson.Bool(true),
      "label" -> ujson.Str("abc")
    )

    assertEquals(obtained, expected)

  test("render yaml export"):
    val ast = Parser.parse(
      """val data = (
        |  x = Vector(1, 2),
        |  y = null,
        |  ok = true,
        |  label = "abc"
        |)
        |""".stripMargin
    )

    val obtained = Main.render(ast, name = "data", exportJson = false, exportYaml = true, preserveNums = false)
    val expected =
      """x: 
        |  - 1
        |  - 2
        |y: !!null
        |ok: true
        |label: abc
        |""".stripMargin

    assertEquals(obtained, expected)

  test("yaml export uses explicit scalar tags"):
    val ast = Parser.parse(
      """val data = (
        |  i = 1,
        |  l = 2L,
        |  keep = 3.5,
        |  b = true,
        |  s = "abc",
        |  n = null
        |)
        |""".stripMargin
    )

    val node = Main.exprToYamlNode(ast.declaration.value, preserveNums = true)
    val Node.MappingNode(mappings, _) = node: @unchecked

    val byName = mappings.collect { case (Node.ScalarNode(name, _), value) => name -> value }.toMap

    assertEquals(byName("i").asInstanceOf[Node.ScalarNode].tag, Tag.int)
    assertEquals(byName("l").asInstanceOf[Node.ScalarNode].tag, Tag.str)
    assertEquals(byName("keep").asInstanceOf[Node.ScalarNode].tag, Tag.str)
    assertEquals(byName("b").asInstanceOf[Node.ScalarNode].tag, Tag.boolean)
    assertEquals(byName("s").asInstanceOf[Node.ScalarNode].tag, Tag.str)
    assertEquals(byName("n").asInstanceOf[Node.ScalarNode].tag, Tag.nullTag)
