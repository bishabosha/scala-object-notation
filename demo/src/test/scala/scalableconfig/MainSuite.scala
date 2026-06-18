package scalableconfig

import munit.FunSuite
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Tag
import scalanotation.*
import steps.result.Result

import FormatSchemas.given

class MainSuite extends FunSuite:
  test("render json export"):
    val ast = Readers.quick.readDecls(
      """val data = (
        |  x = Vector(1, 2),
        |  y = null,
        |  ok = true,
        |  label = "abc"
        |)
        |""".stripMargin
    )

    val obtained = ujson.read(
      Main
        .render(
          ast,
          name = "data",
          exportJson = true,
          exportYaml = false,
          preserveNums = false
        )
        .get
    )
    val expected: ujson.Value = ujson.Obj(
      "x"     -> ujson.Arr(1, 2),
      "y"     -> ujson.Null,
      "ok"    -> ujson.Bool(true),
      "label" -> ujson.Str("abc")
    )

    assertEquals(obtained, expected)

  test("render yaml export"):
    val ast = Readers.quick.readDecls(
      """val data = (
        |  x = Vector(1, 2),
        |  y = null,
        |  ok = true,
        |  label = "abc"
        |)
        |""".stripMargin
    )

    def trimLines(s: String): String =
      s.linesIterator.map(_.trim).mkString("\n")
    val obtained = Main
      .render(
        ast,
        name = "data",
        exportJson = false,
        exportYaml = true,
        preserveNums = false
      )
      .get
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
    val ast = Readers.quick.readDecls(
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

    val node                          = Main.exprToYamlNode(ast.declarations.head(1))
    val Node.MappingNode(mappings, _) = node: @unchecked

    val byName = mappings.collect {
      case (Node.ScalarNode(name, _), value @ Node.ScalarNode(_, _)) =>
        name -> value
    }.toMap

    assertEquals(byName("i").tag, Tag.int)
    assertEquals(byName("l").tag, Tag.int)
    assertEquals(byName("d").tag, Tag.float)
    assertEquals(byName("f").tag, Tag.float)
    assertEquals(byName("b").tag, Tag.boolean)
    assertEquals(byName("s").tag, Tag.str)
    assertEquals(byName("n").tag, Tag.nullTag)

  test("ujson values decode through the recursive router schema"):
    val input =
      """(
        |  items = Vector(1, "two", null),
        |  tuple = Tuple(true),
        |  ok = false
        |)
        |""".stripMargin

    val expected: ujson.Value = ujson.Obj(
      "items" -> ujson.Arr(1, "two", ujson.Null),
      "tuple" -> ujson.Arr(true),
      "ok"    -> ujson.Bool(false)
    )

    assertEquals(Readers.readAs[ujson.Value](input), Result.Ok(expected))
    assertEquals(Readers.readAs[ujson.Value](Writers.write(expected)), Result.Ok(expected))

  test("yaml nodes decode through the recursive router schema"):
    val input =
      """(
        |  items = Vector(1, "two", null),
        |  tuple = Tuple(true),
        |  ok = false
        |)
        |""".stripMargin

    Readers.readAs[Node](input) match
      case Result.Ok(node: Node.MappingNode) =>
        val byName = node.mappings.collect { case (Node.ScalarNode(name, _), value) =>
          name -> value
        }.toMap
        assertEquals(byName("items").tag, Tag.seq)
        assertEquals(byName("tuple").tag, Tag.seq)
        assertEquals(byName("ok").tag, Tag.boolean)
        assertEquals(Readers.readAs[Node](Writers.write(node: Node)), Result.Ok(node: Node))
      case other =>
        fail(s"Expected a YAML mapping node, got $other")

  test("format adapters derive external typeclasses from scalanotation ReadWriter"):
    final case class Service(port: Int, label: String)

    given sonServiceReadWriter: ReadWriter[Service]                     = ReadWriter.derived
    given upickleServiceReadWriter: upickle.default.ReadWriter[Service] =
      FormatSchemas.upickleReadWriter[Service]

    val value = Service(8080, "dev")
    val json  = upickle.default.writeJs(value)
    assertEquals(
      json,
      ujson.Obj("port" -> ujson.Num(8080), "label" -> ujson.Str("dev"))
    )
    assertEquals(upickle.default.read[Service](json), value)
    assertEquals(upickle.default.write(value), """{"port":8080,"label":"dev"}""")
    assertEquals(upickle.default.read[Service]("""{"port":8080,"label":"dev"}"""), value)

    val yamlEncoder = FormatSchemas.yamlEncoder[Service]
    val yamlDecoder = FormatSchemas.yamlDecoder[Service]
    val yamlNode    = yamlEncoder.asNode(value)

    assertEquals(yamlDecoder.construct(yamlNode), Right(value))

  test("upickle adapter streams nested products, collections, options, and sums"):
    final case class Metadata(tags: Vector[String], retry: Option[Int]) derives ReadWriter
    enum Mode derives ReadWriter:
      case Fast
      case Scheduled(metadata: Metadata)
    final case class Deployment(name: String, mode: Mode, ports: Vector[Int]) derives ReadWriter

    given upickle.default.ReadWriter[Deployment] =
      FormatSchemas.upickleReadWriter[Deployment]

    val value = Deployment(
      "api",
      Mode.Scheduled(Metadata(Vector("blue", "green"), Some(2))),
      Vector(8080, 9090)
    )
    val json =
      """{"name":"api","mode":{"Scheduled":{"metadata":{"tags":["blue","green"],"retry":2}}},"ports":[8080,9090]}"""
    val son =
      """(name="api",mode=(Scheduled=(metadata=(tags=Vector("blue","green"),retry=2))),ports=Vector(8080,9090))"""

    assertEquals(upickle.default.read[Deployment](json), value)
    assertEquals(upickle.default.read[Deployment](upickle.default.write(value)), value)

    val compactFormat = TextFormat.compact(spacing = 0)
    val text          = Writers.write(value, compactFormat)
    assertEquals(text, son)
