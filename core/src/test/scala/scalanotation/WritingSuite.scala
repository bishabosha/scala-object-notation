package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.schema.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class WritingSuite extends ScalanotationSuite:
  test("write typed values to Expr and read them back"):
    type Data =
      (x: (label: String, ys: Vector[Int]), y: Option[Int], ok: Boolean)

    val value: Data =
      (
        x = (label = "abc", ys = Vector(-1, 2, 3)),
        y = Some(23),
        ok = true
      )

    val expr     = Writers.writeExpr(value)
    val rendered = Writers.write(value)
    val decoded  = expr.decodeAs[Data]
    val reparsed = Readers.readAs[Data](rendered)

    assertEquals(decoded, Result.Ok(value))
    assertEquals(reparsed, Result.Ok(value))
    assertEquals(rendered, """(x = (label = "abc", ys = Vector(-1, 2, 3)), y = 23, ok = true)""")

  test("write typed values with configurable token spacing"):
    type Data = (x: Int, nested: (ok: Boolean), items: Vector[Int])
    val value: Data = (x = 1, nested = (ok = true), items = Vector(1, 2))

    val compactFormat = TextFormat.compact(spacing = 0)
    val rendered      = Writers.write(value, format = compactFormat)
    val decl          = Writers.writeDecl("data", value, format = compactFormat)
    val packaged = Writers.writeDecl("data", value, packageName = "foo.bar", format = compactFormat)

    assertEquals(rendered, "(x=1,nested=(ok=true),items=Vector(1,2))")
    assertEquals(decl, "val data = (x=1,nested=(ok=true),items=Vector(1,2))")
    assertEquals(packaged, "package foo.bar;val data = (x=1,nested=(ok=true),items=Vector(1,2))")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))
    assertEquals(Readers.readDeclAs[Data](decl, rootName = "data"), Result.Ok(value))
    assertEquals(
      Readers.readDeclAs[Data](packaged, rootName = "data", packageName = "foo.bar"),
      Result.Ok(value)
    )

  test("write derived case classes and enums"):
    final case class Metadata(created: LocalDate, tags: Vector[String]) derives Reader, Writer
    enum Mode derives Reader, Writer:
      case Fast
      case Scheduled(at: LocalDate, retries: Int)
    final case class User(name: String, mode: Mode, metadata: Metadata) derives Reader, Writer

    given Reader[LocalDate] =
      summon[Reader[String]].mapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    given Writer[LocalDate] =
      summon[Writer[String]].contramap(_.toString)

    val value = User(
      name = "Ada",
      mode = Mode.Scheduled(LocalDate.parse("2026-03-15"), 2),
      metadata = Metadata(LocalDate.parse("2026-03-14"), Vector("compiler", "scala"))
    )

    val rendered = Writers.writeDecl("data", value)
    val decoded  = Readers.readDeclAs[User](rendered, rootName = "data")

    assertEquals(
      rendered,
      """val data = (name = "Ada", mode = (Scheduled = (at = "2026-03-15", retries = 2)), metadata = (created = "2026-03-14", tags = Vector("compiler", "scala")))"""
    )
    assertEquals(decoded, Result.Ok(value))

  test("write declarations with package statements"):
    type Data = (x: Int, ok: Boolean)
    val value: Data = (x = 1, ok = true)

    val compact = Writers.writeDecl("data", value, packageName = "foo.bar")
    val pretty  = Writers.writeDeclPretty("data", value, packageName = "foo.bar", indent = 2)

    assertEquals(compact, "package foo.bar; val data = (x = 1, ok = true)")
    assertEquals(
      pretty,
      """package foo.bar
        |val data = (
        |  x = 1,
        |  ok = true
        |)""".stripMargin
    )
    assertEquals(
      Readers.readDeclAs[Data](compact, rootName = "data", packageName = "foo.bar"),
      Result.Ok(value)
    )

  test("write strings and chars with escaping"):
    val rendered = Writers.write(
      (
        message = "line1\nline2\t\"quoted\"",
        mark = '\'',
        slash = '\\'
      )
    )

    type Data = (message: String, mark: Char, slash: Char)
    val decoded = Readers.readAs[Data](rendered)

    assertEquals(
      rendered,
      """(message = "line1\nline2\t\"quoted\"", mark = '\'', slash = '\\')"""
    )
    assertEquals(
      decoded,
      Result.Ok((message = "line1\nline2\t\"quoted\"", mark = '\'', slash = '\\'))
    )

  test("render quoted identifiers only when needed"):
    val expr = Expr.NamedTupleExpr(
      IndexedSeq(
        "type"        -> Expr.IntConstant(1),
        "has space"   -> Expr.IntConstant(2),
        "a-b"         -> Expr.IntConstant(3),
        "line\nbreak" -> Expr.IntConstant(4),
        "tick`name"   -> Expr.IntConstant(5),
        "Vector"      -> Expr.IntConstant(6),
        "empty_?"     -> Expr.IntConstant(7),
        "+"           -> Expr.IntConstant(8),
        "-"           -> Expr.IntConstant(9)
      )
    )

    val rendered        = expr.render
    val escapedBacktick = "\\" + "u0060"

    assertEquals(
      rendered,
      s"""(`type` = 1, `has space` = 2, `a-b` = 3, `line\\nbreak` = 4, `tick${escapedBacktick}name` = 5, Vector = 6, empty_? = 7, + = 8, - = 9)"""
    )
    assertEquals(Readers.quick.read(rendered), expr)

  test("derived writers quote hard-keyword field names"):
    final case class Data(`type`: Int, Vector: Int) derives ReadWriter

    val value    = Data(1, 2)
    val rendered = Writers.write(value)

    assertEquals(rendered, "(`type` = 1, Vector = 2)")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

  test("pretty print typed values with configurable indentation"):
    type Data =
      (x: (label: String, ys: Vector[Int]), y: Option[Int], ok: Boolean)

    val value: Data =
      (
        x = (label = "abc", ys = Vector(-1, 2, 3)),
        y = Some(23),
        ok = true
      )

    val rendered = Writers.writePretty(value, indent = 2)

    assertEquals(
      rendered,
      """(
        |  x = (
        |    label = "abc",
        |    ys = Vector(
        |      -1,
        |      2,
        |      3
        |    )
        |  ),
        |  y = 23,
        |  ok = true
        |)""".stripMargin
    )
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

  test("pretty print declarations and exprs with TextFormat"):
    val expr = Writers.writeExpr(
      (
        items = Vector(1, 2),
        nested = (ok = true)
      )
    )

    val expected =
      """val data = (
        |    items = Vector(
        |        1,
        |        2
        |    ),
        |    nested = (
        |        ok = true
        |    )
        |)""".stripMargin

    assertEquals(
      Writers.writeDecl(
        "data",
        (items = Vector(1, 2), nested = (ok = true)),
        format = TextFormat.pretty(4)
      ),
      expected
    )
    assertEquals(
      expr.render(TextFormat.pretty(4)),
      expected.stripPrefix("val data = ")
    )

  test("TextFormat rejects negative indentation and spacing"):
    interceptMessage[IllegalArgumentException]("requirement failed: indent must be >= 0, got -1") {
      TextFormat.pretty(-1)
    }
    interceptMessage[IllegalArgumentException]("requirement failed: spacing must be >= 0, got -1") {
      TextFormat.compact(-1)
    }
