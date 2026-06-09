package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class SourceFileSuite extends ScalanotationSuite:
  test("read expected package statement"):
    type Data = (x: Int)
    val input =
      """package foo.bar
        |val data = (x = 1)
        |""".stripMargin

    val parsed   = Readers.quick.readDecls(input, packageName = "foo.bar")
    val expected = Expr.SourceFile(
      Map("data" -> Expr.NamedTupleExpr(IndexedSeq("x" -> Expr.IntConstant(1))))
    )

    assertEquals(parsed, expected)
    assertEquals(
      Readers.readDeclAs[Data](input, rootName = "data", packageName = "foo.bar"),
      Result.Ok((x = 1))
    )

  test("read expected package statement with semicolon separator"):
    type Data = (x: Int)
    val input = "package foo.bar; val data = (x = 1)"

    assertEquals(
      Readers.readDeclAs[Data](input, rootName = "data", packageName = "foo.bar"),
      Result.Ok((x = 1))
    )

  test("read expected single-level package statement"):
    type Data = (x: Int)
    val input =
      """package foo
        |val data = (x = 1)
        |""".stripMargin

    assertEquals(
      Readers.readDeclAs[Data](input, rootName = "data", packageName = "foo"),
      Result.Ok((x = 1))
    )

  test("reject package statement before top-level expression"):
    assertEquals(
      Readers.readAs[Expr]("package foo.bar\n(x = 1)").map(_ => ()),
      Result.Err(DecodeError.ExpectedExpression("'package'").atToken(DecodeError.Span(0, 1, 1)))
    )

  test("reject missing or unexpected package statement"):
    val missing =
      Readers.readDeclAs[Expr]("val data = null", rootName = "data", packageName = "foo.bar")
    assertEquals(
      missing.map(_ => ()),
      Result.Err(DecodeError.ExpectedPackage("'val'").atToken(DecodeError.Span(0, 1, 1)))
    )

    val mismatch =
      Readers.readDeclAs[Expr](
        "package foo.baz\nval data = null",
        rootName = "data",
        packageName = "foo.bar"
      )
    assertEquals(mismatch, Result.Err(DecodeError.UnexpectedPackage("foo.baz")))

  test("reject package statement when expected package is empty"):
    val obtained = Readers.readDeclAs[Expr]("package foo\nval data = null", rootName = "data")
    assertEquals(
      obtained.map(_ => ()),
      Result.Err(DecodeError.ExpectedVal("'package'").atToken(DecodeError.Span(0, 1, 1)))
    )

  test("skip single-line comments"):
    val input =
      """// leading comment
        |val data = ( // comment after opening
        |  x = 1, // trailing field comment
        |  // comment between fields
        |  y = true
        |)
        |// trailing comment
        |""".stripMargin

    val parsed = Readers.quick.readDecls(input)

    val expected = {
      Expr.SourceFile(
        Map(
          "data" -> Expr.NamedTupleExpr(
            IndexedSeq(
              "x" -> Expr.IntConstant(1),
              "y" -> Expr.BooleanConstant(true)
            )
          )
        )
      )
    }
    assertEquals(parsed, expected)

  test("skip nested block comments"):
    val input =
      """/* leading block comment
        |   /* nested block comment */
        |*/
        |val data = (
        |  x = /* before nested tuple */ (
        |    label = "abc",
        |    ys = Vector(1, /* inside vector */ 2)
        |  ),
        |  y = null,
        |  ok = /* trailing value comment */ true
        |)
        |""".stripMargin

    type Data =
      (x: (label: String, ys: Vector[Int]), y: Option[String], ok: Boolean)

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data =
      (x = (label = "abc", ys = Vector(1, 2)), y = None, ok = true)

    assertEquals(decoded, Result.Ok(expected))

  test("reject wrong root declaration name"):
    val input   = "val data = (x = 1)"
    val decoded = Readers.readDeclAs[Expr](input, rootName = "other")
    assertEquals(decoded, Result.Err(DecodeError.UnexpectedRoot("data")))

  test("reject duplicate field decls with Expr"):
    val input   = "val data = (x = 1, x = 2)"
    val decoded = Readers.readDeclAs[Expr](input, rootName = "data")
    assertEquals(decoded.getErr.rootCause, DecodeError.DuplicateField("x"))
    assertEquals(decoded.getErr.path, List(".x"))

  test("reject duplicate field decls with Expr, nested"):
    val input   = "val data = (a = 1, b = (x = true, x = null), c = 3)"
    val decoded = Readers.readDeclAs[Expr](input, rootName = "data")
    assertEquals(decoded.getErr.rootCause, DecodeError.DuplicateField("x"))
    assertEquals(decoded.getErr.path, List(".b", ".x"))
    assertEquals(decoded.getErr.span.map(span => (span.line, span.column)), Some((1, 35)))

  test("reject duplicate field decls with typed named tuples"):
    type Data = (x: Int, y: Int)

    val input   = "val data = (x = 1, x = 2)"
    val decoded = Readers.readDeclAs[Data](input, rootName = "data")

    assertEquals(decoded.getErr.rootCause, DecodeError.DuplicateField("x"))
    assertEquals(decoded.getErr.path, List(".x"))
    assertEquals(decoded.getErr.span.map(span => (span.line, span.column)), Some((1, 20)))

  test("reject duplicate field decls in schema!"):
    type Data = NamedTuple.NamedTuple[("x", "x"), (Int, Int)]

    val input   = "val data = (x = 1, y = 2)"
    val decoded = Readers.readDeclAs[Data](input, rootName = "data")

    assertEquals(decoded.getErr.rootCause, DecodeError.DuplicateSchemaField("x"))
    assertEquals(decoded.getErr.path, List(".x"))
    assertEquals(decoded.getErr.span, None)
