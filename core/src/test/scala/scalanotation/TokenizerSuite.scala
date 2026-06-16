package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class TokenizerSuite extends ScalanotationSuite:
  test("tokenize booleans and negative numbers"):
    val input  = "val data = (a = true, b = false, c = -12, d = -1.5f)"
    val parsed = Readers.quick.readDecls(input)

    val Expr.NamedTupleExpr(fieldExprs) = parsed.declarations.head(1).runtimeChecked
    assertEquals(fieldExprs.length, 4)

  test("tokenize package statements"):
    assertEquals(
      tokenLabels("package foo.bar"),
      List("package", "<Identifier:foo>", ".", "<Identifier:bar>", "eof")
    )
    assertEquals(
      tokenLabels("package foo.bar; val data = null"),
      List(
        "package",
        "<Identifier:foo>",
        ".",
        "<Identifier:bar>",
        ";",
        "val",
        "<Identifier:data>",
        "=",
        "null",
        "eof"
      )
    )
    assertEquals(
      tokenLabels("package foo"),
      List("package", "<Identifier:foo>", "eof")
    )

  test("tokenize Scala regular keywords as reserved syntax"):
    val regularKeywords = List(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "enum",
      "export",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "given",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "then",
      "throw",
      "trait",
      "true",
      "try",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield",
      ":",
      "=",
      "<-",
      "=>",
      "<:",
      ">:",
      "#",
      "@",
      "=>>",
      "?=>"
    )

    assertEquals(tokenLabels(regularKeywords.mkString(" ")), regularKeywords :+ "eof")

  test("do not greedily tokenize symbolic keywords inside longer operator identifiers"):
    val symbolicIdentifiers = List(
      "::",
      "=>=",
      "<->",
      "##",
      "@@",
      "?==>",
      "=>>>",
      "++",
      "--",
      "=="
    )

    assertEquals(
      tokenLabels(symbolicIdentifiers.mkString(" ")),
      symbolicIdentifiers.map(name => s"<Identifier:$name>") :+ "eof"
    )

  test("treat Scala soft keywords as identifiers"):
    val softKeywords = List(
      "as",
      "derives",
      "end",
      "extension",
      "infix",
      "inline",
      "opaque",
      "open",
      "transparent",
      "using"
    )

    assertEquals(
      tokenLabels(softKeywords.mkString(" ")),
      softKeywords.map(name => s"<Identifier:$name>") :+ "eof"
    )

  test("tokenize quoted identifiers according to Scala lexical syntax"):
    val unicodeBacktick = "`" + "\\" + "u0060" + "`"

    assertEquals(
      tokenLabels("""`def` `has space` `a-b` `line\nindent\tpath\\` """ + unicodeBacktick),
      List(
        "<Identifier:def>",
        "<Identifier:has space>",
        "<Identifier:a-b>",
        "<Identifier:line\nindent\tpath\\>",
        "<Identifier:`>",
        "eof"
      )
    )

  test("parse quoted identifiers in identifier positions"):
    val input =
      """val `type` = (`yield` = 1, `has space` = 2, `a-b` = 3, + = 4, - = 5)
        |""".stripMargin

    val parsed = Readers.quick.readDecls(input)

    val expected = Expr.SourceFile(
      Map(
        "type" -> Expr.NamedTupleExpr(
          IndexedSeq(
            "yield"     -> Expr.IntConstant(1),
            "has space" -> Expr.IntConstant(2),
            "a-b"       -> Expr.IntConstant(3),
            "+"         -> Expr.IntConstant(4),
            "-"         -> Expr.IntConstant(5)
          )
        )
      )
    )

    assertEquals(parsed, expected)

  test("parse soft syntax names in identifier positions"):
    val input =
      """val Tuple = (Vector = 99, Tuple = 100)
        |""".stripMargin

    val parsed = Readers.quick.readDecls(input)

    val expected = Expr.SourceFile(
      Map(
        "Tuple" -> Expr.NamedTupleExpr(
          IndexedSeq("Vector" -> Expr.IntConstant(99), "Tuple" -> Expr.IntConstant(100))
        )
      )
    )

    assertEquals(parsed, expected)

  test("soft keywords remain valid field names"):
    val input  = "val data = (using = 1, extension = 2, derives = 3, end = 4)"
    val parsed = Readers.quick.readDecls(input)

    val expected = Expr.SourceFile(
      Map(
        "data" -> Expr.NamedTupleExpr(
          IndexedSeq(
            "using"     -> Expr.IntConstant(1),
            "extension" -> Expr.IntConstant(2),
            "derives"   -> Expr.IntConstant(3),
            "end"       -> Expr.IntConstant(4)
          )
        )
      )
    )
    assertEquals(parsed, expected)

  test("reject regular keywords as field names"):
    val input    = "val data = (class = 1)"
    val obtained = Readers.readDeclAs[Expr](input, rootName = "data")

    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedFieldName("'class'"))
        assertEquals(error.span.map(span => (span.line, span.column)), Some((1, 13)))
      case Result.Ok(value) => fail(s"Expected a parse failure, got $value")

  test("top level Vector"):
    val input  = "val data = Vector(true)"
    val parsed = Readers.quick.readDecls(input)

    val Expr.VectorExpr(elements) = parsed.declarations.head(1).runtimeChecked
    assertEquals(elements.length, 1)
