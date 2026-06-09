package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import munit.FunSuite
import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import scalanotation.internal.Token
import scalanotation.internal.Tokenizer
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class ParserSuite extends FunSuite:
  private def tokenLabels(input: String): List[String] =
    val tokens = Tokenizer
      .tokenize(input, debug = false)
      .getOrElse(fail(s"Expected tokenization to succeed for input: $input"))
    tokens.map {
      case Token.PackageKw(_)        => "package"
      case Token.ValKw(_)            => "val"
      case Token.VectorId(_)         => "Vector"
      case Token.TrueKw(_)           => "true"
      case Token.FalseKw(_)          => "false"
      case Token.NullKw(_)           => "null"
      case Token.EmptyTupleId(_)     => "EmptyTuple"
      case Token.Keyword(raw, _)     => raw
      case Token.Identifier(name, _) => s"<Identifier:$name>"
      case Token.Equals(_)           => "="
      case Token.Dot(_)              => "."
      case Token.Plus(_)             => "+"
      case Token.Minus(_)            => "-"
      case Token.StarColon(_)        => "*:"
      case Token.Comma(_)            => ","
      case Token.Semicolon(_)        => ";"
      case Token.LParen(_)           => "("
      case Token.RParen(_)           => ")"
      case Token.Eof(_)              => "eof"
      case token                     =>
        fail(
          s"Unexpected token in test helper: $token\n${tokens.map(t => s"  $t").mkString("\n")}\nfor input: $input"
        )
    }

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

  test("read tuple cons literal expression"):
    val input  = """1 *: "abc" *: EmptyTuple"""
    val parsed = Readers.quick.read(input)

    assertEquals(
      parsed,
      Expr.TupleExpr(IndexedSeq(Expr.IntConstant(1), Expr.StringConstant("abc")))
    )
    assertEquals(parsed.render, """(1, "abc")""")

  test("describe tuple literal expressions with counted slots"):
    val obtained = Readers.quick.read("""(1, "two", true)""").decodeAs[Int]

    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedType("Int", "(..., ..., ...)"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("parentheses group expressions"):
    assertEquals(Readers.quick.read("(1)"), Expr.IntConstant(1))
    assertEquals(Readers.quick.read("((1))"), Expr.IntConstant(1))
    assertEquals(
      Readers.quick.read("""("foo" + "bar") *: EmptyTuple"""),
      Expr.TupleExpr(IndexedSeq(Expr.StringConstant("foobar")))
    )
    assertEquals(
      Readers.quick.read("""(1 *: EmptyTuple) *: "abc" *: EmptyTuple"""),
      Expr.TupleExpr(
        IndexedSeq(
          Expr.TupleExpr(IndexedSeq(Expr.IntConstant(1))),
          Expr.StringConstant("abc")
        )
      )
    )

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

  test("decode just expression"):
    type Data = (a: Boolean, b: Boolean, c: Int, d: Float)
    val input  = "(a = true, b = false, c = -12, d = -1.5f)"
    val parsed = Readers.readAs[Data](input)
    val data   = parsed.getOrElse(fail(s"Expected successful parse, got $parsed"))
    assertEquals(data, (a = true, b = false, c = -12, d = -1.5f))

  test("decode lossless numeric literal promotions in read mode"):
    type Data =
      (
          intToLong: Long,
          intToDouble: Double,
          intToFloat: Float
      )

    val input =
      """(
        |  intToLong = 1,
        |  intToDouble = 2,
        |  intToFloat = 16_777_216
        |)
        |""".stripMargin

    val expected: Data =
      (
        intToLong = 1L,
        intToDouble = 2.0d,
        intToFloat = 16_777_216.0f
      )

    assertEquals(Readers.readAs[Data](input), Result.Ok(expected))

  test("round-trip int literal promotions for float and double through declaration text"):
    type Data =
      (
          intToFloat: Float,
          intToDouble: Double
      )

    val input =
      """val data = (
        |  intToFloat = 16_777_216,
        |  intToDouble = 2_147_483_647
        |)
        |""".stripMargin

    val expected: Data =
      (
        intToFloat = 16_777_216.0f,
        intToDouble = 2_147_483_647.0d
      )

    val decoded  = Readers.readDeclAs[Data](input, rootName = "data")
    val value    = decoded.getOrElse(fail(s"Expected successful parse, got $decoded"))
    val rendered = Writers.writeDecl("data", value)
    val reparsed = Readers.readDeclAs[Data](rendered, rootName = "data")

    assertEquals(value, expected)
    assertEquals(reparsed, Result.Ok(expected))

  test("reject numeric literal promotions that would lose precision"):
    val floatErr         = Readers.readAs[Float]("16_777_217")
    val doubleToFloatErr = Readers.readAs[Float]("0.1")
    val longToFloatErr   = Readers.readAs[Float]("33_554_432L")
    val longToDoubleErr  = Readers.readAs[Double]("9L")
    val floatToDoubleErr = Readers.readAs[Double]("3.5f")

    floatErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Float", "integer literal '16_777_217'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    doubleToFloatErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Float", "double literal '0.1'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    longToFloatErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Float", "long literal '33_554_432L'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    longToDoubleErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Double", "long literal '9L'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    floatToDoubleErr match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Double", "float literal '3.5f'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("decode lossless numeric literal promotions from Expr values"):
    val expr = Expr.NamedTupleExpr(
      IndexedSeq(
        "intToLong"   -> Expr.IntConstant(1),
        "intToDouble" -> Expr.IntConstant(2),
        "intToFloat"  -> Expr.IntConstant(16_777_216)
      )
    )

    type Data =
      (
          intToLong: Long,
          intToDouble: Double,
          intToFloat: Float
      )

    val expected: Data =
      (
        intToLong = 1L,
        intToDouble = 2.0d,
        intToFloat = 16_777_216.0f
      )

    assertEquals(expr.decodeAs[Data], Result.Ok(expected))
    assertEquals(
      Expr.DoubleConstant(0.1d).decodeAs[Float],
      Result.Err(DecodeError.ExpectedType("Float", "(0.1: Double)"))
    )
    assertEquals(
      Expr.LongConstant(9L).decodeAs[Double],
      Result.Err(DecodeError.ExpectedType("Double", "(9: Long)"))
    )
    assertEquals(
      Expr.FloatConstant(3.5f).decodeAs[Double],
      Result.Err(DecodeError.ExpectedType("Double", "(3.5: Float)"))
    )

  test("BigInt and BigDecimal map through String instances"):
    val bigIntValue     = BigInt("123456789012345678901234567890")
    val bigDecimalValue = BigDecimal("1234567890.012345678900")

    assertEquals(summon[Reader[BigInt]].schema.describeSelf, "String")
    assertEquals(summon[Writer[BigInt]].schema.describeSelf, "String")
    assertEquals(summon[ReadWriter[BigInt]].schema.describeSelf, "String")
    assertEquals(summon[Reader[BigDecimal]].schema.describeSelf, "String")
    assertEquals(summon[Writer[BigDecimal]].schema.describeSelf, "String")
    assertEquals(summon[ReadWriter[BigDecimal]].schema.describeSelf, "String")

    assertEquals(Readers.readAs[BigInt](s""""$bigIntValue""""), Result.Ok(bigIntValue))
    assertEquals(Readers.readAs[BigDecimal](s""""$bigDecimalValue""""), Result.Ok(bigDecimalValue))
    assertEquals(Writers.write(bigIntValue), s""""$bigIntValue"""")
    assertEquals(Writers.write(bigDecimalValue), s""""$bigDecimalValue"""")

    type Data = (count: BigInt, amount: BigDecimal)
    val value: Data =
      (
        count = bigIntValue,
        amount = bigDecimalValue
      )

    val rendered = Writers.write(value)
    assertEquals(rendered, s"""(count = "$bigIntValue", amount = "$bigDecimalValue")""")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

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
    assertEquals(tokenLabels("*: EmptyTuple"), List("*:", "EmptyTuple", "eof"))

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

  test("parse Vector in identifier positions"):
    val input =
      """val Vector = (Vector = 99)
        |""".stripMargin

    val parsed = Readers.quick.readDecls(input)

    val expected = Expr.SourceFile(
      Map(
        "Vector" -> Expr.NamedTupleExpr(IndexedSeq("Vector" -> Expr.IntConstant(99)))
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

  test("decode directly into a typed named tuple"):
    type Data =
      (x: (label: String, ys: Vector[Int]), y: Option[Int], ok: Boolean)

    val input =
      """val data = (
        |  x = (
        |    label = "abc" + "def",
        |    ys = Vector(-1, -0b0000_0011, -0x00_1A)
        |  ),
        |  y = 23,
        |  ok = true
        |)
        |""".stripMargin

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data =
      (
        x = (label = "abcdef", ys = Vector(-1, -3, -26)),
        y = Some(23),
        ok = true
      )

    assertEquals(decoded, Result.Ok(expected))

  test("decode null as None and values as Some"):
    type Data = (missing: Option[Int], present: Option[Int])

    val input =
      """val data = (
        |  missing = null,
        |  present = 41
        |)
        |""".stripMargin

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data = (missing = None, present = Some(41))

    assertEquals(decoded, Result.Ok(expected))

  test("report inner schema mismatches for Option values"):
    type Data = (x: Option[Int])

    val input    = "val data = (x = true)"
    val obtained = Readers.readDeclAs[Data](input, rootName = "data")

    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".x"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "'true'")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("parse dense immutable arrays"):
    type Data = IArray[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assert(read.builder.isInstanceOf[PublicInternal.BuildIArray[?]])
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case IArray(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )

  test("parse dense arrays"):
    type Data = Array[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assert(read.builder.isInstanceOf[PublicInternal.BuildArray[?]])
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case Array(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )

  test("parse arbitrary sequence") {
    type Data = List[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assertEquals(read.builder.getClass.getSimpleName, "SeqFactoryVector")
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case List(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )
  }

  test("parse specialized vector") {
    type Data = Vector[Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case vector: RawSchema.Vector =>
        vector.read match
          case read: RawSchema.VectorRead.FromReaderBuilder[?, ?, ?] =>
            assert(read.builder.isInstanceOf[PublicInternal.BuildVector[?]])
          case _ =>
            fail(s"Expected a vector reader builder, got ${vector.read}")
      case _ =>
        fail(s"Expected a vector reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = { case Vector(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )
  }

  test("parse arbitrary map") {
    type Data = mutable.LinkedHashMap[String, Int]

    val sc = summon[Reader[Data]]
    sc.schema match
      case dict: RawSchema.Dict =>
        dict.read match
          case read: RawSchema.DictRead.FromReaderBuilder[?, ?, ?] =>
            assertEquals(read.builder.getClass.getSimpleName, "MapFactoryDict")
          case _ =>
            fail(s"Expected a dict reader builder, got ${dict.read}")
      case _ =>
        fail(s"Expected a dict reader, got ${sc.schema.describeSelf}")

    val input =
      """val data = (x = 1, y = 2, z = 3)
        |""".stripMargin

    val decoded  = Readers.readDeclAs[Data](input, rootName = "data")
    val expected = mutable.LinkedHashMap("x" -> 1, "y" -> 2, "z" -> 3)
    assertEquals(
      expected,
      decoded.getOrElse(fail(s"Expected successful parse, got $decoded"))
    )
  }

  test("decode vectors of nested named tuples"):
    type Entry = (name: String, value: Int)
    type Data  = (items: Vector[Entry], total: Long)

    val input =
      """val data = (
        |  items = Vector(
        |    (name = "a", value = 1),
        |    (name = "b", value = 2)
        |  ),
        |  total = 2L
        |)
        |""".stripMargin

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data =
      (
        items = Vector((name = "a", value = 1), (name = "b", value = 2)),
        total = 2L
      )

    assertEquals(decoded, Result.Ok(expected))

  test("report schema mismatches during decoding"):
    type Data = (x: Int)

    val input    = "val data = (x = true)"
    val obtained = Readers.readDeclAs[Data](input, rootName = "data")

    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".x"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "'true'")
        )
        assertEquals(
          error.span.map(span => (span.line, span.column)),
          Some((1, 17))
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("report full nested decode path through vectors"):
    type Data = (items: Vector[(value: Int)])

    val input =
      """val data = (
        |  items = Vector((value = true))
        |)
        |""".stripMargin

    val obtained = Readers.readDeclAs[Data](input, rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".items", "[0]", ".value"))
        assertEquals(true, error.format.contains("'.items[0].value'"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "'true'")
        )
        assertEquals(error.span.map(span => span.line), Some(2))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("report full nested decode path through vectors, validated"):
    type Data = (items: Vector[(value: Int)])

    val input =
      """val data = (
        |  items = Vector((value = true))
        |)
        |""".stripMargin

    val obtained  = Readers.readDeclAs[Expr](input, rootName = "data").get
    val validated = obtained.decodeAs[Data]
    validated match
      case Result.Err(error) =>
        assertEquals(error.path, List(".items", "[0]", ".value"))
        assertEquals(true, error.format.contains("'.items[0].value'"))
        assertEquals(
          error.rootCause,
          DecodeError.ExpectedType("Int", "(true: Boolean)")
        )
        assertEquals(error.span, None)
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("nest Expr inside of structured type"):
    type Data = (x: Int, y: (q: Vector[Expr]))

    val input                                 = "val data = (x = 23, y = (q = Vector(41)))"
    val obtained                              = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: PartialFunction[Data, Unit] = {
      case (
            x = 23,
            y = (q = Vector(Expr.IntConstant(41)))
          ) =>
        ()
    }
    assert(
      expected.isDefinedAt(
        obtained.getOrElse(fail(s"Expected successful decode, got $obtained"))
      )
    )

  test("reject swapped named tuple field order"):
    type Data = (x: Int, y: Boolean)

    val input = "val data = (y = true, x = 1)"

    val obtained = Readers.readDeclAs[Data](input, rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.FieldOrderMismatch("x", "y"))
        assertEquals(
          error.span.map(span => (span.line, span.column)),
          Some((1, 13))
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("decode custom types from single-level Reader transformations"):
    enum Mode:
      case Fast, Safe

    final case class User(name: String, age: Int)
    final case class Schedule(dates: Vector[LocalDate])

    given Reader[Mode] =
      summon[Reader[String]].mapResult {
        case "fast" => Result.Ok(Mode.Fast)
        case "safe" => Result.Ok(Mode.Safe)
        case other  => Result.Err(DecodeError.Custom(s"Unknown mode '$other'"))
      }

    given Reader[LocalDate] =
      summon[Reader[String]].mapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    given Reader[User] =
      summon[Reader[(name: String, age: Int)]].map { data =>
        User(name = data.name, age = data.age)
      }

    given Reader[Schedule] =
      summon[Reader[Vector[LocalDate]]].map(Schedule(_))

    type Data = (owner: User, mode: Mode, schedule: Schedule)

    val input =
      """val data = (
        |  owner = (name = "Ada", age = 41),
        |  mode = "fast",
        |  schedule = Vector("2026-03-14", "2026-03-15")
        |)
        |""".stripMargin

    val decoded        = Readers.readDeclAs[Data](input, rootName = "data")
    val expected: Data =
      (
        owner = User("Ada", 41),
        mode = Mode.Fast,
        schedule = Schedule(Vector(LocalDate.parse("2026-03-14"), LocalDate.parse("2026-03-15")))
      )

    assertEquals(decoded, Result.Ok(expected))

  test("report composed paths for custom string decoders inside vectors"):
    given Reader[LocalDate] =
      summon[Reader[String]].mapResult { raw =>
        try Result.Ok(LocalDate.parse(raw))
        catch
          case _: DateTimeParseException =>
            Result.Err(DecodeError.Custom(s"Invalid ISO date '$raw'"))
      }

    type Data = (dates: Vector[LocalDate])

    val input =
      """val data = (
        |  dates = Vector("2026-03-14", "2026-99-99")
        |)
        |""".stripMargin

    val obtained = Readers.readDeclAs[Data](input, rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".dates", "[1]"))
        assertEquals(error.rootCause, DecodeError.Custom("Invalid ISO date '2026-99-99'"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("report custom sequence validation errors at the wrapped field"):
    final case class NonEmptyInts(values: Vector[Int])

    given Reader[NonEmptyInts] =
      summon[Reader[Vector[Int]]].mapResult { values =>
        if values.nonEmpty then Result.Ok(NonEmptyInts(values))
        else Result.Err(DecodeError.Custom("Expected at least one integer"))
      }

    type Data = (items: NonEmptyInts)

    val obtained = Readers.readDeclAs[Data]("val data = (items = Vector())", rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".items"))
        assertEquals(error.rootCause, DecodeError.Custom("Expected at least one integer"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("decode directly into nested case classes"):
    final case class Metadata(created: LocalDate, tags: Vector[String]) derives Reader
    final case class User(name: String, age: Int, metadata: Metadata) derives Reader

    given Reader[LocalDate] =
      summon[Reader[String]].mapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    val input =
      """val data = (
        |  name = "Ada",
        |  age = 41,
        |  metadata = (
        |    created = "2026-03-14",
        |    tags = Vector("compiler", "scala")
        |  )
        |)
        |""".stripMargin

    val decoded  = Readers.readDeclAs[User](input, rootName = "data")
    val expected = User(
      name = "Ada",
      age = 41,
      metadata = Metadata(
        created = LocalDate.parse("2026-03-14"),
        tags = Vector("compiler", "scala")
      )
    )

    assertEquals(decoded, Result.Ok(expected))

  test("case class reader derivation keeps Option fields ordered and required by default"):
    final case class User(
        name: String,
        refreshSeconds: Option[Int],
        debug: Boolean,
        description: Option[String]
    ) derives Reader

    val input =
      """val data = (
        |  name = "Ada",
        |  debug = true
        |)
        |""".stripMargin

    val obtained = Readers.readDeclAs[User](input, rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.FieldOrderMismatch("refreshSeconds", "debug")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("skippable case class readers allow skipped nullable Option fields"):
    final case class User(
        name: String,
        refreshSeconds: Option[Int],
        debug: Boolean,
        description: Option[String]
    )

    given Reader[User] = Reader.skippable.derived

    val input =
      """val data = (
        |  name = "Ada",
        |  debug = true
        |)
        |""".stripMargin

    val decoded  = Readers.readDeclAs[User](input, rootName = "data")
    val expected = User(
      name = "Ada",
      refreshSeconds = None,
      debug = true,
      description = None
    )

    assertEquals(decoded, Result.Ok(expected))

  test("case class reader derivation allows all optional fields by default"):
    final case class Data(x: Option[Int], y: Option[String]) derives Reader

    val decoded = Readers.readAs[Data]("""(x = null, y = "present")""")

    assertEquals(decoded, Result.Ok(Data(None, Some("present"))))

  test("named tuple readers keep Option fields ordered and required"):
    type Data = (name: String, refreshSeconds: Option[Int], debug: Boolean)

    val input =
      """val data = (
        |  name = "Ada",
        |  debug = true
        |)
        |""".stripMargin

    val obtained = Readers.readDeclAs[Data](input, rootName = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.FieldOrderMismatch("refreshSeconds", "debug")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("skippable case class readers reject empty named tuple input"):
    final case class User(name: String, refreshSeconds: Option[Int])

    given Reader[User] = Reader.skippable.derived

    val obtained = Readers.readAs[User]("()")

    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.UnitValueNotAllowed())
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("expr decoder rejects empty input for skipped nullable fields"):
    val reader = Reader.fromSchema[Any](
      RawSchema.NamedTuple(
        IArray(
          RawSchema.Field("start", summon[Reader[Option[Int]]].schema),
          RawSchema.Field("end", summon[Reader[Option[String]]].schema)
        ),
        RawSchema.NamedTupleRead.from(identity),
        allowSkippedNullableFields = true
      )
    )

    val obtained = Expr.NamedTupleExpr(Vector.empty).decodeAs[Any](using reader)

    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.UnitValueNotAllowed())
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("report nested paths for direct case class decoders"):
    final case class Metadata(created: LocalDate)
    final case class User(metadata: Metadata)

    given Reader[LocalDate] =
      summon[Reader[String]].mapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    given Reader[Metadata] = Reader.ofFields[Metadata]
    given Reader[User]     = Reader.ofFields[User]

    val input =
      """val data = (
        |  metadata = (
        |    created = "bad-date"
        |  )
        |)
        |""".stripMargin

    Readers.readDeclAs[User](input, rootName = "data") match
      case Result.Err(error) =>
        assertEquals(error.path, List(".metadata", ".created"))
        assertEquals(error.rootCause, DecodeError.Custom("Invalid ISO date 'bad-date'"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("derive enum schemas with nullary and structured cases"):
    enum Mode derives Reader:
      case Fast
      case Scheduled(at: LocalDate, retries: Int)

    given Reader[LocalDate] =
      summon[Reader[String]].mapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    val fast      = Readers.readDeclAs[Mode]("val data = (Fast = null)", rootName = "data")
    val scheduled = Readers.readDeclAs[Mode](
      """val data = (
        |  Scheduled = (
        |    at = "2026-03-15",
        |    retries = 2
        |  )
        |)
        |""".stripMargin,
      rootName = "data"
    )

    assertEquals(fast, Result.Ok(Mode.Fast))
    assertEquals(
      scheduled,
      Result.Ok(Mode.Scheduled(LocalDate.parse("2026-03-15"), 2))
    )

  test("derive case object schemas"):
    case object Foo derives Reader

    val foo = Readers.readDeclAs[Foo.type]("val data = (Foo = null)", rootName = "data")
    assertEquals(foo, Result.Ok(Foo))

  test("report nested enum case paths"):
    enum Mode derives Reader:
      case Fast
      case Scheduled(at: LocalDate)

    given Reader[LocalDate] =
      summon[Reader[String]].mapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    type Data = (mode: Mode)

    val input =
      """val data = (
        |  mode = (
        |    Scheduled = (
        |      at = "bad-date"
        |    )
        |  )
        |)
        |""".stripMargin

    Readers.readDeclAs[Data](input, rootName = "data") match
      case Result.Err(error) =>
        assertEquals(error.path, List(".mode", ".Scheduled", ".at"))
        assertEquals(error.rootCause, DecodeError.Custom("Invalid ISO date 'bad-date'"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

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

  test("tuple typeclass instances round-trip through Expr and text"):
    type Data = (Int, String, (ok: Boolean), Vector[Long])
    val value: Data = (1, "two", (ok = true), Vector(3L, 4L))

    summon[Reader[Data]].schema match
      case tuple: RawSchema.Tuple =>
        assertEquals(tuple.slots.length, 4)
        tuple.read match
          case read: RawSchema.TupleRead.FromReaderBuilder[?, ?] =>
            assert(read.builder.isInstanceOf[PublicInternal.BuildTuple[?]])
          case _ =>
            fail(s"Expected a tuple reader builder, got ${tuple.read}")
      case other =>
        fail(s"Expected a tuple reader, got ${other.describeSelf}")

    summon[Writer[Data]].schema match
      case tuple: RawSchema.Tuple =>
        assertEquals(tuple.slots.length, 4)
        assertEquals(tuple.write, RawSchema.TupleWrite.productLike)
      case other =>
        fail(s"Expected a tuple writer, got ${other.describeSelf}")

    val expr         = Writers.writeExpr(value)
    val expectedExpr = Expr.TupleExpr(
      IndexedSeq(
        Expr.IntConstant(1),
        Expr.StringConstant("two"),
        Expr.NamedTupleExpr(IndexedSeq("ok" -> Expr.BooleanConstant(true))),
        Expr.VectorExpr(IndexedSeq(Expr.LongConstant(3L), Expr.LongConstant(4L)))
      )
    )
    val rendered = Writers.write(value)

    assertEquals(expr, expectedExpr)
    assertEquals(expr.decodeAs[Data], Result.Ok(value))
    assertEquals(rendered, """(1, "two", (ok = true), Vector(3L, 4L))""")
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))
    assertEquals(summon[ReadWriter[Data]].schema.describeSelf, "(..., ..., ..., ...)")

  test("mapped tuple ReadWriter round-trips through Expr and text"):
    final case class Pair(count: Int, label: String)

    given ReadWriter[Pair] =
      summon[ReadWriter[(Int, String)]].bimapResult { case (count, label) =>
        Result.Ok(Pair(count, label))
      }(pair => (pair.count, pair.label))

    val value    = Pair(7, "seven")
    val expr     = Writers.writeExpr(value)
    val rendered = Writers.write(value)

    assertEquals(
      expr,
      Expr.TupleExpr(IndexedSeq(Expr.IntConstant(7), Expr.StringConstant("seven")))
    )
    assertEquals(expr.decodeAs[Pair], Result.Ok(value))
    assertEquals(rendered, """(7, "seven")""")
    assertEquals(Readers.readAs[Pair](rendered), Result.Ok(value))

  test("tuple typeclass instances support arities above Tuple22"):
    type Data =
      (
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int,
          Int
      )
    val value: Data =
      (
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
      )
    val rendered = Writers.write(value)

    summon[Reader[Data]].schema match
      case tuple: RawSchema.Tuple =>
        assertEquals(tuple.slots.length, 23)
      case other =>
        fail(s"Expected a tuple reader, got ${other.describeSelf}")

    assertEquals(
      Writers.writeExpr(value),
      Expr.TupleExpr((1 to 23).map(Expr.IntConstant.apply).toIndexedSeq)
    )
    assertEquals(
      rendered,
      "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)"
    )
    assertEquals(Readers.readAs[Data](rendered), Result.Ok(value))

  test("tuple decodes support EmptyTuple and cons syntax"):
    assertEquals(Readers.readAs[EmptyTuple]("EmptyTuple"), Result.Ok(EmptyTuple))
    assertEquals(Writers.write(EmptyTuple), "EmptyTuple")

    val singleton: Int *: EmptyTuple = 1 *: EmptyTuple
    assertEquals(
      Readers.readAs[Int *: EmptyTuple]("1 *: EmptyTuple"),
      Result.Ok(singleton)
    )
    assertEquals(Writers.write(singleton), "1 *: EmptyTuple")
    assertEquals(
      Writers.write(Expr.TupleExpr(IndexedSeq(Expr.TupleExpr(IndexedSeq(Expr.IntConstant(1)))))),
      "(1 *: EmptyTuple) *: EmptyTuple"
    )
    assertEquals(
      Writers.write((1 *: EmptyTuple) *: EmptyTuple),
      "(1 *: EmptyTuple) *: EmptyTuple"
    )

    assertEquals(
      Readers.readAs[(String, Int)]("""("foo" + "bar") *: 7 *: EmptyTuple"""),
      Result.Ok(("foobar", 7))
    )
    assertEquals(
      Readers.readAs[(Int *: EmptyTuple, String)](
        """(1 *: EmptyTuple) *: "abc" *: EmptyTuple"""
      ),
      Result.Ok((1 *: EmptyTuple, "abc"))
    )

  test("parenthesized single values are not tuple decodes"):
    Readers.readAs[(Int, String)]("(1)") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedType("(..., ...)", "(1: Int)"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    val singletonReader = Reader.fromSchema[Int *: EmptyTuple](
      RawSchema.Tuple(
        IArray(RawSchema.Int),
        RawSchema.TupleRead.FromReaderBuilder(PublicInternal.BuildTuple[Int *: EmptyTuple]),
        write = null
      )
    )

    Readers.readAs[Int *: EmptyTuple]("(1)")(using singletonReader) match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.ExpectedType("... *: EmptyTuple", "(1: Int)"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    Readers.readAs[Int *: EmptyTuple]("(1, 2)")(using singletonReader) match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.FieldCountMismatch(1, 2))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("no tuple reader is derived when a slot lacks evidence"):
    val errors = typeCheckErrors(
      "class Box\nsummon[scalanotation.Reader[(Int, String, Box)]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("[2]"))
    assert(clue(errors.head.message).contains("Box"))

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
      Writers.writeDecl("data", (items = Vector(1, 2), nested = (ok = true)), TextFormat.pretty(4)),
      expected
    )
    assertEquals(
      expr.render(TextFormat.pretty(4)),
      expected.stripPrefix("val data = ")
    )

  test("TextFormat rejects negative indentation"):
    interceptMessage[IllegalArgumentException]("requirement failed: indent must be >= 0, got -1") {
      TextFormat.pretty(-1)
    }

  test("reader and writer share the same raw schema description"):
    final case class Entry(name: String, value: Int) derives Reader, Writer

    val readerSchema = summon[Reader[Entry]].schema.describeSelf
    val writerSchema = summon[Writer[Entry]].schema.describeSelf

    assertEquals(readerSchema, writerSchema)

  test("derived ReadWriter provides aligned reader and writer views"):
    final case class Metadata(created: LocalDate, tags: Vector[String]) derives ReadWriter
    enum Mode derives ReadWriter:
      case Fast
      case Scheduled(at: LocalDate, retries: Int)
    final case class User(name: String, mode: Mode, metadata: Metadata) derives ReadWriter

    given ReadWriter[LocalDate] =
      summon[ReadWriter[String]].bimapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }(_.toString)

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
    assertEquals(
      summon[ReadWriter[User]].schema.describeSelf,
      summon[Reader[User]].schema.describeSelf
    )
    assertEquals(
      summon[ReadWriter[User]].schema.describeSelf,
      summon[Writer[User]].schema.describeSelf
    )

  test("derived ReadWriter can round-trip through Writers.write and Readers.readAs"):
    final case class Metadata(created: LocalDate) derives ReadWriter
    final case class User(name: String, metadata: Metadata) derives ReadWriter

    given ReadWriter[LocalDate] =
      summon[ReadWriter[String]].bimapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }(_.toString)

    val value    = User("Ada", Metadata(LocalDate.parse("2026-03-14")))
    val rendered = Writers.write(value)
    val decoded  = Readers.readAs[User](rendered)

    assertEquals(
      rendered,
      """(name = "Ada", metadata = (created = "2026-03-14"))"""
    )
    assertEquals(decoded, Result.Ok(value))

  test("configured ReadWriter derives discriminator field sum schemas"):
    enum Mode:
      case Fast
      case Scheduled(at: LocalDate, retries: Int)

    given ReadWriter[LocalDate] =
      summon[ReadWriter[String]].bimapResult { raw =>
        Result.catchException({ case _: DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }(_.toString)

    given Configured[Mode] = Configured.discriminator("type")
    given ReadWriter[Mode] = ReadWriter.configured.derived

    val scheduled: Mode = Mode.Scheduled(LocalDate.parse("2026-03-15"), 2)
    val fast: Mode      = Mode.Fast

    assertEquals(
      Writers.write(scheduled),
      """(`type` = "Scheduled", at = "2026-03-15", retries = 2)"""
    )
    assertEquals(Writers.write(fast), """(`type` = "Fast")""")
    assertEquals(
      Readers.readAs[Mode]("""(`type` = "Scheduled", at = "2026-03-15", retries = 2)"""),
      Result.Ok(scheduled)
    )
    assertEquals(Readers.readAs[Mode]("""(`type` = "Fast")"""), Result.Ok(fast))
    assertEquals(
      Expr
        .NamedTupleExpr(
          IndexedSeq(
            "type"    -> Expr.StringConstant("Scheduled"),
            "at"      -> Expr.StringConstant("2026-03-15"),
            "retries" -> Expr.IntConstant(2)
          )
        )
        .decodeAs[Mode],
      Result.Ok(scheduled)
    )

  test("configured discriminator sum decoder requires the discriminator field first"):
    enum Command:
      case Copy(from: String, to: String)

    given Configured[Command] = Configured.discriminator("kind")
    given Reader[Command]     = Reader.configured.derived

    Readers.readAs[Command]("""(from = "a", kind = "Copy", to = "b")""") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.FieldOrderMismatch("kind", "from"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("configured ReadWriter with no discriminator uses the standard sum schema"):
    enum Mode:
      case Fast
      case Scheduled(at: String)

    given Configured[Mode] = Configured.default
    given ReadWriter[Mode] = ReadWriter.configured.derived

    val value: Mode = Mode.Scheduled("soon")
    val rendered    = Writers.write(value)

    assertEquals(rendered, """(Scheduled = (at = "soon"))""")
    assertEquals(Readers.readAs[Mode](rendered), Result.Ok(value))

  test("configured skippable products allow skipped nullable fields"):
    final case class User(name: String, nickname: Option[String])

    given Configured[User] = Configured.skippable
    given Reader[User]     = Reader.configured.derived

    assertEquals(
      Readers.readAs[User]("""(name = "Ada")"""),
      Result.Ok(User("Ada", None))
    )

  test("configured skippable discriminator sum cases may contain only optional fields"):
    enum Event:
      case Ping(id: Option[Int], label: Option[String])

    given Configured[Event] = Configured.discriminator("type", skippable = true)
    given Reader[Event]     = Reader.configured.derived

    assertEquals(
      Readers.readAs[Event]("""(`type` = "Ping")"""),
      Result.Ok(Event.Ping(None, None))
    )
    assertEquals(
      Readers.readAs[Event]("""(`type` = "Ping", id = 1)"""),
      Result.Ok(Event.Ping(Some(1), None))
    )

  test("configured discriminator requires a sum type"):
    val errors = typeCheckErrors(
      "final case class Data(x: Int)\nscalanotation.Configured.discriminator[Data](\"type\")"
    )

    assert(clue(errors).nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.SumOf")))

  test("configured skippable products still require one non-optional field"):
    val errors = typeCheckErrors(
      "final case class Data(x: Option[Int], y: Option[String])\nscalanotation.Configured.skippable[Data]"
    )

    assert(errors.nonEmpty)
    assert(
      clue(errors.head.message)
        .contains("Configured skippable derivation for a product with only Option fields")
    )

  test("case class read-writer derivation allows all optional fields by default"):
    final case class Data(x: Option[Int], y: Option[String]) derives ReadWriter

    val value    = Data(None, Some("present"))
    val rendered = Writers.write(value)
    val decoded  = Readers.readAs[Data](rendered)

    assertEquals(rendered, """(x = null, y = "present")""")
    assertEquals(decoded, Result.Ok(value))

  test("skippable case class read-writers allow skipped nullable Option fields"):
    final case class User(
        name: String,
        refreshSeconds: Option[Int],
        debug: Boolean,
        description: Option[String]
    )

    given ReadWriter[User] = ReadWriter.skippable.derived

    val input =
      """val data = (
        |  name = "Ada",
        |  debug = true
        |)
        |""".stripMargin

    val decoded  = Readers.readDeclAs[User](input, rootName = "data")
    val expected = User(
      name = "Ada",
      refreshSeconds = None,
      debug = true,
      description = None
    )

    assertEquals(decoded, Result.Ok(expected))
    assertEquals(
      Writers.write(expected),
      """(name = "Ada", refreshSeconds = null, debug = true, description = null)"""
    )

  test("no writer is derived for nested Option"):
    val errors = typeCheckErrors(
      "type Data = (x: Option[Option[Int]])\nsummon[scalanotation.Writer[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".x"))
    assert(
      clue(errors.head.message)
        .contains("Writer[Option[Option[?]]] is not supported")
    )

  test("no decoder is derived for Any"):
    val errors = typeCheckErrors("summon[scalanotation.Reader[Any]]")
    assert(errors.nonEmpty)

  test("no decoder is derived for Vector[Any]"):
    val errors =
      typeCheckErrors("summon[scalanotation.Reader[Vector[Any]]]")
    assert(errors.nonEmpty)

  test("tuple typeclass instances are derived for EmptyTuple and singleton tuples"):
    assertEquals(summon[Reader[EmptyTuple]].schema.describeSelf, "EmptyTuple")
    assertEquals(summon[Writer[EmptyTuple]].schema.describeSelf, "EmptyTuple")
    assertEquals(summon[ReadWriter[EmptyTuple]].schema.describeSelf, "EmptyTuple")

    assertEquals(summon[Reader[Int *: EmptyTuple]].schema.describeSelf, "... *: EmptyTuple")
    assertEquals(summon[Writer[Int *: EmptyTuple]].schema.describeSelf, "... *: EmptyTuple")
    assertEquals(summon[ReadWriter[Int *: EmptyTuple]].schema.describeSelf, "... *: EmptyTuple")

  test("no decoder is derived for nested Option"):
    val errors = typeCheckErrors(
      "type Data = (x: Option[Option[Int]])\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".x"))
    assert(
      clue(errors.head.message)
        .contains("Reader[Option[Option[?]]] is not supported")
    )

  test("no skippable case class reader is derived when every field is an Option"):
    val errors = typeCheckErrors(
      "final case class Data(x: Option[Int], y: Option[String])\ngiven scalanotation.Reader[Data] = scalanotation.Reader.skippable.derived"
    )

    assert(errors.nonEmpty)
    assert(
      clue(errors.head.message)
        .contains("Reader derivation for a product with only Option fields is not supported")
    )

  test("no read-writer is derived for nested Option"):
    val errors = typeCheckErrors(
      "type Data = (x: Option[Option[Int]])\nsummon[scalanotation.ReadWriter[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".x"))
    assert(
      clue(errors.head.message)
        .contains("ReadWriter[Option[Option[?]]] is not supported")
    )

  test("no skippable case class read-writer is derived when every field is an Option"):
    val errors = typeCheckErrors(
      "final case class Data(x: Option[Int], y: Option[String])\ngiven scalanotation.ReadWriter[Data] = scalanotation.ReadWriter.skippable.derived"
    )

    assert(errors.nonEmpty)
    assert(
      clue(errors.head.message)
        .contains("ReadWriter derivation for a product with only Option fields is not supported")
    )

  test("compile-time derivation error includes nested field path"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (outer: (bad: Box[Int]))\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("outer.bad"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error includes nested field path Vector"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (outer: (inner: Vector[(sub1: (bad: Box[Int]))]))\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".outer.inner[].sub1.bad"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error includes nested field path Vector root"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = Vector[(sub1: (bad: Box[Int]))]\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("'[].sub1.bad'"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error includes vector path segment"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (items: Vector[(bad: Box[Int])])\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("'.items[].bad'"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("schema mappings only appear on mapped schemas"):
    final case class UserId(value: Int)

    assertEquals(summon[Reader[Int]].schema, RawSchema.Int)
    assertEquals(summon[Writer[Int]].schema, RawSchema.Int)

    val mappedReader = summon[Reader[Int]].map(UserId(_))
    mappedReader.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Int)
        assert(mapping.resultMap != null)
        assertEquals(mapping.inputMap, null)
        assertEquals(mappedReader.schema.describeSelf, "Int")
      case other =>
        fail(s"Expected a mapped reader schema, got $other")

    val mappedWriter = summon[Writer[Int]].contramap[UserId](_.value)
    mappedWriter.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Int)
        assertEquals(mapping.resultMap, null)
        assert(mapping.inputMap != null)
        assertEquals(mappedWriter.schema.describeSelf, "Int")
      case other =>
        fail(s"Expected a mapped writer schema, got $other")

    val mappedReadWriter = summon[ReadWriter[Int]].bimap(UserId(_))(_.value)
    mappedReadWriter.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Int)
        assert(mapping.resultMap != null)
        assert(mapping.inputMap != null)
      case other =>
        fail(s"Expected a mapped read-writer schema, got $other")

    Reader.forNull(UserId(1)).schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Null)
        assert(mapping.resultMap != null)
        assertEquals(mapping.inputMap, null)
      case other =>
        fail(s"Expected a mapped nullary schema, got $other")

  test("Null is a primitive schema and decodes to null"):
    assertEquals(summon[Reader[Null]].schema, RawSchema.Null)
    assertEquals(summon[Writer[Null]].schema, RawSchema.Null)
    assertEquals(summon[ReadWriter[Null]].schema, RawSchema.Null)

    assertEquals(Readers.readAs[Null]("null"), Result.Ok(null: Null))
    assertEquals(Writers.write(null: Null), "null")

  test("RawSchema.describeSelf"):
    // primitive schemas
    assertEquals(RawSchema.Int.describeSelf, "Int")
    assertEquals(RawSchema.Long.describeSelf, "Long")
    assertEquals(RawSchema.Float.describeSelf, "Float")
    assertEquals(RawSchema.Double.describeSelf, "Double")
    assertEquals(RawSchema.Boolean.describeSelf, "Boolean")
    assertEquals(RawSchema.Char.describeSelf, "Char")
    assertEquals(RawSchema.String.describeSelf, "String")
    assertEquals(RawSchema.AnyExpr.describeSelf, "Any")
    assertEquals(RawSchema.Null.describeSelf, "Null")
    // empty named tuple
    assertEquals(
      RawSchema.NamedTuple(IArray.empty[RawSchema.Field]).describeSelf,
      "AnyNamedTuple"
    )
    // named tuple with fields
    val withFields = RawSchema.NamedTuple(
      IArray(
        RawSchema.Field("x", summon[Reader[Int]].schema),
        RawSchema.Field("y", summon[Reader[String]].schema)
      )
    )
    assertEquals(withFields.describeSelf, "(x: ..., y: ...)")
    // single-case sum schema
    val sumSchema = RawSchema.Sum(
      IArray(RawSchema.SumCase("Fast", summon[Reader[Int]].schema))
    )
    assertEquals(sumSchema.describeSelf, "(Fast: ...)")
    enum AorB:
      case A, B

    // multi-case sum schema
    val multiSumDesc = RawSchema
      .Sum(
        IArray(
          RawSchema.SumCase("A", Reader.forNull(AorB.A).schema),
          RawSchema.SumCase("B", Reader.forNull(AorB.B).schema)
        )
      )
      .describeSelf
    assert(clue(multiSumDesc).contains("(A: ...)"))
    assert(clue(multiSumDesc).contains("(B: ...)"))
    assert(clue(multiSumDesc).contains(" | "))
    // Vector schema
    assertEquals(
      summon[Reader[Vector[Int]]].schema.describeSelf,
      "Vector[...]"
    )
    assertEquals(
      summon[Reader[Vector[(x: String, y: Int)]]].schema.describeSelf,
      "Vector[...]"
    )
    // Tuple schema
    assertEquals(
      RawSchema.Tuple(IArray(RawSchema.Int)).describeSelf,
      "... *: EmptyTuple"
    )
    assertEquals(
      summon[Reader[(Int, String)]].schema.describeSelf,
      "(..., ...)"
    )
    assertEquals(
      summon[Reader[(Int, (Boolean, String), Long)]].schema.describeSelf,
      "(..., ..., ...)"
    )
    // Option schema
    assertEquals(
      RawSchema.Option(summon[Reader[Int]].schema).describeSelf,
      "Int | Null"
    )
    assertEquals(
      RawSchema.Option(summon[Reader[String]].schema).describeSelf,
      "String | Null"
    )
