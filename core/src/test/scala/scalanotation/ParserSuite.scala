package scalanotation

import steps.result.Result
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

    val parsed = Parser.quick.parse(input)

    val expected: PartialFunction[SourceFile[Expr], Unit] = {
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
                          Expr.IntConstant(-0x00_1a)
                        )
                      )
                    )
                  ),
                  Expr.NullConstant
                )
              )
            )
          ) =>
        ()
    }

    assert(expected.isDefinedAt(parsed))

  test("tokenize booleans and negative numbers"):
    val input  = "val data = (a = true, b = false, c = -12, d = -1.5f)"
    val parsed = Parser.quick.parse(input)

    val Expr.NamedTupleExpr(_, elements) = parsed.declaration.value: @unchecked
    assertEquals(elements.length, 4)

  test("top level Vector"):
    val input  = "val data = Vector(true)"
    val parsed = Parser.quick.parse(input)

    val Expr.VectorExpr(elements) = parsed.declaration.value: @unchecked
    assertEquals(elements.length, 1)

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

    val parsed = Parser.quick.parse(input)

    val expected: PartialFunction[SourceFile[Expr], Unit] = {
      case SourceFile(
            ValDecl(
              "data",
              Expr.NamedTupleExpr(
                IArray("x", "y"),
                IArray(Expr.IntConstant(1), Expr.BooleanConstant(true))
              )
            )
          ) =>
        ()
    }

    assert(expected.isDefinedAt(parsed))

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

    val decoded        = Parser.parseValueAs[Data](input, name = "data")
    val expected: Data =
      (x = (label = "abc", ys = Vector(1, 2)), y = None, ok = true)

    assertEquals(decoded, Result.Ok(expected))

  test("reject wrong root declaration name"):
    val input   = "val data = (x = 1)"
    val decoded = Parser.parseValueAs[Expr](input, name = "other")
    assertEquals(decoded, Result.Err(DecodeError.UnexpectedRoot("data")))

  test("reject duplicate field decls with Expr"):
    val input   = "val data = (x = 1, x = 2)"
    val decoded = Parser.parseValueAs[Expr](input, name = "data")
    assertEquals(decoded.getErr.rootCause, DecodeError.DuplicateField("x"))
    assertEquals(decoded.getErr.path, List(".x"))

  test("reject duplicate field decls with Expr, nested"):
    val input   = "val data = (a = 1, b = (x = true, x = null), c = 3)"
    val decoded = Parser.parseValueAs[Expr](input, name = "data")
    assertEquals(decoded.getErr.rootCause, DecodeError.DuplicateField("x"))
    assertEquals(decoded.getErr.path, List(".b", ".x"))
    assertEquals(decoded.getErr.span.map(span => (span.line, span.column)), Some((1, 35)))

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

    val decoded        = Parser.parseValueAs[Data](input, name = "data")
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

    val decoded        = Parser.parseValueAs[Data](input, name = "data")
    val expected: Data = (missing = None, present = Some(41))

    assertEquals(decoded, Result.Ok(expected))

  test("report inner schema mismatches for Option values"):
    type Data = (x: Option[Int])

    val input    = "val data = (x = true)"
    val obtained = Parser.parseValueAs[Data](input, name = "data")

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

    val sc = summon[TaggedSchema[Data]]
    sc.schema match
      case Schema.Vector(_, builder: Internal.BuildIArray[Int]) =>
        ()
      case _ => fail(s"Expected a BuildIArray schema, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Parser.parseValueAs[Data](input, name = "data")
    val expected: PartialFunction[Data, Unit] = { case IArray(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )

  test("parse dense arrays"):
    type Data = Array[Int]

    val sc = summon[TaggedSchema[Data]]
    sc.schema match
      case Schema.Vector(_, builder: Internal.BuildArray[Int]) =>
        ()
      case _ => fail(s"Expected a BuildArray schema, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Parser.parseValueAs[Data](input, name = "data")
    val expected: PartialFunction[Data, Unit] = { case Array(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
    )

  test("parse arbitrary sequence") {
    type Data = List[Int]

    val sc = summon[TaggedSchema[Data]]
    sc.schema match
      case Schema.Vector(_, builder: Internal.SeqFactoryVector[Int, List]) =>
        ()
      case _ => fail(s"Expected a SeqFactoryVector schema, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Parser.parseValueAs[Data](input, name = "data")
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

    val sc = summon[TaggedSchema[Data]]
    sc.schema match
      case Schema.Vector(_, builder: Internal.BuildVector[Int]) =>
        ()
      case _ => fail(s"Expected a BuildVector schema, got ${sc.schema.describeSelf}")

    val input =
      """val data = Vector(1, 2, 3)
        |""".stripMargin

    val decoded                               = Parser.parseValueAs[Data](input, name = "data")
    val expected: PartialFunction[Data, Unit] = { case Vector(1, 2, 3) =>
      ()
    }
    assertEquals(
      true,
      expected.isDefinedAt(decoded.getOrElse(fail(s"Expected successful parse, got $decoded")))
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

    val decoded        = Parser.parseValueAs[Data](input, name = "data")
    val expected: Data =
      (
        items = Vector((name = "a", value = 1), (name = "b", value = 2)),
        total = 2L
      )

    assertEquals(decoded, Result.Ok(expected))

  test("report schema mismatches during decoding"):
    type Data = (x: Int)

    val input    = "val data = (x = true)"
    val obtained = Parser.parseValueAs[Data](input, name = "data")

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

    val obtained = Parser.parseValueAs[Data](input, name = "data")
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

    val obtained  = Parser.parseValueAs[Expr](input, name = "data").get
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
    val obtained                              = Parser.parseValueAs[Data](input, name = "data")
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

    val obtained = Parser.parseValueAs[Data](input, name = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.FieldOrderMismatch("x", "y"))
        assertEquals(
          error.span.map(span => (span.line, span.column)),
          Some((1, 13))
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("decode custom types from single-level TaggedSchema transformations"):
    import java.time.LocalDate

    enum Mode:
      case Fast, Safe

    final case class User(name: String, age: Int)
    final case class Schedule(dates: Vector[LocalDate])

    given TaggedSchema[Mode] =
      summon[TaggedSchema[String]].emap {
        case "fast" => Result.Ok(Mode.Fast)
        case "safe" => Result.Ok(Mode.Safe)
        case other  => Result.Err(DecodeError.Custom(s"Unknown mode '$other'"))
      }

    given TaggedSchema[LocalDate] =
      summon[TaggedSchema[String]].emap { raw =>
        Result.catchException({ case _: java.time.format.DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    given TaggedSchema[User] =
      summon[TaggedSchema[(name: String, age: Int)]].map { data =>
        User(name = data.name, age = data.age)
      }

    given TaggedSchema[Schedule] =
      summon[TaggedSchema[Vector[LocalDate]]].map(Schedule(_))

    type Data = (owner: User, mode: Mode, schedule: Schedule)

    val input =
      """val data = (
        |  owner = (name = "Ada", age = 41),
        |  mode = "fast",
        |  schedule = Vector("2026-03-14", "2026-03-15")
        |)
        |""".stripMargin

    val decoded        = Parser.parseValueAs[Data](input, name = "data")
    val expected: Data =
      (
        owner = User("Ada", 41),
        mode = Mode.Fast,
        schedule = Schedule(Vector(LocalDate.parse("2026-03-14"), LocalDate.parse("2026-03-15")))
      )

    assertEquals(decoded, Result.Ok(expected))

  test("report composed paths for custom string decoders inside vectors"):
    import java.time.LocalDate

    given TaggedSchema[LocalDate] =
      summon[TaggedSchema[String]].emap { raw =>
        try Result.Ok(LocalDate.parse(raw))
        catch
          case _: java.time.format.DateTimeParseException =>
            Result.Err(DecodeError.Custom(s"Invalid ISO date '$raw'"))
      }

    type Data = (dates: Vector[LocalDate])

    val input =
      """val data = (
        |  dates = Vector("2026-03-14", "2026-99-99")
        |)
        |""".stripMargin

    val obtained = Parser.parseValueAs[Data](input, name = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".dates", "[1]"))
        assertEquals(error.rootCause, DecodeError.Custom("Invalid ISO date '2026-99-99'"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("report custom sequence validation errors at the wrapped field"):
    final case class NonEmptyInts(values: Vector[Int])

    given TaggedSchema[NonEmptyInts] =
      summon[TaggedSchema[Vector[Int]]].emap { values =>
        if values.nonEmpty then Result.Ok(NonEmptyInts(values))
        else Result.Err(DecodeError.Custom("Expected at least one integer"))
      }

    type Data = (items: NonEmptyInts)

    val obtained = Parser.parseValueAs[Data]("val data = (items = Vector())", name = "data")
    obtained match
      case Result.Err(error) =>
        assertEquals(error.path, List(".items"))
        assertEquals(error.rootCause, DecodeError.Custom("Expected at least one integer"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("decode directly into nested case classes"):
    import java.time.LocalDate

    final case class Metadata(created: LocalDate, tags: Vector[String]) derives TaggedSchema
    final case class User(name: String, age: Int, metadata: Metadata) derives TaggedSchema

    given TaggedSchema[LocalDate] =
      summon[TaggedSchema[String]].emap { raw =>
        Result.catchException({ case _: java.time.format.DateTimeParseException =>
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

    val decoded  = Parser.parseValueAs[User](input, name = "data")
    val expected = User(
      name = "Ada",
      age = 41,
      metadata = Metadata(
        created = LocalDate.parse("2026-03-14"),
        tags = Vector("compiler", "scala")
      )
    )

    assertEquals(decoded, Result.Ok(expected))

  test("report nested paths for direct case class decoders"):
    import java.time.LocalDate

    final case class Metadata(created: LocalDate)
    final case class User(metadata: Metadata)

    given TaggedSchema[LocalDate] =
      summon[TaggedSchema[String]].emap { raw =>
        Result.catchException({ case _: java.time.format.DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    given TaggedSchema[Metadata] = TaggedSchema.ofFields[Metadata]
    given TaggedSchema[User]     = TaggedSchema.ofFields[User]

    val input =
      """val data = (
        |  metadata = (
        |    created = "bad-date"
        |  )
        |)
        |""".stripMargin

    Parser.parseValueAs[User](input, name = "data") match
      case Result.Err(error) =>
        assertEquals(error.path, List(".metadata", ".created"))
        assertEquals(error.rootCause, DecodeError.Custom("Invalid ISO date 'bad-date'"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("derive enum schemas with nullary and structured cases"):
    import java.time.LocalDate

    enum Mode derives TaggedSchema:
      case Fast
      case Scheduled(at: LocalDate, retries: Int)

    given TaggedSchema[LocalDate] =
      summon[TaggedSchema[String]].emap { raw =>
        Result.catchException({ case _: java.time.format.DateTimeParseException =>
          DecodeError.Custom(s"Invalid ISO date '$raw'")
        }) {
          LocalDate.parse(raw)
        }
      }

    val fast      = Parser.parseValueAs[Mode]("val data = (Fast = null)", name = "data")
    val scheduled = Parser.parseValueAs[Mode](
      """val data = (
        |  Scheduled = (
        |    at = "2026-03-15",
        |    retries = 2
        |  )
        |)
        |""".stripMargin,
      name = "data"
    )

    assertEquals(fast, Result.Ok(Mode.Fast))
    assertEquals(
      scheduled,
      Result.Ok(Mode.Scheduled(LocalDate.parse("2026-03-15"), 2))
    )

  test("derive case object schemas"):
    import java.time.LocalDate

    case object Foo derives TaggedSchema

    val foo = Parser.parseValueAs[Foo.type]("val data = (Foo = null)", name = "data")
    assertEquals(foo, Result.Ok(Foo))

  test("report nested enum case paths"):
    import java.time.LocalDate

    enum Mode derives TaggedSchema:
      case Fast
      case Scheduled(at: LocalDate)

    given TaggedSchema[LocalDate] =
      summon[TaggedSchema[String]].emap { raw =>
        Result.catchException({ case _: java.time.format.DateTimeParseException =>
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

    Parser.parseValueAs[Data](input, name = "data") match
      case Result.Err(error) =>
        assertEquals(error.path, List(".mode", ".Scheduled", ".at"))
        assertEquals(error.rootCause, DecodeError.Custom("Invalid ISO date 'bad-date'"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("no decoder is derived for Any"):
    val errors = typeCheckErrors("summon[scalanotation.TaggedSchema[Any]]")
    assert(errors.nonEmpty)

  test("no decoder is derived for Vector[Any]"):
    val errors =
      typeCheckErrors("summon[scalanotation.TaggedSchema[Vector[Any]]]")
    assert(errors.nonEmpty)

  test("no decoder is derived for nested Option"):
    val errors = typeCheckErrors(
      "type Data = (x: Option[Option[Int]])\nsummon[scalanotation.TaggedSchema[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".x"))
    assert(
      clue(errors.head.message)
        .contains("TaggedSchema[Option[Option[?]]] is not supported")
    )

  test("compile-time derivation error includes nested field path"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (outer: (bad: Box[Int]))\nsummon[scalanotation.TaggedSchema[Data]]"
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
      "type Data = (outer: (inner: Vector[(sub1: (bad: Box[Int]))]))\nsummon[scalanotation.TaggedSchema[Data]]"
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
      "type Data = Vector[(sub1: (bad: Box[Int]))]\nsummon[scalanotation.TaggedSchema[Data]]"
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
      "type Data = (items: Vector[(bad: Box[Int])])\nsummon[scalanotation.TaggedSchema[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("'.items[].bad'"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("Schema.describeSelf"):
    // primitive schemas
    assertEquals(Schema.Int.describeSelf, "Int")
    assertEquals(Schema.Long.describeSelf, "Long")
    assertEquals(Schema.Float.describeSelf, "Float")
    assertEquals(Schema.Double.describeSelf, "Double")
    assertEquals(Schema.Boolean.describeSelf, "Boolean")
    assertEquals(Schema.Char.describeSelf, "Char")
    assertEquals(Schema.String.describeSelf, "String")
    assertEquals(Schema.AnyExpr.describeSelf, "Any")
    assertEquals(Schema.Nullary(null).describeSelf, "Null")
    // empty named tuple
    assertEquals(
      Schema.NamedTuple(IArray.empty, _ => ()).describeSelf,
      "AnyNamedTuple"
    )
    // named tuple with fields
    val withFields = Schema.NamedTuple(
      IArray(
        Schema.Field("x", summon[TaggedSchema[Int]]),
        Schema.Field("y", summon[TaggedSchema[String]])
      ),
      _ => ()
    )
    assertEquals(withFields.describeSelf, "(x: ..., y: ...)")
    // single-case sum schema
    val sumSchema = Schema.Sum(
      Map("Fast" -> Schema.SumCase("Fast", summon[TaggedSchema[Int]]))
    )
    assertEquals(sumSchema.describeSelf, "(Fast: ...)")
    enum AorB:
      case A, B

    // multi-case sum schema
    val multiSumDesc = Schema
      .Sum(
        Map(
          "A" -> Schema.SumCase("A", TaggedSchema.forNull(AorB.A)),
          "B" -> Schema.SumCase("B", TaggedSchema.forNull(AorB.B))
        )
      )
      .describeSelf
    assert(clue(multiSumDesc).contains("(A: ...)"))
    assert(clue(multiSumDesc).contains("(B: ...)"))
    assert(clue(multiSumDesc).contains(" | "))
    // Vector schema
    assertEquals(
      summon[TaggedSchema[Vector[Int]]].schema.describeSelf,
      "Vector[...]"
    )
    assertEquals(
      summon[TaggedSchema[Vector[(x: String, y: Int)]]].schema.describeSelf,
      "Vector[...]"
    )
    // Option schema
    assertEquals(
      Schema.Option(summon[TaggedSchema[Int]]).describeSelf,
      "Int | Null"
    )
    assertEquals(
      Schema.Option(summon[TaggedSchema[String]]).describeSelf,
      "String | Null"
    )
