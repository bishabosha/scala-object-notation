package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.schema.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class ProductDecodingSuite extends ScalanotationSuite:
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

  test("skippable case class readers do not treat skipped nullable fields as duplicates"):
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
        |  debug = true,
        |  refreshSeconds = 5
        |)
        |""".stripMargin

    Readers.readDeclAs[User](input, rootName = "data") match
      case Result.Err(error) =>
        assertEquals(
          error.rootCause,
          DecodeError.FieldOrderMismatch("description", "refreshSeconds")
        )
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

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
