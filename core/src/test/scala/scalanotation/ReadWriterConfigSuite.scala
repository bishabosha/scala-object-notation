package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.schema.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class ReadWriterConfigSuite extends ScalanotationSuite:
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

  test("configured discriminator sum decoder rejects payload fields that repeat discriminator"):
    enum Command:
      case Copy(kind: String, to: String)

    given Configured[Command] = Configured.discriminator("kind")
    given Reader[Command]     = Reader.configured.derived

    Readers.readAs[Command]("""(kind = "Copy", kind = "source", to = "b")""") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.DuplicateField("kind"))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("configured discriminator sum decoder accepts non-plain case name literals"):
    enum Command:
      case Copy(from: String, to: String)

    given Configured[Command] = Configured.discriminator("kind")
    given Reader[Command]     = Reader.configured.derived

    val expected = Result.Ok(Command.Copy("a", "b"))
    // the fast path slice-matches escape-free literals; each discrepancy below must fall
    // back to the general string decode and resolve the identical case
    assertEquals(Readers.readAs[Command]("""(kind = "Copy", from = "a", to = "b")"""), expected)
    assertEquals(
      Readers.readAs[Command]("""(kind = "Co" + "py", from = "a", to = "b")"""),
      expected
    )
    assertEquals(
      Readers.readAs[Command]("""(kind = "" + "Copy", from = "a", to = "b")"""),
      expected
    )
    assertEquals(
      Readers.readAs[Command]("""(kind = "Co" /* comment */ + "py", from = "a", to = "b")"""),
      expected
    )

  test("configured discriminator sum decoder reports unknown cases in any literal form"):
    enum Command:
      case Copy(from: String, to: String)

    given Configured[Command] = Configured.discriminator("kind")
    given Reader[Command]     = Reader.configured.derived

    def unknownCase(input: String, decodedName: String): Unit =
      Readers.readAs[Command](input) match
        case Result.Err(error) =>
          assertEquals(error.rootCause, DecodeError.UnexpectedField(decodedName))
        case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    unknownCase("""(kind = "Move", from = "a", to = "b")""", "Move")
    unknownCase("""(kind = "Mo" + "ve", from = "a", to = "b")""", "Move")
    // an escape diverts to the general decode, which processes it before reporting
    unknownCase("""(kind = "Mo\tve", from = "a", to = "b")""", "Mo\tve")

  test("configured discriminator sum decoder rejects non-string discriminator values"):
    enum Command:
      case Copy(from: String, to: String)

    given Configured[Command] = Configured.discriminator("kind")
    given Reader[Command]     = Reader.configured.derived

    Readers.readAs[Command]("""(kind = Copy, from = "a", to = "b")""") match
      case Result.Err(error) => assert(error.rootCause.isInstanceOf[DecodeError.ExpectedType])
      case Result.Ok(value)  => fail(s"Expected a decode failure, got $value")

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

  test("configured skippable products may contain only Option fields"):
    final case class Data(x: Option[Int], y: Option[String])

    given Configured[Data] = Configured.skippable
    given Reader[Data]     = Reader.configured.derived

    assertEquals(Readers.readAs[Data]("""(x = 1)"""), Result.Ok(Data(Some(1), None)))
    // with every field skipped the record is empty, which is spelled NamedTuple.Empty — `()`
    // stays the Unit literal
    assertEquals(Readers.readAs[Data]("NamedTuple.Empty"), Result.Ok(Data(None, None)))
    Readers.readAs[Data]("()") match
      case Result.Err(error) =>
        assert(error.rootCause.isInstanceOf[DecodeError.UnitValueNotAllowed], error.toString)
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

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

  test("skippable derivation supports products with only Option fields"):
    final case class Flags(verbose: Option[Boolean], level: Option[Int])

    given ReadWriter[Flags] = ReadWriter.skippable.derived

    assertEquals(Readers.readAs[Flags]("""(level = 3)"""), Result.Ok(Flags(None, Some(3))))
    assertEquals(Readers.readAs[Flags]("NamedTuple.Empty"), Result.Ok(Flags(None, None)))
    // the writer keeps explicit nulls, so all-None round-trips without the literal
    assertEquals(Writers.write(Flags(None, None)), """(verbose = null, level = null)""")
