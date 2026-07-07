package scalanotation

import scalanotation.macros.Defaults
import scalanotation.macros.TypedFactories
import steps.result.Result

class DefaultValuesSuite extends munit.FunSuite:

  private def assertReads[T: Reader](input: String)(expected: Result[T, DecodeError]): Unit =
    assertEquals(Readers.readAs[T](input), expected)
    given BatchContext = BatchContext.local()
    assertEquals(Readers.batched.readAs[T](input), expected)

  test("omitted fields decode to their gathered constructor defaults"):
    case class Server(host: String, port: Int = 8080, secure: Boolean = false, name: String = "srv")

    given DefaultValues[Server] = Defaults.derived
    given Configured[Server]    = Configured.default.withDefaultValues
    given Reader[Server]        = Reader.configured.derived

    assertReads[Server]("""(host = "a")""")(Result.Ok(Server("a")))
    assertReads[Server]("""(host = "a", port = 9000)""")(Result.Ok(Server("a", 9000)))
    assertReads[Server]("""(host = "a", secure = true)""")(
      Result.Ok(Server("a", secure = true))
    )
    assertReads[Server]("""(host = "a", port = 1, secure = true, name = "x")""")(
      Result.Ok(Server("a", 1, true, "x"))
    )

  test("defaults fill interleaved and trailing omissions, in order"):
    case class Wide(
        a: Int = 1,
        b: String,
        c: Int = 3,
        d: Int = 4,
        e: String,
        f: Boolean = true
    )

    given DefaultValues[Wide] = Defaults.derived
    given Configured[Wide]    = Configured.default.withDefaultValues
    given Reader[Wide]        = Reader.configured.derived

    assertReads[Wide]("""(b = "x", e = "y")""")(Result.Ok(Wide(b = "x", e = "y")))
    assertReads[Wide]("""(a = 9, b = "x", d = 40, e = "y")""")(
      Result.Ok(Wide(a = 9, b = "x", d = 40, e = "y"))
    )

  test("a field without a default still fails when omitted"):
    case class Server(host: String, port: Int = 8080)

    given DefaultValues[Server] = Defaults.derived
    given Configured[Server]    = Configured.default.withDefaultValues
    given Reader[Server]        = Reader.configured.derived

    Readers.readAs[Server]("""(port = 1)""") match
      case Result.Err(_)    => ()
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("defaults work through the typed-factory configuration"):
    case class Server(host: String, port: Int = 8080, secure: Boolean = false)

    given TypedFactory[Server]  = TypedFactories.derived
    given DefaultValues[Server] = Defaults.derived
    given Configured[Server]    = Configured.typed.withDefaultValues
    given Reader[Server]        = Reader.configured.derived

    assertReads[Server]("""(host = "a")""")(Result.Ok(Server("a")))
    assertReads[Server]("""(host = "a", secure = true)""")(
      Result.Ok(Server("a", secure = true))
    )

  test("enum case constructor defaults fill omitted case fields"):
    enum Job:
      case Retry(name: String, attempts: Int = 3)
      case Stop(reason: String = "done")

    given DefaultValues[Job] = Defaults.derived
    given Configured[Job]    = Configured.default.withDefaultValues
    given Reader[Job]        = Reader.configured.derived

    assertReads[Job]("""(Retry = (name = "a"))""")(Result.Ok(Job.Retry("a")))
    assertReads[Job]("""(Retry = (name = "a", attempts = 5))""")(Result.Ok(Job.Retry("a", 5)))
    assertReads[Job]("""(Stop = (reason = "manual"))""")(Result.Ok(Job.Stop("manual")))

  test("defaults decode from Expr trees identically"):
    case class Server(host: String, port: Int = 8080)

    given DefaultValues[Server] = Defaults.derived
    given Configured[Server]    = Configured.default.withDefaultValues
    given Reader[Server]        = Reader.configured.derived

    assertEquals(
      Expr.NamedTupleExpr(IndexedSeq("host" -> Expr.StringConstant("a"))).decodeAs[Server],
      Result.Ok(Server("a"))
    )

  test("manual bindings install defaults at nested paths"):
    case class Db(host: String, port: Int)
    case class Config(name: String, db: Db)

    given Reader[Db]            = Reader.derived
    given DefaultValues[Config] = DefaultValues.of[Config] { c =>
      Seq(
        c.name    := "app",
        c.db.port := 5432
      )
    }
    given Configured[Config] = Configured.default.withDefaultValues
    given Reader[Config]     = Reader.configured.derived

    assertReads[Config]("""(db = (host = "h"))""")(Result.Ok(Config("app", Db("h", 5432))))
    assertReads[Config]("""(name = "x", db = (host = "h", port = 1))""")(
      Result.Ok(Config("x", Db("h", 1)))
    )

  test("manual bindings step through Option and Vector fields"):
    case class Worker(id: Int, retries: Int)
    case class Db(host: String, port: Int)
    case class Config(db: Option[Db], workers: Vector[Worker])

    given Reader[Db]            = Reader.derived
    given Reader[Worker]        = Reader.derived
    given DefaultValues[Config] = DefaultValues.of[Config] { c =>
      Seq(
        c.db.some.port         := 5432,
        c.workers.each.retries := 3
      )
    }
    given Configured[Config] = Configured.default.withDefaultValues
    given Reader[Config]     = Reader.configured.derived

    assertReads[Config]("""(db = (host = "h"), workers = Vector((id = 1), (id = 2)))""")(
      Result.Ok(Config(Some(Db("h", 5432)), Vector(Worker(1, 3), Worker(2, 3))))
    )

  test("a binding path naming a missing field is rejected at configuration time"):
    case class Config(name: String)

    given DefaultValues[Config] =
      DefaultValues.of[Config] { c =>
        Seq(c.selectDynamic("nope").asInstanceOf[DefaultValues.Path[Int]] := 1)
      }
    given Configured[Config] = Configured.default.withDefaultValues
    intercept[IllegalArgumentException] {
      Reader.configured.derived[Config].schema
    }

  test("defaults and skippable options are mutually exclusive"):
    case class Data(x: Option[Int], y: Int = 1)

    given DefaultValues[Data] = Defaults.derived
    intercept[IllegalArgumentException] {
      Configured.skippable[Data].withDefaultValues
    }
