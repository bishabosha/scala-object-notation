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
    // a sum value carries exactly one case field, which `NamedTuple.Empty` cannot provide
    Readers.readAs[Job]("NamedTuple.Empty") match
      case Result.Err(error) =>
        assertEquals(error.rootCause, DecodeError.FieldCountMismatch(1, 0))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("defaults decode from Expr trees identically"):
    case class Server(host: String, port: Int = 8080)

    given DefaultValues[Server] = Defaults.derived
    given Configured[Server]    = Configured.default.withDefaultValues
    given Reader[Server]        = Reader.configured.derived

    assertEquals(
      Expr.NamedTupleExpr(IndexedSeq("host" -> Expr.StringConstant("a"))).decodeAs[Server],
      Result.Ok(Server("a"))
    )

  test("derived defaults compose per level through nested, Option- and Vector-represented fields"):
    case class Probe(path: String = "/health", timeout: Int = 30)
    case class Endpoint(host: String, port: Int = 8080, probe: Option[Probe] = None)
    case class Cluster(
        name: String = "main",
        endpoints: List[Endpoint] = Nil,
        arbiter: Option[Endpoint] = None
    )

    // each level derives and configures its OWN defaults; outer schemas embed the configured
    // reader schema, so the installed defaults ride along inside Option and Seq representations
    given DefaultValues[Probe]    = Defaults.derived
    given Configured[Probe]       = Configured.default.withDefaultValues
    given Reader[Probe]           = Reader.configured.derived
    given DefaultValues[Endpoint] = Defaults.derived
    given Configured[Endpoint]    = Configured.default.withDefaultValues
    given Reader[Endpoint]        = Reader.configured.derived
    given DefaultValues[Cluster]  = Defaults.derived
    given Configured[Cluster]     = Configured.default.withDefaultValues
    given Reader[Cluster]         = Reader.configured.derived

    // every level omits fields at once: Cluster.name, Endpoint.port/probe, Probe.timeout
    assertReads[Cluster](
      """(endpoints = Vector((host = "a"), (host = "b", port = 9), (host = "c", probe = (path = "/p"))), arbiter = (host = "z", probe = (timeout = 5)))"""
    )(
      Result.Ok(
        Cluster(
          "main",
          List(
            Endpoint("a"),
            Endpoint("b", 9),
            Endpoint("c", probe = Some(Probe("/p")))
          ),
          Some(Endpoint("z", probe = Some(Probe(timeout = 5))))
        )
      )
    )
    // `()` is the Unit literal, never a record — even with every field defaulted; the empty
    // named tuple is spelled the way Scala spells it
    Readers.readAs[Cluster]("""()""") match
      case Result.Err(error) =>
        assert(error.rootCause.isInstanceOf[DecodeError.UnitValueNotAllowed], error.toString)
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")
    assertReads[Cluster]("""NamedTuple.Empty""")(Result.Ok(Cluster()))
    assertReads[Cluster](
      """(endpoints = Vector((host = "a", probe = NamedTuple.Empty)))"""
    )(
      Result.Ok(Cluster(endpoints = List(Endpoint("a", probe = Some(Probe())))))
    )
    // a record with any non-defaulted field reports the shortfall precisely
    Readers.readAs[Endpoint]("""NamedTuple.Empty""") match
      case Result.Err(error) =>
        assert(error.rootCause.isInstanceOf[DecodeError.FieldCountMismatch], error.toString)
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")
    // the Expr layer has no Unit: an empty NamedTupleExpr IS the empty named tuple
    assertEquals(Expr.NamedTupleExpr(IndexedSeq.empty).decodeAs[Cluster], Result.Ok(Cluster()))
    assertEquals(Expr.NamedTupleExpr(IndexedSeq.empty).render, "NamedTuple.Empty")

  test("batches of heterogeneous field orders decode correctly while the loop learns arrival"):
    case class Rec(a: Int = 1, b: Int = 2, c: Int = 3, d: Int = 4)

    given DefaultValues[Rec] = Defaults.derived
    given Configured[Rec]    = Configured.default.withDefaultValues
    given Reader[Rec]        = Reader.configured.derived

    // each element arrives with a different subset/order, so the learned arrival prediction is
    // wrong on most transitions and must fall back to the resolver without changing results
    assertReads[Vector[Rec]](
      """Vector((a = 10, c = 30), (a = 1, b = 2, c = 3, d = 4), (d = 40), (b = 20), (a = 10, c = 30))"""
    )(
      Result.Ok(
        Vector(
          Rec(a = 10, c = 30),
          Rec(1, 2, 3, 4),
          Rec(d = 40),
          Rec(b = 20),
          Rec(a = 10, c = 30)
        )
      )
    )
    // error parity must not depend on what earlier decodes learned: the same bad input fails
    // identically before and after batches that train the prediction
    def rootCauseOf(input: String): DecodeError =
      Readers.readAs[Rec](input) match
        case Result.Err(error) => error.rootCause
        case Result.Ok(value)  => fail(s"Expected a decode failure for $input, got $value")
    val duplicateBefore = rootCauseOf("""(a = 1, a = 2)""")
    val reorderBefore   = rootCauseOf("""(c = 3, a = 1)""")
    assertReads[Vector[Rec]]("""Vector((c = 30, d = 40), (b = 20, d = 40))""")(
      Result.Ok(Vector(Rec(c = 30, d = 40), Rec(b = 20, d = 40)))
    )
    assertEquals(rootCauseOf("""(a = 1, a = 2)"""), duplicateBefore)
    assertEquals(rootCauseOf("""(c = 3, a = 1)"""), reorderBefore)

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

  test("each steps any Seq-represented field via the VectorRepr witness"):
    case class Worker(id: Int, retries: Int)
    case class Config(workers: List[Worker])

    given Reader[Worker]        = Reader.derived
    given DefaultValues[Config] = DefaultValues.of[Config] { c =>
      Seq(c.workers.each.retries := 3)
    }
    given Configured[Config] = Configured.default.withDefaultValues
    given Reader[Config]     = Reader.configured.derived

    assertReads[Config]("""(workers = Vector((id = 1), (id = 2)))""")(
      Result.Ok(Config(List(Worker(1, 3), Worker(2, 3))))
    )

  test("a custom witness makes an Option-represented type steppable"):
    case class Db(host: String, port: Int)
    // decodes through an Option schema (bare value or null), mapped to the custom wrapper
    case class Cached[A](value: Option[A])
    given [A: Reader]: Reader[Cached[A]] =
      summon[Reader[Option[A]]].map(Cached(_))
    given [A]: (DefaultValues.OptionRepr[Cached[A]] { type Inner = A }) =
      new DefaultValues.OptionRepr[Cached[A]] { type Inner = A }

    case class Config(db: Cached[Db])
    given Reader[Db]            = Reader.derived
    given DefaultValues[Config] = DefaultValues.of[Config] { c =>
      Seq(c.db.some.port := 5432)
    }
    given Configured[Config] = Configured.default.withDefaultValues
    given Reader[Config]     = Reader.configured.derived

    assertReads[Config]("""(db = (host = "h"))""")(
      Result.Ok(Config(Cached(Some(Db("h", 5432)))))
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
