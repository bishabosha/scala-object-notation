package scalanotation

import scalanotation.internal.RawSchema
import steps.result.Result

class TypedFactorySuite extends ScalanotationSuite:

  /** decodes through the plain (one-shot) API and through a pooled batch context, which is the path
    * that exercises the builder-slots factories; both must agree
    */
  private def assertReads[T: Reader](input: String)(expected: Result[T, DecodeError]): Unit =
    assertEquals(Readers.readAs[T](input), expected)
    given BatchContext = BatchContext.local()
    assertEquals(Readers.batched.readAs[T](input), expected)

  test("typed configuration attaches a slots factory to derived products"):
    final case class Point(x: Int, y: Int)

    given Configured[Point] = Configured.typed
    given Reader[Point]     = Reader.configured.derived

    summon[Reader[Point]].schema match
      case nt: RawSchema.NamedTuple =>
        assert(nt.read != null)
        assert(nt.read.nn.slotsFactory != null)
      case other => fail(s"Expected a named tuple schema, got ${other.describeSelf}")

    assertReads[Point]("(x = 1, y = -2)")(Result.Ok(Point(1, -2)))

  test("typed factories pull each constructor argument from the decoder slots"):
    final case class Point(x: Int, y: Int)

    val config  = Configured.typed[Point]
    val factory = config.typedFactories.nn.selfFactory.nn
    val slots   = BuilderSlots().reset(2)
    slots.setInt(0, 3)
    slots.setInt(1, 4)

    assertEquals(factory.fromSlots(slots), Point(3, 4))

  test("typed factories construct products through every slot kind"):
    final case class AllKinds(
        s: String,
        c: Char,
        i: Int,
        l: Long,
        f: Float,
        d: Double,
        b: Boolean,
        o: Option[Int],
        v: Vector[Int]
    )

    given Configured[AllKinds] = Configured.typed
    given Reader[AllKinds]     = Reader.configured.derived

    assertReads[AllKinds](
      """(s = "str", c = 'c', i = -1, l = 2147483648L, f = 1.5f, d = 2.25, b = true, o = 7, v = Vector(1, 2, 3))"""
    )(
      Result.Ok(
        AllKinds("str", 'c', -1, 2147483648L, 1.5f, 2.25, true, Some(7), Vector(1, 2, 3))
      )
    )

  test("typed factories unbox values that a mapped field schema had to box"):
    final case class Sized(width: Int, label: String)

    given Reader[Int]       = summon[Reader[String]].map(_.length)
    given Configured[Sized] = Configured.typed
    given Reader[Sized]     = Reader.configured.derived

    assertReads[Sized]("""(width = "wide", label = "x")""")(Result.Ok(Sized(4, "x")))

  test("typed factories compose with nested products and generic case classes"):
    final case class Inner(count: Int)
    final case class Box[A](value: A, label: String)

    given Reader[Inner]          = Reader.derived
    given Configured[Box[Inner]] = Configured.typed
    given Reader[Box[Inner]]     = Reader.configured.derived

    assertReads[Box[Inner]]("""(value = (count = 2), label = "b")""")(
      Result.Ok(Box(Inner(2), "b"))
    )

  test("typed configuration attaches per-case factories on discriminator sums"):
    enum Shape:
      case Circle(radius: Double)
      case Rect(w: Int, h: Int)
      case Dot

    given Configured[Shape] = Configured.discriminator[Shape]("kind").withTypedFactories
    given Reader[Shape]     = Reader.configured.derived

    assertReads[Shape]("""(kind = "Rect", w = 3, h = 4)""")(Result.Ok(Shape.Rect(3, 4)))
    assertReads[Shape]("""(kind = "Circle", radius = 1.5)""")(Result.Ok(Shape.Circle(1.5)))
    assertReads[Shape]("""(kind = "Dot")""")(Result.Ok(Shape.Dot))

  test("typed factories apply to plain sum cases and round-trip with writers"):
    enum Mode:
      case Fast
      case Scheduled(at: String, retries: Int)

    given Configured[Mode] = Configured.typed
    given ReadWriter[Mode] = ReadWriter.configured.derived

    val scheduled: Mode = Mode.Scheduled("soon", 3)
    val fast: Mode      = Mode.Fast

    assertReads[Mode](Writers.write(scheduled))(Result.Ok(scheduled))
    assertReads[Mode](Writers.write(fast))(Result.Ok(fast))

  test("typed skippable products fill skipped fields before the factory runs"):
    final case class User(name: String, nickname: Option[String], age: Int)

    given Configured[User] = Configured.skippable[User].withTypedFactories
    given Reader[User]     = Reader.configured.derived

    assertReads[User]("""(name = "Ada", age = 36)""")(Result.Ok(User("Ada", None, 36)))
    assertReads[User]("""(name = "Ada", nickname = "ada", age = 36)""")(
      Result.Ok(User("Ada", Some("ada"), 36))
    )

  test("pooled decoders build tuples and named tuples directly from the typed slots"):
    type Mixed = (Int, String, (ok: Boolean), Long)

    assertReads[Mixed]("""(1, "two", (ok = true), 3L)""")(
      Result.Ok((1, "two", (ok = true), 3L))
    )
    assertReads[Int *: String *: EmptyTuple]("""1 *: "two" *: EmptyTuple""")(
      Result.Ok(1 *: "two" *: EmptyTuple)
    )
    assertReads[(x: Int, label: String)]("""(x = 5, label = "five")""")(
      Result.Ok((x = 5, label = "five"))
    )

  test("typed factories report decode errors like the legacy path"):
    final case class Point(x: Int, y: Int)

    given Configured[Point] = Configured.typed
    given Reader[Point]     = Reader.configured.derived

    def check(result: Result[Point, DecodeError]): Unit = result match
      case Result.Err(error) =>
        assertEquals(error.path, List(".y"))
        assertEquals(error.rootCause, DecodeError.ExpectedType("Int", "string literal \"two\""))
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

    check(Readers.readAs[Point]("""(x = 1, y = "two")"""))
    given BatchContext = BatchContext.local()
    check(Readers.batched.readAs[Point]("""(x = 1, y = "two")"""))
