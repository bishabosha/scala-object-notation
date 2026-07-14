package scalanotation

import steps.result.Result

class NestedOptionSuite extends ScalanotationSuite:
  case class Two(x: Option[Option[Int]]) derives ReadWriter
  case class Three(x: Option[Option[Option[Int]]]) derives ReadWriter
  case class Plain(x: Option[Int]) derives ReadWriter

  private def roundTrip[T: ReadWriter](value: T, expected: String): Unit =
    val rendered = Writers.write(value)
    assertEquals(rendered, expected)
    assertEquals(Readers.readAs[T](rendered), Result.Ok(value))

  test("plain option fields keep the bare encoding"):
    roundTrip(Plain(None), "(x = null)")
    roundTrip(Plain(Some(1)), "(x = 1)")

  test("doubly nested option fields spell Some as a one-case sum"):
    roundTrip(Two(None), "(x = null)")
    roundTrip(Two(Some(None)), "(x = (Some = null))")
    roundTrip(Two(Some(Some(1))), "(x = (Some = 1))")

  test("triply nested option fields wrap each ambiguous level"):
    roundTrip(Three(None), "(x = null)")
    roundTrip(Three(Some(None)), "(x = (Some = null))")
    roundTrip(Three(Some(Some(None))), "(x = (Some = (Some = null)))")
    roundTrip(Three(Some(Some(Some(1)))), "(x = (Some = (Some = 1)))")

  test("nested options work at the top level and inside collections"):
    roundTrip[Option[Option[Int]]](Some(None), "(Some = null)")
    roundTrip[Option[Option[Int]]](None, "null")
    roundTrip[Vector[Option[Option[Int]]]](
      Vector(None, Some(None), Some(Some(3))),
      "Vector(null, (Some = null), (Some = 3))"
    )

  test("an unknown case name in the wrapper is rejected"):
    Readers.readAs[Two]("(x = (Sme = 1))") match
      case Result.Err(err) =>
        assert(err.rootCause.isInstanceOf[DecodeError.UnexpectedField], err.format)
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")

  test("a bare value where the wrapper is required is rejected"):
    Readers.readAs[Two]("(x = 1)") match
      case Result.Err(err) =>
        assert(err.rootCause.isInstanceOf[DecodeError.ExpectedType], err.format)
      case Result.Ok(value) => fail(s"Expected a decode failure, got $value")
