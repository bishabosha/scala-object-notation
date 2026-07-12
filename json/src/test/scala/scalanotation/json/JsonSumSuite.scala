package scalanotation.json

import scalanotation.Configured
import scalanotation.ReadWriter
import steps.result.Result

enum Shape derives ReadWriter:
  case Circle(radius: Double)
  case Rect(width: Double, height: Double)
  case Origin

enum ShapeK:
  case Circle(radius: Double)
  case Rect(width: Double, height: Double)
  case Origin
object ShapeK:
  given Configured[ShapeK] = Configured.discriminator[ShapeK]("kind")
  given ReadWriter[ShapeK] = ReadWriter.configured.derived

class JsonSumSuite extends munit.FunSuite:

  private def errOf[T](result: Result[T, scalanotation.DecodeError]): scalanotation.DecodeError =
    result match
      case Result.Err(error) => error
      case other             => fail(s"expected an error, got $other")

  test("sum values encode as single-field objects"):
    assertEquals(Json.write[Shape](Shape.Circle(1.5)), """{"Circle":{"radius":1.5}}""")
    assertEquals(
      Json.write[Shape](Shape.Rect(2.0, 3.0)),
      """{"Rect":{"width":2.0,"height":3.0}}"""
    )
    assertEquals(Json.write[Shape](Shape.Origin), """{"Origin":null}""")

  test("sum values decode"):
    assertEquals(
      Json.readAs[Shape]("""{"Circle":{"radius":1.5}}"""),
      Result.Ok(Shape.Circle(1.5))
    )
    assertEquals(
      Json.readAs[Shape]("""{"Rect":{"width":2.0,"height":3.0}}"""),
      Result.Ok(Shape.Rect(2.0, 3.0))
    )
    assertEquals(Json.readAs[Shape]("""{"Origin":null}"""), Result.Ok(Shape.Origin))

  test("sum round trips"):
    val shapes: List[Shape] =
      List(Shape.Circle(1.5), Shape.Rect(2.0, 3.0), Shape.Origin)
    for shape <- shapes do
      assertEquals(Json.readAs[Shape](Json.write(shape)), Result.Ok(shape), clue = shape.toString)

  test("unknown case is an error"):
    val result = Json.readAs[Shape]("""{"Triangle":{"a":1}}""")
    assert(result.isErr)
    assert(errOf(result).format.contains("Triangle"))

  test("sum with two fields is an error"):
    assert(Json.readAs[Shape]("""{"Circle":{"radius":1.5},"Rect":{}}""").isErr)

  test("empty object is not a sum value"):
    assert(Json.readAs[Shape]("{}").isErr)

  test("discriminator sums encode with the discriminator first"):
    assertEquals(
      Json.write[ShapeK](ShapeK.Circle(1.5)),
      """{"kind":"Circle","radius":1.5}"""
    )
    assertEquals(
      Json.write[ShapeK](ShapeK.Rect(2.0, 3.0)),
      """{"kind":"Rect","width":2.0,"height":3.0}"""
    )
    assertEquals(Json.write[ShapeK](ShapeK.Origin), """{"kind":"Origin"}""")

  test("discriminator sums decode"):
    assertEquals(
      Json.readAs[ShapeK]("""{"kind":"Circle","radius":1.5}"""),
      Result.Ok(ShapeK.Circle(1.5))
    )
    assertEquals(
      Json.readAs[ShapeK]("""{"kind":"Rect","width":2.0,"height":3.0}"""),
      Result.Ok(ShapeK.Rect(2.0, 3.0))
    )
    assertEquals(Json.readAs[ShapeK]("""{"kind":"Origin"}"""), Result.Ok(ShapeK.Origin))

  test("discriminator round trips"):
    val shapes: List[ShapeK] =
      List(ShapeK.Circle(1.5), ShapeK.Rect(2.0, 3.0), ShapeK.Origin)
    for shape <- shapes do
      assertEquals(Json.readAs[ShapeK](Json.write(shape)), Result.Ok(shape), clue = shape.toString)

  test("discriminator must come first"):
    assert(Json.readAs[ShapeK]("""{"radius":1.5,"kind":"Circle"}""").isErr)

  test("unknown discriminator case is an error"):
    val result = Json.readAs[ShapeK]("""{"kind":"Blob"}""")
    assert(result.isErr)
    assert(errOf(result).format.contains("Blob"))

  test("missing discriminator is an error"):
    assert(Json.readAs[ShapeK]("{}").isErr)

  test("re-sent discriminator field is a duplicate"):
    assert(Json.readAs[ShapeK]("""{"kind":"Circle","kind":"Circle","radius":1.5}""").isErr)

  test("discriminator payload with whitespace and escaped case name"):
    assertEquals(
      Json.readAs[ShapeK]("""{ "kind" : "Circle" , "radius" : 1.5 }"""),
      Result.Ok(ShapeK.Circle(1.5))
    )
    assertEquals(
      Json.readAs[ShapeK]("""{"kind":"Circle","radius":1.5}"""),
      Result.Ok(ShapeK.Circle(1.5))
    )
