package scalanotation.json

import scalanotation.DecodeError
import steps.result.Result

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.nio.charset.StandardCharsets

/** An InputStream that returns at most `chunk` bytes per read — every token boundary in the input
  * eventually lands on a refill boundary as the chunk sizes vary.
  */
final class ChunkedStream(data: Array[Byte], chunk: Int) extends InputStream:
  private var at = 0

  def read(): Int =
    if at >= data.length then -1
    else
      val b = data(at) & 0xff
      at += 1
      b

  override def read(target: Array[Byte], off: Int, len: Int): Int =
    if at >= data.length then -1
    else
      val n = math.min(math.min(len, chunk), data.length - at)
      System.arraycopy(data, at, target, off, n)
      at += n
      n

class JsonStreamSuite extends munit.FunSuite:

  private val chunkSizes = List(1, 2, 3, 7, 64, 4096)

  private def assertStreamsLikeBytes[T: scalanotation.Reader](input: String)(
      using munit.Location
  ): Unit =
    val bytes    = input.getBytes(StandardCharsets.UTF_8)
    val expected = Json.readAs[T](bytes)
    for chunk <- chunkSizes do
      assertEquals(
        Json.readAs[T](ChunkedStream(bytes, chunk)),
        expected,
        clue = s"chunk size $chunk for: ${input.take(120)}"
      )

  test("records stream through every chunk size"):
    assertStreamsLikeBytes[Order](
      """{"id":1000,"sku":"sku-1","qty":3,"price":100.99,"active":true}"""
    )
    assertStreamsLikeBytes[Order](
      """{ "id" : 1000 , "sku" : "sku-1" , "qty" : 3 , "price" : 100.99 , "active" : true }"""
    )

  test("nested structures stream"):
    assertStreamsLikeBytes[Nested](
      """{"name":"outer","inner":{"id":1,"sku":"s","qty":2,"price":1.5,"active":false},"tags":["a","b","c"]}"""
    )
    assertStreamsLikeBytes[Map[String, Vector[Int]]]("""{"a":[1,2,3],"b":[],"c":[42]}""")

  test("strings with escapes and multi-byte UTF-8 stream across boundaries"):
    assertStreamsLikeBytes[String]("\"plain\"")
    assertStreamsLikeBytes[String]("\"esc\\n\\t\\\"\\\\\\u0041end\"")
    assertStreamsLikeBytes[String]("\"日本語と😀の混在テキスト\"")
    assertStreamsLikeBytes[Vector[String]]("""["a","日本語","😀","x\\y"]""")

  test("numbers stream across boundaries"):
    assertStreamsLikeBytes[Vector[Double]]("[1.5,-2.25e10,0.001,123456789.123456789,1e-22]")
    assertStreamsLikeBytes[Vector[Long]]("[1,-9223372036854775808,9223372036854775807]")
    assertStreamsLikeBytes[Int]("-2147483648")
    assertStreamsLikeBytes[Double]("3.141592653589793238462643383279")

  test("literals stream across boundaries"):
    assertStreamsLikeBytes[Vector[Boolean]]("[true,false,true]")
    assertStreamsLikeBytes[Vector[Option[Int]]]("[1,null,3]")

  test("sums and discriminators stream"):
    assertStreamsLikeBytes[Shape]("""{"Circle":{"radius":1.5}}""")
    assertStreamsLikeBytes[ShapeK]("""{"kind":"Rect","width":2.5,"height":3.5}""")

  test("out-of-order and unknown fields stream"):
    assertStreamsLikeBytes[Order](
      """{"active":true,"junk":{"a":[1,{"b":"x"}]},"price":100.99,"qty":3,"sku":"sku-1","id":1000}"""
    )
    assertStreamsLikeBytes[Sparse]("""{"active":true,"retries":3,"id":1,"sku":"s"}""")

  test("dynamic values stream"):
    assertStreamsLikeBytes[JsonValue](
      """{"name":"test","values":[1,2.5,"three",null],"nested":{"flag":false},"pi":3.14159265358979323846}"""
    )

  test("values larger than the refill buffer stream correctly"):
    // a single string longer than the 16 KB buffer forces growth mid-token
    val big   = "x" * 40000
    val json  = s"""{"id":1,"note":"$big","sku":"s","retries":2,"active":true}"""
    val bytes = json.getBytes(StandardCharsets.UTF_8)
    for chunk <- List(1024, 8192) do
      assertEquals(
        Json.readAs[Sparse](ChunkedStream(bytes, chunk)),
        Result.Ok(Sparse(1L, Some(big), "s", Some(2), active = true)),
        clue = s"chunk size $chunk"
      )
    // and a batch far larger than the buffer with ordinary tokens
    val many =
      (1 to 3000).map(i => s"""{"id":$i,"sku":"sku-$i","qty":1,"price":0.5,"active":true}""")
    val batchJson = s"""{"orders":[${many.mkString(",")}]}"""
    assert(batchJson.length > 100000)
    val decoded = Json
      .readAs[OrderBatchLike](ChunkedStream(batchJson.getBytes(StandardCharsets.UTF_8), 4096))
      .getOrElse(fail("stream decode failed"))
    assertEquals(decoded.orders.length, 3000)
    assertEquals(decoded.orders.last.id, 3000L)

  test("premature end of stream is an error"):
    def err(input: String): DecodeError =
      Json.readAs[Order](
        ChunkedStream(input.getBytes(StandardCharsets.UTF_8), 3)
      ) match
        case Result.Err(error) => error
        case other             => fail(s"expected an error, got $other")
    assert(err("""{"id":1000,"sku":"sk""").format.contains("Unterminated"))
    assert(err("""{"id":1000,""").format.nonEmpty)
    assert(err("""{"id":10""").format.nonEmpty)
    assert(err("""{"id":1000,"sku":"s","qty":3,"price":1.5,"active":tr""").format.nonEmpty)

  test("trailing content after the value is an error"):
    val bytes = """{"Circle":{"radius":1.5}} garbage""".getBytes(StandardCharsets.UTF_8)
    assert(Json.readAs[Shape](ChunkedStream(bytes, 2)).isErr)
    // trailing whitespace is fine
    val padded = ("""{"Circle":{"radius":1.5}}""" + " \n\t " * 10).getBytes(StandardCharsets.UTF_8)
    assertEquals(Json.readAs[Shape](ChunkedStream(padded, 2)), Result.Ok(Shape.Circle(1.5)))

  test("error spans stay exact after the buffer scrolls"):
    // a > 16 KB filler field pushes the error far past the first refill; the span must still
    // report the true line and column
    val filler = "y" * 20000
    val json   =
      "{\n\"note\":\"" + filler + "\",\n\"id\":1,\n\"sku\":\"s\",\n\"retries\":\"oops\",\n\"active\":true}"
    val result = Json.readAs[Sparse](ChunkedStream(json.getBytes(StandardCharsets.UTF_8), 1024))
    result match
      case Result.Err(error) =>
        val span = error.span.getOrElse(fail("expected a span"))
        assertEquals(span.line, 5, clue = error.format)
        assert(error.format.contains(".retries"), clue = error.format)
      case other => fail(s"expected an error, got $other")

  test("batched stream decoding reuses the pooled decoder"):
    given ctx: JsonBatchContext = JsonBatchContext.local()
    val bytes                   = """{"id":7,"sku":"s","qty":1,"price":0.5,"active":false}"""
      .getBytes(StandardCharsets.UTF_8)
    var i = 0
    while i < 5 do
      assertEquals(
        Json.batched.readAs[Order](ChunkedStream(bytes, 5)),
        Result.Ok(Order(7L, "s", 1, 0.5, active = false))
      )
      i += 1

  test("plain ByteArrayInputStream decodes"):
    val bytes = """{"id":1,"sku":"s","qty":2,"price":1.5,"active":true}"""
      .getBytes(StandardCharsets.UTF_8)
    assertEquals(
      Json.readAs[Order](new ByteArrayInputStream(bytes)),
      Result.Ok(Order(1L, "s", 2, 1.5, active = true))
    )

case class OrderBatchLike(orders: Vector[Order]) derives scalanotation.ReadWriter
