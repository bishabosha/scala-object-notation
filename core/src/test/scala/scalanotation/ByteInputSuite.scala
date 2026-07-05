package scalanotation

import java.nio.charset.StandardCharsets.UTF_8
import steps.result.Result

/** The UTF-8 byte input mode must decode exactly like the String mode: ASCII inputs widen straight
  * into the scanner's buffer, non-ASCII inputs delegate to the JDK decoder, and identifiers are
  * never materialized as Strings on either path.
  */
class ByteInputSuite extends munit.FunSuite:
  private def assertSameAsString[T: Reader](input: String)(using munit.Location): Unit =
    assertEquals(
      Readers.readAs[T](input.getBytes(UTF_8)).toEither.left.map(_.format),
      Readers.readAs[T](input).toEither.left.map(_.format),
      s"byte and String decoding disagree for: $input"
    )

  test("ascii record round-trips like the String mode"):
    assertSameAsString[(id: Long, sku: String, qty: Int, price: Double, active: Boolean)](
      """(id = 42000, sku = "sku-42", qty = 3, price = 4200.99, active = true)"""
    )

  test("nested vectors of records"):
    assertSameAsString[(orders: Vector[(id: Int, note: String)])](
      """(orders = Vector((id = 1, note = "a"), (id = 2, note = "b")))"""
    )

  test("non-ascii string values decode through the JDK decoder"):
    assertSameAsString[(note: String)]("""(note = "héllo wörld ✓ 日本語")""")

  test("non-ascii identifiers decode identically"):
    assertSameAsString[(größe: Int)]("(größe = 5)")

  test("unicode digits keep their exact interpretation"):
    assertSameAsString[(x: Int)]("(x = ١٢٣)")

  test("errors match the String mode, including spans"):
    assertSameAsString[(x: Int)]("(x = \"oops\")")
    assertSameAsString[(x: Int)]("(x = )")
    assertSameAsString[(x: Int)]("(y = 1)")

  test("malformed literals surface as token errors on both paths"):
    assertSameAsString[(x: Int)]("(x = 0x)")

  test("batched byte decoding matches, reusing pooled buffers across sizes"):
    given BatchContext = BatchContext.local()
    val small          = """(x = 1)"""
    val big            = s"""(xs = Vector(${(1 to 3000).mkString(", ")}))"""
    assertEquals(
      Readers.batched.readAs[(x: Int)](small.getBytes(UTF_8)).toOption,
      Some((x = 1))
    )
    // over MaxPooledInputChars: transient buffer path
    assertEquals(
      Readers.batched.readAs[(xs: Vector[Int])](big.getBytes(UTF_8)).toOption.map(_.xs.sum),
      Some((1 to 3000).sum)
    )
    // and back to a small pooled decode after the oversized one
    assertEquals(
      Readers.batched.readAs[(x: Int)](small.getBytes(UTF_8)).toOption,
      Some((x = 1))
    )
