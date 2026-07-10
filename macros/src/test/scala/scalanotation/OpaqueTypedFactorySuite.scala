package scalanotation

import scalanotation.macros.TypedFactories
import steps.result.Result

/** Typed factories over fields whose types are opaque aliases. Derivation happens outside the
  * defining scope, where the alias neither dealiases nor compares equal to its underlying type, so
  * the factory classifies the field's slot via the underlying type instead
  * (`TypeRef.translucentSuperType`).
  */
class OpaqueTypedFactorySuite extends munit.FunSuite:
  import OpaqueTypedFactorySuite.*
  import OpaqueTypedFactorySuite.opaques.*

  /** encodes, then decodes through the plain (one-shot) API and through a pooled batch context,
    * which is the path that exercises the builder-slots factories; both must agree
    */
  private def assertRoundTrips[T: ReadWriter](value: T): Unit =
    val rendered = Writers.write(value)
    assertEquals(Readers.readAs[T](rendered), Result.Ok(value))
    given BatchContext = BatchContext.local()
    assertEquals(Readers.batched.readAs[T](rendered), Result.Ok(value))

  test("products with opaque primitive and string fields round-trip"):
    assertRoundTrips(Person(Id(42), "hi"))

  test("the factory pulls an opaque Int field from the int slot"):
    val factory = Configured.typed[Person].typedFactories.nn.selfFactory.nn
    val slots   = BuilderSlots().reset(2)
    slots.setInt(0, 7)
    slots.setString(1, "x")
    assertEquals[Any, Any](factory.fromSlots(slots), Person(Id(7), "x"))

  test("mapped, generic, and chained opaque fields round-trip"):
    // Code decodes from a string literal through a mapped codec, so its slot is filled boxed
    // via the plain ref add while the factory pulls it through the int slot's kind fallback
    assertRoundTrips(Tagged(Name("n"), Code(7), Tag[String](3), Outer(9)))

  test("the factory pulls a mapped opaque field from a ref slot through the kind fallback"):
    val factory = Configured.typed[Tagged].typedFactories.nn.selfFactory.nn
    val slots   = BuilderSlots().reset(4)
    slots.setString(0, "n")
    slots.setRef(1, 7)
    slots.setInt(2, 3)
    slots.setRef(3, 9)
    assertEquals[Any, Any](
      factory.fromSlots(slots),
      Tagged(Name("n"), Code(7), Tag[String](3), Outer(9))
    )

  test("value classes and opaques over value classes stay on the ref path"):
    assertRoundTrips(Measured(Meters(5), Depth(Meters(6))))
    // a value class erases to int in the constructor exactly like an opaque Int, but its
    // boxed runtime representation is a Meters instance — an int-slot pull would fail here
    val factory = Configured.typed[Measured].typedFactories.nn.selfFactory.nn
    val slots   = BuilderSlots().reset(2)
    slots.setRef(0, Meters(5))
    slots.setRef(1, Meters(6))
    assertEquals[Any, Any](factory.fromSlots(slots), Measured(Meters(5), Depth(Meters(6))))

object OpaqueTypedFactorySuite:
  /** the opaque aliases live in their own object so that everything below it — including the
    * derived products — sits outside their defining scope and cannot see through them
    */
  object opaques:
    opaque type Id = Int
    object Id:
      def apply(i: Int): Id = i
      given ReadWriter[Id]  = summon[ReadWriter[Int]]

    opaque type Name = String
    object Name:
      def apply(s: String): Name = s
      given ReadWriter[Name]     = summon[ReadWriter[String]]

    /** bounded opaque whose codec is mapped over a different base: it renders as a string literal,
      * so the decoder stores the mapped value boxed instead of using the int slot
      */
    opaque type Code <: Int = Int
    object Code:
      def apply(i: Int): Code = i
      given ReadWriter[Code]  =
        summon[ReadWriter[String]].bimap(s => Code(s.toInt))(c => (c: Int).toString)

    final class Meters(val v: Int) extends AnyVal
    object Meters:
      given ReadWriter[Meters] = summon[ReadWriter[Int]].bimap(Meters(_))(_.v)

    /** opaque over a value class: erases to int like the primitive opaques, but its boxed runtime
      * representation is a Meters instance, so it must take the ref slot
      */
    opaque type Depth = Meters
    object Depth:
      def apply(m: Meters): Depth = m
      given ReadWriter[Depth]     = summon[ReadWriter[Meters]]

    /** generic opaque (tagged-type pattern) */
    opaque type Tag[T] = Int
    object Tag:
      def apply[T](i: Int): Tag[T]    = i
      given [T] => ReadWriter[Tag[T]] = summon[ReadWriter[Int]]

    /** opaque alias of another opaque alias */
    opaque type Inner = Int
    opaque type Outer = Inner
    object Outer:
      def apply(i: Int): Outer = i
      given ReadWriter[Outer]  = summon[ReadWriter[Int]]

  import opaques.*

  case class Person(id: Id, str: String)
  object Person:
    given TypedFactory[Person] = TypedFactories.derived
    given Configured[Person]   = Configured.typed
    given ReadWriter[Person]   = ReadWriter.configured.derived

  case class Tagged(name: Name, code: Code, tag: Tag[String], outer: Outer)
  object Tagged:
    given TypedFactory[Tagged] = TypedFactories.derived
    given Configured[Tagged]   = Configured.typed
    given ReadWriter[Tagged]   = ReadWriter.configured.derived

  case class Measured(m: Meters, d: Depth)
  object Measured:
    given TypedFactory[Measured] = TypedFactories.derived
    given Configured[Measured]   = Configured.typed
    given ReadWriter[Measured]   = ReadWriter.configured.derived
