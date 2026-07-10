package scalanotation

/** Pins that the typed field accessor dispatches an opaque field through a direct field select
  * rather than the boxed `productElement` fallback. Lives in `jvm-native` because the pin observes
  * a failed cast, which throws a catchable [[ClassCastException]] only where casts are checked — on
  * Scala.js a failed `asInstanceOf` is undefined behaviour.
  */
class OpaqueTypedFactoryDispatchSuite extends munit.FunSuite:
  import OpaqueTypedFactorySuite.*
  import OpaqueTypedFactorySuite.opaques.*

  test("the typed accessor dispatches an opaque field through a direct field select"):
    val factory = Configured.typed[Person].typedFactories.nn.selfFactory.nn
    assertEquals(factory.intFieldValue(Person(Id(5), "x"), 0), 5)
    // the typed arm casts to Person before selecting; the boxed productElement fallback
    // would have returned 99 for any product
    intercept[ClassCastException]:
      factory.intFieldValue(Tuple1(99), 0)
