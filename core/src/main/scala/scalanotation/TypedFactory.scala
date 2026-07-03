package scalanotation

import scala.annotation.publicInBinary

/** Typeclass that finalizes values of `T` directly from the decoder's typed [[BuilderSlots]].
  * Evidence takes one of two shapes: [[TypedFactory.OfProduct]] for a product `T`, and
  * [[TypedFactory.OfSum]] for a sum `T`. Instances are derived by `TypedFactories.derived` in the
  * macros module.
  */
sealed trait TypedFactory[T]

object TypedFactory:
  /** Builds a product `T` directly from the typed slots: a derived factory pulls each constructor
    * argument from the matching typed slot, so primitive fields are never boxed at any point of the
    * decode.
    *
    * The typed field accessors are the write-side dual: they read a field straight off the product
    * value with a precise signature, so primitive fields are never boxed at any point of the
    * encode. The defaults box through the [[Product]] interface; derived factories override them
    * with direct field selections.
    */
  trait OfProduct[T] extends TypedFactory[T]:
    def fromSlots(slots: BuilderSlots): T

    def stringFieldValue(value: Any, index: Int): String =
      value.asInstanceOf[Product].productElement(index).asInstanceOf[String]
    def charFieldValue(value: Any, index: Int): Char =
      value.asInstanceOf[Product].productElement(index).asInstanceOf[Char]
    def intFieldValue(value: Any, index: Int): Int =
      value.asInstanceOf[Product].productElement(index).asInstanceOf[Int]
    def longFieldValue(value: Any, index: Int): Long =
      value.asInstanceOf[Product].productElement(index).asInstanceOf[Long]
    def floatFieldValue(value: Any, index: Int): Float =
      value.asInstanceOf[Product].productElement(index).asInstanceOf[Float]
    def doubleFieldValue(value: Any, index: Int): Double =
      value.asInstanceOf[Product].productElement(index).asInstanceOf[Double]
    def booleanFieldValue(value: Any, index: Int): Boolean =
      value.asInstanceOf[Product].productElement(index).asInstanceOf[Boolean]

  /** Factories for the structured product cases of a sum `T`, keyed by case label — the decoder
    * builds the matching case, never `T` itself. Nullary cases decode to a fixed value and need no
    * factory.
    */
  trait OfSum[T] extends TypedFactory[T]:
    def caseFactories: Map[String, OfProduct[?]]

  @publicInBinary
  private[scalanotation] def ofSum[T](factories: Map[String, OfProduct[?]]): OfSum[T] =
    new OfSum[T]:
      def caseFactories: Map[String, OfProduct[?]] = factories
