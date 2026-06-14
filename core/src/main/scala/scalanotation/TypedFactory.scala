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
    */
  trait OfProduct[T] extends TypedFactory[T]:
    def fromSlots(slots: BuilderSlots): T

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
