package scalanotation.macros

import scalanotation.TypedFactory
import scalanotation.macros.internal.TypedFactoryMacros

import scala.deriving.Mirror

/** Macro derivation of [[scalanotation.TypedFactory]] evidence, as required by the typed-factory
  * methods of `Configured` ([[scalanotation.Configured.typed]] and `withTypedFactories`).
  *
  * Derivation is always explicit — bind the evidence at the use site:
  * {{{
  * given TypedFactory[Point] = TypedFactories.derived
  *
  * given Configured[Point] = Configured.typed
  * }}}
  */
object TypedFactories:
  /** Derives [[scalanotation.TypedFactory]] evidence for a mirrored type: every structured product
    * case is built by invoking its primary constructor with each argument pulled from the matching
    * typed slot, so primitive fields are never boxed at any point of the decode.
    */
  inline def derived[T](using m: Mirror.Of[T]): TypedFactory[T] =
    inline m match
      case p: Mirror.ProductOf[T] =>
        TypedFactoryMacros.productFactory[T](using p)
      case s: Mirror.SumOf[T] =>
        TypedFactory.ofSum[T](
          TypedFactoryMacros.caseFactories[s.MirroredElemLabels, s.MirroredElemTypes]
        )
