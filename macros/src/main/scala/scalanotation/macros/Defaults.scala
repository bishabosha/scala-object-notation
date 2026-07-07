package scalanotation.macros

import scalanotation.DefaultValues
import scalanotation.macros.internal.DefaultValueMacros

import scala.deriving.Mirror

/** Macro derivation of [[scalanotation.DefaultValues]] evidence, as consumed by
  * [[scalanotation.Configured.withDefaultValues]].
  *
  * Derivation is always explicit — bind the evidence at the use site:
  * {{{
  * case class Server(host: String, port: Int = 8080)
  *
  * given DefaultValues[Server] = Defaults.derived
  * given Configured[Server]    = Configured.default.withDefaultValues
  * }}}
  */
object Defaults:
  /** Derives [[scalanotation.DefaultValues]] for a mirrored type: the parameterless constructor
    * default values of a case class, or of every structured case of an enum. Values are evaluated
    * once, here.
    */
  inline def derived[T](using m: Mirror.Of[T]): DefaultValues[T] =
    inline m match
      case p: Mirror.ProductOf[T] =>
        DefaultValues.create[T](DefaultValueMacros.productDefaults[T], Map.empty)
      case s: Mirror.SumOf[T] =>
        DefaultValues.create[T](
          Map.empty,
          DefaultValueMacros.caseDefaults[s.MirroredElemLabels, s.MirroredElemTypes]
        )
