package scalanotation

import scala.annotation.publicInBinary

/** Field default values for `T`, consumed by [[Configured.withDefaultValues]]: a field omitted from
  * the input decodes to its default instead of failing. Evidence is gathered from `T`'s definition
  * by `Defaults.derived` in the macros module (case-class and enum-case constructor defaults), or
  * assembled manually.
  *
  * Values are evaluated once, when the evidence is created. Defaults that depend on other
  * constructor parameters cannot be gathered and are treated as absent.
  */
final class DefaultValues[T] @publicInBinary private[scalanotation] (
    private[scalanotation] val selfDefaults: Map[String, AnyRef],
    private[scalanotation] val caseDefaults: Map[String, Map[String, AnyRef]]
)

object DefaultValues:
  @publicInBinary
  private[scalanotation] def create[T](
      selfDefaults: Map[String, AnyRef],
      caseDefaults: Map[String, Map[String, AnyRef]]
  ): DefaultValues[T] =
    new DefaultValues[T](selfDefaults, caseDefaults)
