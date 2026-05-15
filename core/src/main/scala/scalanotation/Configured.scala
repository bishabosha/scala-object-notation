package scalanotation

import scalanotation.internal.RawSchema

import scala.annotation.publicInBinary
import scala.compiletime
import scala.deriving.Mirror

final class Configured[T] private (
    val discriminatorField: Option[String],
    val skippable: Boolean
)

object Configured:
  def default[T]: Configured[T] =
    create(None, skippable = false)

  inline def skippable[T](using mirror: Mirror.Of[T]): Configured[T] =
    validateSkippable[T]
    create(None, skippable = true)

  def discriminator[T](
      field: String,
      skippable: Boolean = false
  )(using Mirror.SumOf[T]): Configured[T] =
    create(Some(field), skippable)

  @publicInBinary
  private[scalanotation] def create[T](
      discriminatorField: Option[String],
      skippable: Boolean
  ): Configured[T] =
    new Configured[T](discriminatorField, skippable)

  private[scalanotation] def applyToSchema(schema: RawSchema, config: Configured[?]): RawSchema =
    val baseSchema =
      if config.skippable then makeSkippable(schema)
      else schema
    config.discriminatorField match
      case Some(discriminatorField) =>
        baseSchema match
          case RawSchema.Sum(cases, write) =>
            RawSchema.DiscriminatorSum(
              cases.map(sumCase =>
                sumCase.copy(
                  schema = RawSchema.PartialNamedTuple(sumCase.schema, discriminatorField)
                )
              ),
              write,
              discriminatorField
            )
          case other => other
      case None => baseSchema

  private def makeSkippable(schema: RawSchema): RawSchema =
    schema match
      case RawSchema.NamedTuple(fields, read, write, _) =>
        RawSchema.NamedTuple(
          fields,
          read,
          write,
          allowSkippedNullableFields = true
        )
      case RawSchema.Sum(cases, write) =>
        RawSchema.Sum(
          cases.map(sumCase => sumCase.copy(schema = makeSkippable(sumCase.schema))),
          write
        )
      case RawSchema.DiscriminatorSum(cases, write, discriminatorField) =>
        RawSchema.DiscriminatorSum(
          cases.map(sumCase => sumCase.copy(schema = makeSkippable(sumCase.schema))),
          write,
          discriminatorField
        )
      case RawSchema.Mapped(base, mapping) =>
        RawSchema.Mapped(makeSkippable(base), mapping)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        RawSchema.PartialNamedTuple(makeSkippable(base), alreadySeenField)
      case other => other

  private inline def validateSkippable[T](using mirror: Mirror.Of[T]): Unit =
    inline mirror match
      case m: Mirror.ProductOf[T] =>
        validateHasNonOptionalField[m.MirroredElemTypes]
      case _ => ()

  private inline def validateHasNonOptionalField[Values <: Tuple]: Unit =
    inline compiletime.erasedValue[Values] match
      case _: EmptyTuple =>
        compiletime.error(
          "Configured skippable derivation for a product with only Option fields is not supported."
        )
      case _: (Option[?] *: tail) =>
        validateHasNonOptionalField[tail]
      case _: (_ *: _) =>
        ()
