package scalanotation

import scala.annotation.publicInBinary
import scala.compiletime
import scala.deriving.Mirror

import scalanotation.schema.RawSchema

final class Configured[T] private (
    val discriminatorField: Option[String],
    val skippable: Boolean,
    private[scalanotation] val typedFactories: Configured.TypedFactories | Null
)

object Configured:
  /** typed factories derived for the product cases of `T`: [[selfFactory]] for a product `T`
    * itself, and per-case factories (keyed by case label) for a sum
    */
  private[scalanotation] final class TypedFactories(
      val selfFactory: TypedFactory.OfProduct[?] | Null,
      val caseFactories: Map[String, TypedFactory.OfProduct[?]]
  )

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

  /** Like [[default]], but every structured product case is built by the contextual
    * [[TypedFactory]] evidence, which pulls each constructor argument from the decoder's typed
    * slots, so primitive fields are never boxed at any point of the decode. Evidence is derived by
    * `TypedFactories.derived` in the macros module.
    */
  def typed[T](using TypedFactory[T]): Configured[T] =
    default[T].withTypedFactories

  extension [T](config: Configured[T])
    /** a copy of this configuration with the contextual [[TypedFactory]] evidence attached */
    def withTypedFactories(using factory: TypedFactory[T]): Configured[T] =
      factory match
        case product: TypedFactory.OfProduct[?] =>
          createTyped(config.discriminatorField, config.skippable, product, Map.empty)
        case sum: TypedFactory.OfSum[?] =>
          createTyped(config.discriminatorField, config.skippable, null, sum.caseFactories)

  @publicInBinary
  private[scalanotation] def create[T](
      discriminatorField: Option[String],
      skippable: Boolean
  ): Configured[T] =
    new Configured[T](discriminatorField, skippable, null)

  @publicInBinary
  private[scalanotation] def createTyped[T](
      discriminatorField: Option[String],
      skippable: Boolean,
      selfFactory: TypedFactory.OfProduct[?] | Null,
      caseFactories: Map[String, TypedFactory.OfProduct[?]]
  ): Configured[T] =
    new Configured[T](discriminatorField, skippable, TypedFactories(selfFactory, caseFactories))

  private[scalanotation] def applyToSchema[T](
      schema: RawSchema[T],
      config: Configured[?]
  ): RawSchema[T] =
    val baseSchema =
      if config.skippable then makeSkippable(schema)
      else schema
    val discriminated = config.discriminatorField match
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
    config.typedFactories match
      case null      => discriminated
      case factories => attachTypedFactories(discriminated, factories)

  private def attachTypedFactories[T](
      schema: RawSchema[T],
      factories: TypedFactories
  ): RawSchema[T] =
    schema match
      case RawSchema.NamedTuple(fields, read, write, allowSkipped) =>
        val factory = factories.selfFactory
        if factory == null || read == null then schema
        else
          RawSchema.NamedTuple(
            fields,
            RawSchema.NamedTupleRead.withSlotsFactory(read, factory),
            write,
            allowSkipped
          )
      case RawSchema.Sum(cases, write) =>
        RawSchema.Sum(cases.map(attachCaseFactory(factories, _)), write)
      case RawSchema.DiscriminatorSum(cases, write, discriminatorField) =>
        RawSchema.DiscriminatorSum(
          cases.map(attachCaseFactory(factories, _)),
          write,
          discriminatorField
        )
      case RawSchema.Mapped(base, mapping) =>
        RawSchema.Mapped(attachTypedFactories(base, factories), mapping)
      case other => other

  private def attachCaseFactory(
      factories: TypedFactories,
      sumCase: RawSchema.SumCase
  ): RawSchema.SumCase =
    factories.caseFactories.get(sumCase.name) match
      case Some(factory) => sumCase.copy(schema = attachFactory(sumCase.schema, factory))
      case None          => sumCase

  private def attachFactory[T](
      schema: RawSchema[T],
      factory: TypedFactory.OfProduct[?]
  ): RawSchema[T] =
    schema match
      case RawSchema.NamedTuple(fields, read, write, allowSkipped) =>
        if read == null then schema
        else
          RawSchema.NamedTuple(
            fields,
            RawSchema.NamedTupleRead.withSlotsFactory(read, factory),
            write,
            allowSkipped
          )
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        RawSchema.PartialNamedTuple(attachFactory(base, factory), alreadySeenField)
      case RawSchema.Mapped(base, mapping) =>
        RawSchema.Mapped(attachFactory(base, factory), mapping)
      case other => other

  private def makeSkippable[T](schema: RawSchema[T]): RawSchema[T] =
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
