package scalanotation

import scala.annotation.publicInBinary
import scala.compiletime
import scala.deriving.Mirror

import scalanotation.schema.RawSchema

final class Configured[T] private (
    val discriminatorField: Option[String],
    val skippable: Boolean,
    private[scalanotation] val typedFactories: Configured.TypedFactories | Null,
    private[scalanotation] val defaultValues: DefaultValues[?] | Null
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
      val typedFactories = factory match
        case product: TypedFactory.OfProduct[?] => TypedFactories(product, Map.empty)
        case sum: TypedFactory.OfSum[?]         => TypedFactories(null, sum.caseFactories)
      new Configured[T](
        config.discriminatorField,
        config.skippable,
        typedFactories,
        config.defaultValues
      )

    /** A copy of this configuration with the contextual [[DefaultValues]] evidence attached: fields
      * omitted from the input decode to their default values. A mode switch with skippable options
      * — a configuration is either defaults-filling or skippable, never both.
      */
    def withDefaultValues(using defaults: DefaultValues[T]): Configured[T] =
      require(
        !config.skippable,
        "default values and skippable options are mutually exclusive decode modes"
      )
      new Configured[T](
        config.discriminatorField,
        skippable = false,
        config.typedFactories,
        defaults
      )

  @publicInBinary
  private[scalanotation] def create[T](
      discriminatorField: Option[String],
      skippable: Boolean
  ): Configured[T] =
    new Configured[T](discriminatorField, skippable, null, null)

  @publicInBinary
  private[scalanotation] def createTyped[T](
      discriminatorField: Option[String],
      skippable: Boolean,
      selfFactory: TypedFactory.OfProduct[?] | Null,
      caseFactories: Map[String, TypedFactory.OfProduct[?]]
  ): Configured[T] =
    new Configured[T](
      discriminatorField,
      skippable,
      TypedFactories(selfFactory, caseFactories),
      null
    )

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
    val typed = config.typedFactories match
      case null      => discriminated
      case factories => attachTypedFactories(discriminated, factories)
    // defaults install LAST: every attachment above copies record nodes, and the installed
    // property must live on the node the reader keeps
    config.defaultValues match
      case null     => typed
      case defaults => installDefaults(typed, defaults)

  /** Installs the gathered default values onto fresh copies of the schema's record nodes — the
    * decode loop fills omitted fields from them. Structure mirrors [[makeSkippable]]; the two modes
    * are mutually exclusive (enforced by `withDefaultValues`).
    */
  private def installDefaults[T](
      schema: RawSchema[T],
      defaults: DefaultValues[?]
  ): RawSchema[T] =
    schema match
      case RawSchema.NamedTuple(fields, read, write, allowSkipped) =>
        installFieldDefaults(schema, defaults.selfDefaults)
      case RawSchema.Sum(cases, write) =>
        RawSchema.Sum(cases.map(installCaseDefaults(defaults, _)), write)
      case RawSchema.DiscriminatorSum(cases, write, discriminatorField) =>
        RawSchema.DiscriminatorSum(
          cases.map(installCaseDefaults(defaults, _)),
          write,
          discriminatorField
        )
      case RawSchema.Mapped(base, mapping) =>
        RawSchema.Mapped(installDefaults(base, defaults), mapping)
      case other => other

  private def installCaseDefaults(
      defaults: DefaultValues[?],
      sumCase: RawSchema.SumCase
  ): RawSchema.SumCase =
    defaults.caseDefaults.get(sumCase.name) match
      case Some(fieldDefaults) =>
        sumCase.copy(schema = installFieldDefaults(sumCase.schema, fieldDefaults))
      case None => sumCase

  private def installFieldDefaults[T](
      schema: RawSchema[T],
      fieldDefaults: Map[String, AnyRef]
  ): RawSchema[T] =
    schema match
      case RawSchema.NamedTuple(fields, read, write, allowSkipped) =>
        if fieldDefaults.isEmpty then schema
        else
          require(
            !allowSkipped,
            "default values and skippable options are mutually exclusive decode modes"
          )
          val defaultsByIndex: IArray[AnyRef | Null] =
            IArray.from(fields.map(field => fieldDefaults.getOrElse(field.name, null)))
          val copy = RawSchema.NamedTuple[T](fields, read, write, allowSkipped)
          copy.installFieldDefaults(defaultsByIndex)
          copy
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        RawSchema.PartialNamedTuple(installFieldDefaults(base, fieldDefaults), alreadySeenField)
      case RawSchema.Mapped(base, mapping) =>
        RawSchema.Mapped(installFieldDefaults(base, fieldDefaults), mapping)
      case other => other

  private def attachTypedFactories[T](
      schema: RawSchema[T],
      factories: TypedFactories
  ): RawSchema[T] =
    schema match
      case RawSchema.NamedTuple(fields, read, write, allowSkipped) =>
        val factory = factories.selfFactory
        if factory == null then schema
        else
          RawSchema.NamedTuple(
            fields,
            if read == null then read
            else RawSchema.NamedTupleRead.withSlotsFactory(read, factory),
            if write == null then write
            else RawSchema.NamedTupleWrite.withTypedFieldValues(write, factory),
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
        if read == null && write == null then schema
        else
          RawSchema.NamedTuple(
            fields,
            if read == null then read
            else RawSchema.NamedTupleRead.withSlotsFactory(read, factory),
            if write == null then write
            else RawSchema.NamedTupleWrite.withTypedFieldValues(write, factory),
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
