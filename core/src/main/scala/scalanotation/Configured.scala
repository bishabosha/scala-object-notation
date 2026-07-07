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
    * decode loop fills omitted fields from them. Macro-gathered defaults address the root record
    * (or each sum case); manual bindings walk their path through nested records, `Option`s and
    * `Vector`s first. Structure mirrors [[makeSkippable]]; the two modes are mutually exclusive
    * (enforced by `withDefaultValues` and re-checked at installation).
    */
  private def installDefaults[T](
      schema: RawSchema[T],
      defaults: DefaultValues[?]
  ): RawSchema[T] =
    val rootBindings =
      defaults.selfDefaults.view
        .map((name, value) => (List(DefaultValues.Segment.Field(name)), value))
        .toList ++ defaults.bindings
    schema match
      case RawSchema.Sum(cases, write) =>
        RawSchema.Sum(cases.map(installCaseDefaults(defaults, _)), write)
      case RawSchema.DiscriminatorSum(cases, write, discriminatorField) =>
        RawSchema.DiscriminatorSum(
          cases.map(installCaseDefaults(defaults, _)),
          write,
          discriminatorField
        )
      case RawSchema.Mapped(base, mapping)
          if base.isInstanceOf[RawSchema.Sum[?]]
            || base.isInstanceOf[RawSchema.DiscriminatorSum[?]] =>
        RawSchema.Mapped(installDefaults(base, defaults), mapping)
      case other => installBindingsAt(other, rootBindings)

  private def installCaseDefaults(
      defaults: DefaultValues[?],
      sumCase: RawSchema.SumCase
  ): RawSchema.SumCase =
    defaults.caseDefaults.get(sumCase.name) match
      case Some(fieldDefaults) =>
        val bindings =
          fieldDefaults.view
            .map((name, value) => (List(DefaultValues.Segment.Field(name)), value))
            .toList
        sumCase.copy(schema = installBindingsAt(sumCase.schema, bindings))
      case None => sumCase

  /** installs path bindings into `schema`, recursing along each binding's segments */
  private def installBindingsAt[T](
      schema: RawSchema[T],
      bindings: List[(List[DefaultValues.Segment], AnyRef)]
  ): RawSchema[T] =
    if bindings.isEmpty then schema
    else
      schema match
        case RawSchema.NamedTuple(fields, read, write, allowSkipped) =>
          require(
            !allowSkipped,
            "default values and skippable options are mutually exclusive decode modes"
          )
          bindings.foreach { (segments, _) =>
            segments match
              case DefaultValues.Segment.Field(name) :: _ =>
                require(
                  fields.exists(_.name == name),
                  s"no field named '$name' on ${schema.describeSelf} to install a default for"
                )
              case other :: _ =>
                throw IllegalArgumentException(
                  s"a default path selects '$other' but ${schema.describeSelf} is a record"
                )
              case Nil => ()
          }
          val newFields = fields.map { field =>
            val deeper = bindings.collect {
              case (DefaultValues.Segment.Field(field.name) :: (rest @ _ :: _), value) =>
                (rest, value)
            }
            if deeper.isEmpty then field
            else field.copy(schema = installBindingsAt(field.schema, deeper))
          }
          val localDefaults = bindings.collect {
            case (DefaultValues.Segment.Field(name) :: Nil, value) => name -> value
          }.toMap
          val copy = RawSchema.NamedTuple[T](newFields, read, write, allowSkipped)
          if localDefaults.nonEmpty then
            copy.installFieldDefaults(
              IArray.from(newFields.map(field => localDefaults.getOrElse(field.name, null)))
            )
          copy
        case option: RawSchema.Option[t] =>
          RawSchema
            .Option(
              installBindingsAt(
                option.inner,
                stepped(bindings, schema, DefaultValues.Segment.InOption)
              )
            )
            .asInstanceOf[RawSchema[T]]
        case vector: RawSchema.Vector[a, e] =>
          RawSchema
            .Vector[a, e](
              installBindingsAt(
                vector.element.asInstanceOf[RawSchema[e]],
                stepped(bindings, schema, DefaultValues.Segment.InVector)
              ),
              vector.read,
              vector.write
            )
            .asInstanceOf[RawSchema[T]]
        case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
          RawSchema.PartialNamedTuple(installBindingsAt(base, bindings), alreadySeenField)
        case RawSchema.Mapped(base, mapping) =>
          RawSchema.Mapped(installBindingsAt(base, bindings), mapping)
        case other =>
          throw IllegalArgumentException(
            s"a default path continues into ${other.describeSelf}, which has no fields to default"
          )

  /** consumes one expected step segment off every binding, rejecting mismatched paths */
  private def stepped(
      bindings: List[(List[DefaultValues.Segment], AnyRef)],
      at: RawSchema[?],
      expected: DefaultValues.Segment
  ): List[(List[DefaultValues.Segment], AnyRef)] =
    bindings.map { (segments, value) =>
      segments match
        case head :: rest if head == expected => (rest, value)
        case other                            =>
          throw IllegalArgumentException(
            s"a default path selects '${other.headOption}' but the schema at this step is ${at.describeSelf}"
          )
    }

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
