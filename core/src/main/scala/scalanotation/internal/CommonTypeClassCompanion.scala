package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.internal.PublicInternal.showType
import steps.result.Result

import scala.NamedTuple.NamedTuple
import scala.deriving.Mirror
import scala.util.NotGiven

private[scalanotation] trait CommonTypeClassCompanion[TC[_]]:
  private[scalanotation] def fromSchema[T](schema0: RawSchema): TC[T]

  private[scalanotation] def schemaOf[T](typeclass: TC[T]): RawSchema

  protected def primitiveTypeClass[T](schema: RawSchema): TC[T]

  protected def mappedStringTypeClass[T](
      read: String => Result[T, DecodeError],
      write: T => String
  ): TC[T] =
    fromSchema(
      RawSchema.mapResultAndInput(RawSchema.String)(
        resultMap0 =
          value => read(value.asInstanceOf[String]).asInstanceOf[Result[Any, DecodeError]],
        inputMap0 = value => write(value.asInstanceOf[T])
      )
    )

  protected final def pairSeqSchema(
      pairSchema: RawSchema,
      read: RawSchema.PairSeqRead | Null,
      write: RawSchema.PairSeqWrite | Null
  ): RawSchema =
    val (key, value) = pairSchemas(pairSchema)
    RawSchema.PairSeq(key, value, read, write)

  private def pairSchemas(pairSchema: RawSchema): (RawSchema, RawSchema) =
    pairSchema match
      case RawSchema.Tuple(slots, _, _) if slots.length == 2 =>
        slots(0) -> slots(1)
      case other =>
        throw IllegalArgumentException(
          s"Expected pair tuple schema for map entries, got ${other.describeSelf}"
        )

  trait CommonDerivationBuilders[
      RejectAllOptionalProducts <: Boolean,
      TypeClassName <: "Reader" | "Writer" | "ReadWriter"
  ]:
    type ThisBuilder <: this.type &
      CommonDerivationBuilders[
        RejectAllOptionalProducts,
        TypeClassName
      ]
    val thisBuilder: ThisBuilder
    type FieldRepr
    type SumCaseRepr[A]

    inline def derived[T](using mirror: Mirror.Of[T]): TC[T] =
      inline mirror match
        case m: Mirror.ProductOf[T] =>
          compiletime.summonFrom {
            case _: (m.MirroredElemTypes =:= EmptyTuple) =>
              val label = compiletime.constValue[m.MirroredLabel]
              singleton[T](using m)(using ValueOf(label))
            case _ =>
              ofFields[T](using m)(
                using compiletime.summonInline[
                  thisBuilder.ProductFieldsAtPath["", m.MirroredElemLabels, m.MirroredElemTypes]
                ]
              )
          }
        case m: Mirror.SumOf[T] =>
          ofCases[T](using m)(
            using compiletime.summonInline[
              thisBuilder.SumCasesAtPath["", T, m.MirroredElemLabels, m.MirroredElemTypes]
            ]
          )

    final def singleton[T](using mirror: Mirror.ProductOf[T])(
        using label: ValueOf[mirror.MirroredLabel],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): TC[T] = singletonTypeClass[T](label.value)

    final inline def ofFields[T](using mirror: Mirror.ProductOf[T])(
        using atPath: ProductFieldsAtPath["", mirror.MirroredElemLabels, mirror.MirroredElemTypes],
        hasFields: NotGiven[mirror.MirroredElemTypes =:= EmptyTuple]
    ): TC[T] =
      validateProductFields[RejectAllOptionalProducts, "", mirror.MirroredElemTypes]
      productTypeClass[T](ProductFieldsAtPath.fields(atPath))

    final def ofCases[T](using mirror: Mirror.SumOf[T])(
        using casesAtPath: SumCasesAtPath[
          "",
          T,
          mirror.MirroredElemLabels,
          mirror.MirroredElemTypes
        ]
    ): TC[T] = sumTypeClass[T](casesAtPath)

    private[scalanotation] def makeField[T](name: String, typeclass: TC[T]): FieldRepr

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): TC[T]

    private[scalanotation] def tupleTypeClass[T <: Tuple](slots: List[RawSchema]): TC[T] =
      fromSchema[T](RawSchema.Tuple(IArray.from(slots), read = null, write = null))

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): TC[T]

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): TC[T]

    private[scalanotation] def nullaryEnumCaseTypeClass[T](
        using mirror: Mirror.ProductOf[T],
        empty: mirror.MirroredElemTypes =:= EmptyTuple
    ): TC[T]

    private[scalanotation] def sumCaseTypeClass[A, T <: A](
        name: String,
        typeclass: TC[T]
    ): SumCaseRepr[A]

    private[scalanotation] def sumTypeClass[T](cases: List[SumCaseRepr[T]])(
        using mirror: Mirror.SumOf[T]
    ): TC[T]

    private inline def validateProductFields[
        RejectAllOptional <: Boolean,
        Path <: String,
        Values <: Tuple
    ]: Unit =
      inline compiletime.erasedValue[RejectAllOptional] match
        case _: true  => HasNonOptionalField.validate[Path, Values]
        case _: false => ()

    inline def formatPath[Path <: String]: String = ("'" + compiletime.constValue[Path] + "'")

    opaque type ProductFieldsAtPath[Path <: String, Labels <: Tuple, Values <: Tuple] =
      List[FieldRepr]

    object ProductFieldsAtPath:
      import compiletime.ops.string.+
      import AtPath.typeclass

      extension [Path <: String, Labels <: Tuple, Values <: Tuple](
          atPath: ProductFieldsAtPath[Path, Labels, Values]
      ) def fields: List[FieldRepr] = atPath

      given Empty: [Path <: String] => ProductFieldsAtPath[Path, EmptyTuple, EmptyTuple] = Nil

      given Cons: [Path <: String, Label <: String, Value, Labels <: Tuple, Values <: Tuple]
        => (valueOf: ValueOf[Label])
        => (atPath: AtPath[Path + "." + Label, Value])
        => (rest: ProductFieldsAtPath[Path, Labels, Values])
        => ProductFieldsAtPath[Path, Label *: Labels, Value *: Values] =
        makeField(valueOf.value, atPath.typeclass) :: rest.fields

    object HasNonOptionalField:
      inline def validate[Path <: String, Values <: Tuple]: Unit =
        inline compiletime.erasedValue[Values] match
          case _: EmptyTuple =>
            compiletime.error(
              "at path " + formatPath[Path] + ": " + compiletime.constValue[TypeClassName] +
                " derivation for a product with only Option fields is not supported."
            )
          case _: (Option[?] *: tail) =>
            validate[Path, tail]
          case _: (_ *: _) =>
            ()

    opaque type SumCasesAtPath[Path <: String, A, Labels <: Tuple, Cases <: Tuple] =
      List[SumCaseRepr[A]]

    opaque type DerivedEnumCaseAtPath[Path <: String, T] = TC[T]

    object DerivedEnumCaseAtPath:
      import ProductFieldsAtPath.fields

      extension [Path <: String, T](self: DerivedEnumCaseAtPath[Path, T])
        private[scalanotation] def typeclass: TC[T] = self

      given Nullary[Path <: String, T](
          using mirror: Mirror.ProductOf[T],
          empty: mirror.MirroredElemTypes =:= EmptyTuple
      ): DerivedEnumCaseAtPath[Path, T] =
        nullaryEnumCaseTypeClass[T]

      given Product[Path <: String, T](
          using mirror: Mirror.ProductOf[T],
          fieldsAtPath: ProductFieldsAtPath[
            Path,
            mirror.MirroredElemLabels,
            mirror.MirroredElemTypes
          ],
          nonEmpty: NotGiven[mirror.MirroredElemTypes =:= EmptyTuple]
      ): DerivedEnumCaseAtPath[Path, T] =
        productTypeClass[T](fieldsAtPath.fields)

    object SumCasesAtPath:
      import compiletime.ops.string.+
      import DerivedEnumCaseAtPath.typeclass

      extension [Path <: String, A, Labels <: Tuple, Cases <: Tuple](
          atPath: SumCasesAtPath[Path, A, Labels, Cases]
      ) def cases: List[SumCaseRepr[A]] = atPath

      given Empty: [Path <: String, A] => SumCasesAtPath[Path, A, EmptyTuple, EmptyTuple] = Nil

      given Cons: [Path <: String, A, Label <: String, T <: A, Labels <: Tuple, Cases <: Tuple]
        => (valueOf: ValueOf[Label])
        => (caseTypeClass: DerivedEnumCaseAtPath[Path + "." + Label, T])
        => (rest: SumCasesAtPath[Path, A, Labels, Cases])
        => SumCasesAtPath[Path, A, Label *: Labels, T *: Cases] =
        sumCaseTypeClass[A, T](valueOf.value, caseTypeClass.typeclass) :: rest.cases

    opaque type NonNestedOption[Path <: String, T] <: Unit = Unit

    object NonNestedOption:
      inline given [Path <: String, T]: NonNestedOption[Path, T] =
        compiletime.summonFrom {
          case _: (T <:< Option[?]) =>
            compiletime.error(
              "at path " + formatPath[Path] + ": " + compiletime.constValue[TypeClassName] +
                "[Option[Option[?]]] is not supported."
            )
          case _ =>
            ()
        }

    opaque type AtPath[Path <: String, T] = TC[T] | List[FieldRepr]

    opaque type TupleSlotsAtPath[Path <: String, Values <: Tuple] = List[RawSchema]

    private def tupleSlotSchemas[Path <: String, Values <: Tuple](
        slots: TupleSlotsAtPath[Path, Values]
    ): List[RawSchema] = slots

    private[scalanotation] final def liftAtPath[Path <: String, T](
        typeclass: TC[T]
    ): AtPath[Path, T] =
      typeclass

    object AtPath:
      import compiletime.ops.string.+

      extension [Path <: String, T](value: AtPath[Path, T])
        def typeclass: TC[T] = value match
          case fields: List[?] =>
            namedTupleTypeClass(fields.asInstanceOf[List[FieldRepr]])
          case _ =>
            value.asInstanceOf[TC[T]]

      inline given DefaultAtPath: [Path <: String, T] => AtPath[Path, T] =
        compiletime.summonFrom {
          case tc: TC[T] => liftAtPath[Path, T](tc)
          case _         =>
            compiletime.error(
              "at path " + formatPath[Path] + ": Could not find " +
                compiletime.constValue[TypeClassName] + "[" + showType[T] + "]."
            )
        }

      given TupleAtPath: [Path <: String, T <: Tuple]
        => (slots: TupleSlotsAtPath[Path, T])
        => AtPath[Path, T] =
        liftAtPath[Path, T](tupleTypeClass[T](tupleSlotSchemas(slots)))

      given NamedTupleCons: [Path <: String, N <: String, V, Ns <: Tuple, Vs <: Tuple]
        => (vn: ValueOf[N], ap: AtPath[Path + "." + N, V], rest: AtPath[Path, NamedTuple[Ns, Vs]])
        => AtPath[Path, NamedTuple[N *: Ns, V *: Vs]] =
        rest match
          case fs: List[?] =>
            makeField(vn.value, ap.typeclass) :: fs.asInstanceOf[List[FieldRepr]]
          case _ =>
            throw IllegalArgumentException(
              "Expected the rest of the named tuple to be a NamedTuple schema"
            )

      given NamedTupleEmpty: [Path <: String] => AtPath[Path, NamedTuple.Empty] =
        Nil

    object TupleSlotsAtPath:
      import compiletime.ops.string.+
      import AtPath.typeclass

      extension [Path <: String, Values <: Tuple](slots: TupleSlotsAtPath[Path, Values])
        def schemas: List[RawSchema] = slots

      opaque type Indexed[Path <: String, Index <: Int, Values <: Tuple] = List[RawSchema]

      object Indexed:
        extension [Path <: String, Index <: Int, Values <: Tuple](
            slots: Indexed[Path, Index, Values]
        ) def schemas: List[RawSchema] = slots

        given Empty: [Path <: String, Index <: Int] => Indexed[Path, Index, EmptyTuple] =
          Nil

        given Cons: [Path <: String, Index <: Int, Head, Tail <: Tuple]
          => (
              head: AtPath[
                Path + "[" + compiletime.ops.int.ToString[Index] + "]",
                Head
              ]
        )
          => (tail: Indexed[Path, compiletime.ops.int.+[Index, 1], Tail])
          => Indexed[Path, Index, Head *: Tail] =
          schemaOf(head.typeclass) :: tail.schemas

      given FromIndexed: [Path <: String, Values <: Tuple]
        => (indexed: Indexed[Path, 0, Values])
        => TupleSlotsAtPath[Path, Values] =
        indexed.schemas

  trait CommonBuilders[
      RejectAllOptionalProducts <: Boolean,
      TypeClassName <: "Reader" | "Writer" | "ReadWriter"
  ] extends CommonDerivationBuilders[RejectAllOptionalProducts, TypeClassName]:
    given OptionAtPath: [Path <: String, T]
      => NonNestedOption[Path, T]
      => (wrapped: AtPath[Path, T])
      => AtPath[Path, Option[T]] =
      liftAtPath[Path, Option[T]](
        fromSchema[Option[T]](
          RawSchema.Option(schemaOf(wrapped.typeclass))
        )
      )

  val Builders: CommonBuilders[false, ? <: "Reader" | "Writer" | "ReadWriter"]

  given ExprSchema: TC[Expr] =
    primitiveTypeClass(RawSchema.ExprRouterSchema)

  given StringSchema: TC[String] =
    primitiveTypeClass(RawSchema.String)

  private def invalidMappedStringValue(typeName: String, raw: String): DecodeError =
    DecodeError.Custom(s"Invalid $typeName '$raw'")

  given BigIntSchema: TC[BigInt] =
    mappedStringTypeClass(
      read = raw =>
        Result.catchException({ case _: NumberFormatException =>
          invalidMappedStringValue("BigInt", raw)
        }) {
          BigInt(raw)
        },
      write = _.toString
    )

  given BigDecimalSchema: TC[BigDecimal] =
    mappedStringTypeClass(
      read = raw =>
        Result.catchException({ case _: NumberFormatException =>
          invalidMappedStringValue("BigDecimal", raw)
        }) {
          BigDecimal(raw)
        },
      write = _.toString
    )

  given CharSchema: TC[Char] =
    primitiveTypeClass(RawSchema.Char)

  given IntSchema: TC[Int] =
    primitiveTypeClass(RawSchema.Int)

  given LongSchema: TC[Long] =
    primitiveTypeClass(RawSchema.Long)

  given FloatSchema: TC[Float] =
    primitiveTypeClass(RawSchema.Float)

  given DoubleSchema: TC[Double] =
    primitiveTypeClass(RawSchema.Double)

  given BooleanSchema: TC[Boolean] =
    primitiveTypeClass(RawSchema.Boolean)

  given NullSchema: TC[Null] =
    primitiveTypeClass(RawSchema.Null)

  given OptionSchema: [T] => (atPath: Builders.AtPath["", Option[T]]) => TC[Option[T]] =
    atPath.typeclass

  given VectorSchema: [T] => (atPath: Builders.AtPath["", Vector[T]]) => TC[Vector[T]] =
    atPath.typeclass

  given IArraySchema: [T] => (atPath: Builders.AtPath["", IArray[T]]) => TC[IArray[T]] =
    atPath.typeclass

  given ArraySchema: [T] => (atPath: Builders.AtPath["", Array[T]]) => TC[Array[T]] =
    atPath.typeclass

  given SeqSchema: [Col[X] <: scala.collection.Seq[X], T]
    => (atPath: Builders.AtPath["", Col[T]])
    => TC[Col[T]] =
    atPath.typeclass

  given MapSchema: [Col[X, Y] <: scala.collection.Map[X, Y], T]
    => (atPath: Builders.AtPath["", Col[String, T]])
    => TC[Col[String, T]] =
    atPath.typeclass

  given [Col[X, Y] <: scala.collection.Map[X, Y], K, V]
    => NotGiven[K =:= String]
    => (atPath: Builders.AtPath["", Col[K, V]])
    => TC[Col[K, V]] =
    atPath.typeclass

  given TupleSchema: [T <: Tuple]
    => (atPath: Builders.AtPath["", T])
    => TC[T] =
    atPath.typeclass

  given NamedTupleSchema: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: Builders.AtPath["", NT]) => TC[NT] =
    atPath.typeclass
