package scalanotation.internal

import scalanotation.internal.PublicInternal.showType

import scala.NamedTuple.NamedTuple
import scala.deriving.Mirror
import scala.util.NotGiven

private[scalanotation] trait CommonDerivationBuilders[TC[_]]:
  type FieldRepr
  type SumCaseRepr[A]

  private[scalanotation] inline def typeClassName: String

  private[scalanotation] def makeField[T](name: String, typeclass: TC[T]): FieldRepr

  private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): TC[T]

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
            "at path " + formatPath[Path] +
              ": " + typeClassName + "[Option[Option[?]]] is not supported."
          )
        case _ =>
          ()
      }

  opaque type AtPath[Path <: String, T] = TC[T] | List[FieldRepr]

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
            "at path " + formatPath[Path] + ": Could not find " + typeClassName + "[" + showType[
              T
            ] + "]."
          )
      }

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
