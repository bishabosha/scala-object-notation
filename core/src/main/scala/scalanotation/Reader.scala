package scalanotation

import scalanotation.Reader.Builders.AtPath
import scalanotation.internal.PublicInternal
import scalanotation.internal.PublicInternal.showType
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.NamedTuple.NamedTuple
import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven

import Result.eval.{ok, break}

sealed trait Reader[T]:
  private[scalanotation] def schema: RawSchema

  final def map[U](f: T => U): Reader[U] =
    Reader.mapped(this)(f)

  final def emap[U](f: T => Result[U, DecodeError]): Reader[U] =
    Reader.mappedResult(this)(f)

object Reader {
  private[scalanotation] final def finalize[Z, T](
      decoder: Reader[T],
      checked: Result[Z, DecodeError]
  ): Result[T, DecodeError] =
    decoder match
      case mapped: MappedSchema[?, T] => mapped.parse(checked)
      case _                          => checked.asInstanceOf[Result[T, DecodeError]]

  private class MappedSchema[A, B](
      private val base: Reader[A],
      private val transform: A => Result[B, DecodeError]
  ) extends Reader[B] {
    val schema: RawSchema = base.schema

    final def parse[Z](result: Result[Z, DecodeError]): Result[B, DecodeError] =
      @tailrec
      def loop[X, Y](
          res: Result[X, DecodeError],
          base: Reader[Y],
          stack: List[Any => Result[Any, DecodeError]]
      ): Result[Y, DecodeError] =
        base match
          case mapped: MappedSchema[y1, Y] =>
            loop[X, y1](
              res,
              mapped.base,
              mapped.transform.asInstanceOf[Any => Result[Any, DecodeError]] :: stack
            ).asInstanceOf[Result[Y, DecodeError]]
          case _ =>
            stack
              .foldLeft(res.asInstanceOf[Result[Any, DecodeError]]) {
                _.flatMap(_)
              }
              .asInstanceOf[Result[Y, DecodeError]]
      loop(result, this, Nil)
  }

  private[scalanotation] def identity[T](schema0: RawSchema): Reader[T] =
    new Reader[T]:
      val schema: RawSchema = schema0

  def mapped[A, B](base: Reader[A])(transform: A => B): Reader[B] =
    mappedResult(base)(value => Result.Ok(transform(value)))

  def mappedResult[A, B](base: Reader[A])(
      transform: A => Result[B, DecodeError]
  ): Reader[B] = MappedSchema(base, transform)

  inline def derived[T](using mirror: Mirror.Of[T]): Reader[T] =
    inline mirror match
      case m: Mirror.ProductOf[T] =>
        compiletime.summonFrom({
          case _: (m.MirroredElemTypes =:= EmptyTuple) =>
            val label = compiletime.constValue[m.MirroredLabel]
            singleton[T](using m)(using ValueOf(label))
          case _ =>
            ofFields[T](using m)(
              using compiletime.summonInline[
                Builders.ProductFieldsAtPath["", m.MirroredElemLabels, m.MirroredElemTypes]
              ]
            )
        })
      case m: Mirror.SumOf[T] =>
        ofCases[T](using m)(
          using compiletime.summonInline[
            Builders.SumCasesAtPath["", T, m.MirroredElemLabels, m.MirroredElemTypes]
          ]
        )

  def ofFields[T](using mirror: Mirror.ProductOf[T])(
      using
      atPath: Builders.ProductFieldsAtPath["", mirror.MirroredElemLabels, mirror.MirroredElemTypes],
      hasFields: NotGiven[mirror.MirroredElemTypes =:= EmptyTuple]
  ): Reader[T] =
    identity(
      RawSchema.NamedTuple(
        IArray.from(atPath.fields),
        PublicInternal.caseClassBuilder[T]
      )
    )

  def singleton[T](using mirror: Mirror.ProductOf[T])(
      using label: ValueOf[mirror.MirroredLabel],
      noFields: mirror.MirroredElemTypes =:= EmptyTuple
  ): Reader[T] =
    val value = mirror.fromProduct(EmptyTuple)
    identity(
      RawSchema.NamedTuple(
        IArray(RawSchema.Field(label.value, forNull(value))),
        _ => value
      )
    )

  def forNull[T](value: T): Reader[T] =
    Builders.nullaryCase(value)

  def ofCases[T](using mirror: Mirror.SumOf[T])(
      using casesAtPath: Builders.SumCasesAtPath[
        "",
        T,
        mirror.MirroredElemLabels,
        mirror.MirroredElemTypes
      ]
  ): Reader[T] =
    var buf = Map.empty[String, RawSchema.SumCase[T]]
    for sumCase <- casesAtPath.cases do buf = buf.updated(sumCase.name, sumCase)
    identity(RawSchema.Sum(buf))

  given ExprSchema: Reader[Expr] = identity(RawSchema.AnyExpr)

  given StringSchema: Reader[String] = identity(RawSchema.String)

  given CharSchema: Reader[Char] = identity(RawSchema.Char)

  given IntSchema: Reader[Int] = identity(RawSchema.Int)

  given LongSchema: Reader[Long] = identity(RawSchema.Long)

  given FloatSchema: Reader[Float] = identity(RawSchema.Float)

  given DoubleSchema: Reader[Double] = identity(RawSchema.Double)

  given BooleanSchema: Reader[Boolean] = identity(RawSchema.Boolean)

  given OptionSchema: [T] => (atPath: AtPath["", Option[T]]) => Reader[Option[T]] =
    atPath.decoder

  given VectorSchema: [T] => (atPath: AtPath["", Vector[T]]) => Reader[Vector[T]] =
    atPath.decoder

  given IArraySchema: [T] => (atPath: AtPath["", IArray[T]]) => Reader[IArray[T]] =
    atPath.decoder

  given ArraySchema: [T] => (atPath: AtPath["", Array[T]]) => Reader[Array[T]] =
    atPath.decoder

  given SeqSchema: [Col[X] <: scala.collection.Seq[X], T] => (atPath: AtPath["", Col[T]])
    => Reader[Col[T]] =
    atPath.decoder

  given MapSchema
      : [Col[X, Y] <: scala.collection.Map[X, Y], T] => (atPath: AtPath["", Col[String, T]])
        => Reader[Col[String, T]] =
    atPath.decoder

  given NamedTupleSchema: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: AtPath["", NT]) => Reader[NT] =
    atPath.decoder

  object Builders {
    import AtPath.decoder
    import compiletime.{erasedValue, summonInline}
    import ProductFieldsAtPath.{Cons as ProductFieldsCons, Empty as ProductFieldsEmpty}
    import ProductFieldsAtPath.fields

    inline def formatPath[Path <: String]: String = ("'" + compiletime.constValue[Path] + "'")

    private[scalanotation] def nullaryCase[T](value: T): Reader[T] =
      identity(RawSchema.Nullary(value))

    opaque type ProductFieldsAtPath[Path <: String, Labels <: Tuple, Values <: Tuple] =
      List[RawSchema.Field[?]]

    object ProductFieldsAtPath:
      import compiletime.ops.string.+

      extension [Path <: String, Labels <: Tuple, Values <: Tuple](
          atPath: ProductFieldsAtPath[Path, Labels, Values]
      ) def fields: List[RawSchema.Field[?]] = atPath

      given Empty: [Path <: String] => ProductFieldsAtPath[Path, EmptyTuple, EmptyTuple] = Nil

      given Cons: [Path <: String, Label <: String, Value, Labels <: Tuple, Values <: Tuple]
        => (valueOf: ValueOf[Label])
        => (atPath: AtPath[Path + "." + Label, Value])
        => (rest: ProductFieldsAtPath[Path, Labels, Values])
        => ProductFieldsAtPath[Path, Label *: Labels, Value *: Values] =
        val decoder = atPath.decoder
        RawSchema.Field(valueOf.value, decoder) :: rest.fields

    opaque type SumCasesAtPath[Path <: String, A, Labels <: Tuple, Cases <: Tuple] =
      List[RawSchema.SumCase[A]]

    opaque type DerivedEnumCaseAtPath[Path <: String, T] = Reader[T]

    object DerivedEnumCaseAtPath:
      extension [Path <: String, T](self: DerivedEnumCaseAtPath[Path, T])
        private[scalanotation] def upcast[A >: T]: DerivedEnumCaseAtPath[Path, A] =
          self.asInstanceOf[DerivedEnumCaseAtPath[Path, A]]

      given Nullary[Path <: String, T](
          using mirror: Mirror.ProductOf[T],
          empty: mirror.MirroredElemTypes =:= EmptyTuple
      ): DerivedEnumCaseAtPath[Path, T] =
        nullaryCase(mirror.fromProduct(EmptyTuple))

      given Product[Path <: String, T](
          using mirror: Mirror.ProductOf[T],
          fieldsAtPath: ProductFieldsAtPath[
            Path,
            mirror.MirroredElemLabels,
            mirror.MirroredElemTypes
          ],
          nonEmpty: NotGiven[mirror.MirroredElemTypes =:= EmptyTuple]
      ): DerivedEnumCaseAtPath[Path, T] =
        identity(
          RawSchema.NamedTuple(
            IArray.from(fieldsAtPath.fields),
            PublicInternal.caseClassBuilder[T]
          )
        )

    object SumCasesAtPath:
      import compiletime.ops.string.+
      import DerivedEnumCaseAtPath.upcast

      extension [Path <: String, A, Labels <: Tuple, Cases <: Tuple](
          atPath: SumCasesAtPath[Path, A, Labels, Cases]
      ) def cases: List[RawSchema.SumCase[A]] = atPath

      given Empty: [Path <: String, A] => SumCasesAtPath[Path, A, EmptyTuple, EmptyTuple] = Nil

      given Cons: [Path <: String, A, Label <: String, Case <: A, Labels <: Tuple, Cases <: Tuple]
        => (valueOf: ValueOf[Label])
        => (caseDecoder: DerivedEnumCaseAtPath[Path + "." + Label, Case])
        => (rest: SumCasesAtPath[Path, A, Labels, Cases])
        => SumCasesAtPath[Path, A, Label *: Labels, Case *: Cases] =
        RawSchema.SumCase(valueOf.value, caseDecoder.upcast[A]) :: rest.cases

    opaque type NonNestedOption[Path <: String, T] = Unit

    object NonNestedOption:
      inline given Default: [Path <: String, T] => NonNestedOption[Path, T] =
        compiletime.summonFrom {
          case _: (T <:< Option[?]) =>
            compiletime.error(
              "at path " + formatPath[Path] +
                ": Reader[Option[Option[?]]] is not supported."
            )
          case _ =>
            ()
        }

    opaque type AtPath[Path <: String, T] = Reader[T] | List[RawSchema.Field[?]]

    object AtPath:
      import compiletime.ops.string.+

      extension [Path <: String, T](schema: AtPath[Path, T])
        def decoder: Reader[T] = schema match
          case d: Reader[T]                 => d
          case ls: List[RawSchema.Field[?]] =>
            identity(RawSchema.NamedTuple(IArray.from(ls), PublicInternal.buildNamedTuple))

        def schema: RawSchema = decoder.schema

      inline given DefaultAtPath: [Path <: String, T] => AtPath[Path, T] =
        compiletime.summonFrom {
          case d: Reader[T] => d
          case _            =>
            compiletime.error(
              "at path " + formatPath[Path] + ": Could not find Reader[" + showType[T] + "]."
            )
        }

      given VectorAtPath: [Path <: String, T]
        => (wrapped: AtPath[Path + "[]", T])
        => AtPath[Path, Vector[T]] =
        identity(RawSchema.Vector(wrapped.decoder, PublicInternal.BuildVector[T]))

      given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
        => (wrapped: AtPath[Path + "[]", T])
        => (factory: scala.collection.Factory[T, Col[T]])
        => AtPath[Path, Col[T]] =
        identity(RawSchema.Vector(wrapped.decoder, PublicInternal.SeqFactoryVector[T, Col]))

      given IArrayAtPath: [Path <: String, T: ClassTag as tag]
        => (wrapped: AtPath[Path + "[]", T])
        => AtPath[Path, IArray[T]] =
        identity(RawSchema.Vector(wrapped.decoder, PublicInternal.BuildIArray[T]))

      given ArrayAtPath: [Path <: String, T: ClassTag as tag]
        => (wrapped: AtPath[Path + "[]", T])
        => AtPath[Path, Array[T]] =
        identity(RawSchema.Vector(wrapped.decoder, PublicInternal.BuildArray[T]))

      given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
        => (wrapped: AtPath[Path + ".*", T])
        => (factory: scala.collection.Factory[(String, T), Col[String, T]])
        => AtPath[Path, Col[String, T]] =
        identity(RawSchema.Dict(wrapped.decoder, PublicInternal.MapFactoryDict[T, Col]))

      given OptionAtPath: [Path <: String, T]
        => NonNestedOption[Path, T]
        => (wrapped: AtPath[Path, T])
        => AtPath[Path, Option[T]] =
        identity(RawSchema.Option(wrapped.decoder))

      given NamedTupleCons: [Path <: String, N <: String, V, Ns <: Tuple, Vs <: Tuple]
        => (vn: ValueOf[N], ap: AtPath[Path + "." + N, V], rest: AtPath[Path, NamedTuple[Ns, Vs]])
        => AtPath[Path, NamedTuple[N *: Ns, V *: Vs]] =
        rest match
          case fs: List[RawSchema.Field[?]] =>
            val decoder: Reader[V] = ap.decoder
            RawSchema.Field(vn.value, decoder) :: fs
          case _ =>
            throw IllegalArgumentException(
              "Expected the rest of the named tuple to be a NamedTuple schema"
            )

      given NamedTupleEmpty: [Path <: String] => AtPath[Path, NamedTuple.Empty] =
        Nil
  }
}
