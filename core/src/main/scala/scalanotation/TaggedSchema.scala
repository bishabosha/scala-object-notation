package scalanotation

import scalanotation.Internal.showType
import scalanotation.TaggedSchema.Builders.AtPath
import steps.result.Result

import scala.NamedTuple.NamedTuple
import scala.deriving.Mirror
import scala.util.NotGiven

import Result.eval.{ok, break}
import scala.annotation.tailrec
import scala.reflect.ClassTag

sealed trait TaggedSchema[T]:
  private[scalanotation] def schema: Schema

  final def decode(expr: Expr): Result[T, DecodeError] =
    ExprDecoder.decodeTagged(this, expr)

  final def map[U](f: T => U): TaggedSchema[U] =
    TaggedSchema.mapped(this)(f)

  final def emap[U](f: T => Result[U, DecodeError]): TaggedSchema[U] =
    TaggedSchema.mappedResult(this)(f)

object TaggedSchema {
  private[scalanotation] final def finalize[Z, T](
      decoder: TaggedSchema[T],
      checked: Result[Z, DecodeError]
  ): Result[T, DecodeError] =
    decoder match
      case mapped: MappedSchema[?, T] => mapped.parse(checked)
      case _                          => checked.asInstanceOf[Result[T, DecodeError]]

  private class MappedSchema[A, B](
      private val base: TaggedSchema[A],
      private val transform: A => Result[B, DecodeError]
  ) extends TaggedSchema[B] {
    val schema: Schema = base.schema

    final def parse[Z](result: Result[Z, DecodeError]): Result[B, DecodeError] =
      @tailrec
      def loop[X, Y](
          res: Result[X, DecodeError],
          base: TaggedSchema[Y],
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

  private[scalanotation] def identity[T](schema0: Schema): TaggedSchema[T] =
    new TaggedSchema[T]:
      val schema: Schema = schema0

  def mapped[A, B](base: TaggedSchema[A])(transform: A => B): TaggedSchema[B] =
    mappedResult(base)(value => Result.Ok(transform(value)))

  def mappedResult[A, B](base: TaggedSchema[A])(
      transform: A => Result[B, DecodeError]
  ): TaggedSchema[B] = MappedSchema(base, transform)

  inline def derived[T](using mirror: Mirror.Of[T]): TaggedSchema[T] =
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
  ): TaggedSchema[T] =
    identity(
      Schema.NamedTuple(
        IArray.from(atPath.fields),
        Internal.caseClassBuilder[T]
      )
    )

  def singleton[T](using mirror: Mirror.ProductOf[T])(
      using label: ValueOf[mirror.MirroredLabel],
      noFields: mirror.MirroredElemTypes =:= EmptyTuple
  ): TaggedSchema[T] =
    val value = mirror.fromProduct(EmptyTuple)
    identity(
      Schema.NamedTuple(
        IArray(Schema.Field(label.value, forNull(value))),
        _ => value
      )
    )

  def forNull[T](value: T): TaggedSchema[T] =
    Builders.nullaryCase(value)

  def ofCases[T](using mirror: Mirror.SumOf[T])(
      using casesAtPath: Builders.SumCasesAtPath[
        "",
        T,
        mirror.MirroredElemLabels,
        mirror.MirroredElemTypes
      ]
  ): TaggedSchema[T] =
    var buf = Map.empty[String, Schema.SumCase[T]]
    for sumCase <- casesAtPath.cases do buf = buf.updated(sumCase.name, sumCase)
    identity(Schema.Sum(buf))

  given ExprSchema: TaggedSchema[Expr] = identity(Schema.AnyExpr)

  given StringSchema: TaggedSchema[String] = identity(Schema.String)

  given CharSchema: TaggedSchema[Char] = identity(Schema.Char)

  given IntSchema: TaggedSchema[Int] = identity(Schema.Int)

  given LongSchema: TaggedSchema[Long] = identity(Schema.Long)

  given FloatSchema: TaggedSchema[Float] = identity(Schema.Float)

  given DoubleSchema: TaggedSchema[Double] = identity(Schema.Double)

  given BooleanSchema: TaggedSchema[Boolean] = identity(Schema.Boolean)

  given OptionSchema: [T] => (atPath: AtPath["", Option[T]]) => TaggedSchema[Option[T]] =
    atPath.decoder

  given VectorSchema: [T] => (atPath: AtPath["", Vector[T]]) => TaggedSchema[Vector[T]] =
    atPath.decoder

  given IArraySchema: [T] => (atPath: AtPath["", IArray[T]]) => TaggedSchema[IArray[T]] =
    atPath.decoder

  given ArraySchema: [T] => (atPath: AtPath["", Array[T]]) => TaggedSchema[Array[T]] =
    atPath.decoder

  given SeqSchema: [Col[X] <: scala.collection.Seq[X], T] => (atPath: AtPath["", Col[T]])
    => TaggedSchema[Col[T]] =
    atPath.decoder

  given MapSchema
      : [Col[X, Y] <: scala.collection.Map[X, Y], T] => (atPath: AtPath["", Col[String, T]])
        => TaggedSchema[Col[String, T]] =
    atPath.decoder

  given NamedTupleSchema: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: AtPath["", NT]) => TaggedSchema[NT] =
    atPath.decoder

  object Builders {
    import AtPath.decoder
    import compiletime.{erasedValue, summonInline}
    import ProductFieldsAtPath.{Cons as ProductFieldsCons, Empty as ProductFieldsEmpty}
    import ProductFieldsAtPath.fields

    inline def formatPath[Path <: String]: String = ("'" + compiletime.constValue[Path] + "'")

    private[scalanotation] def nullaryCase[T](value: T): TaggedSchema[T] =
      identity(Schema.Nullary(value))

    opaque type ProductFieldsAtPath[Path <: String, Labels <: Tuple, Values <: Tuple] =
      List[Schema.Field[?]]

    object ProductFieldsAtPath:
      import compiletime.ops.string.+

      extension [Path <: String, Labels <: Tuple, Values <: Tuple](
          atPath: ProductFieldsAtPath[Path, Labels, Values]
      ) def fields: List[Schema.Field[?]] = atPath

      given Empty: [Path <: String] => ProductFieldsAtPath[Path, EmptyTuple, EmptyTuple] = Nil

      given Cons: [Path <: String, Label <: String, Value, Labels <: Tuple, Values <: Tuple]
        => (valueOf: ValueOf[Label])
        => (atPath: AtPath[Path + "." + Label, Value])
        => (rest: ProductFieldsAtPath[Path, Labels, Values])
        => ProductFieldsAtPath[Path, Label *: Labels, Value *: Values] =
        val decoder = atPath.decoder
        Schema.Field(valueOf.value, decoder) :: rest.fields

    opaque type SumCasesAtPath[Path <: String, A, Labels <: Tuple, Cases <: Tuple] =
      List[Schema.SumCase[A]]

    opaque type DerivedEnumCaseAtPath[Path <: String, T] = TaggedSchema[T]

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
          Schema.NamedTuple(
            IArray.from(fieldsAtPath.fields),
            Internal.caseClassBuilder[T]
          )
        )

    object SumCasesAtPath:
      import compiletime.ops.string.+
      import DerivedEnumCaseAtPath.upcast

      extension [Path <: String, A, Labels <: Tuple, Cases <: Tuple](
          atPath: SumCasesAtPath[Path, A, Labels, Cases]
      ) def cases: List[Schema.SumCase[A]] = atPath

      given Empty: [Path <: String, A] => SumCasesAtPath[Path, A, EmptyTuple, EmptyTuple] = Nil

      given Cons: [Path <: String, A, Label <: String, Case <: A, Labels <: Tuple, Cases <: Tuple]
        => (valueOf: ValueOf[Label])
        => (caseDecoder: DerivedEnumCaseAtPath[Path + "." + Label, Case])
        => (rest: SumCasesAtPath[Path, A, Labels, Cases])
        => SumCasesAtPath[Path, A, Label *: Labels, Case *: Cases] =
        Schema.SumCase(valueOf.value, caseDecoder.upcast[A]) :: rest.cases

    opaque type NonNestedOption[Path <: String, T] = Unit

    object NonNestedOption:
      inline given Default: [Path <: String, T] => NonNestedOption[Path, T] =
        compiletime.summonFrom {
          case _: (T <:< Option[?]) =>
            compiletime.error(
              "at path " + formatPath[Path] +
                ": TaggedSchema[Option[Option[?]]] is not supported."
            )
          case _ =>
            ()
        }

    opaque type AtPath[Path <: String, T] = TaggedSchema[T] | List[Schema.Field[?]]

    object AtPath:
      import compiletime.ops.string.+

      extension [Path <: String, T](schema: AtPath[Path, T])
        def decoder: TaggedSchema[T] = schema match
          case d: TaggedSchema[T]        => d
          case ls: List[Schema.Field[?]] =>
            identity(Schema.NamedTuple(IArray.from(ls), Internal.buildNamedTuple))

        def schema: Schema = decoder.schema

      inline given DefaultAtPath: [Path <: String, T] => AtPath[Path, T] =
        compiletime.summonFrom {
          case d: TaggedSchema[T] => d
          case _                  =>
            compiletime.error(
              "at path " + formatPath[Path] + ": Could not find TaggedSchema[" + showType[T] + "]."
            )
        }

      given VectorAtPath: [Path <: String, T]
        => (wrapped: AtPath[Path + "[]", T])
        => AtPath[Path, Vector[T]] =
        identity(Schema.Vector(wrapped.decoder, Internal.BuildVector[T]))

      given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
        => (wrapped: AtPath[Path + "[]", T])
        => (factory: scala.collection.Factory[T, Col[T]])
        => AtPath[Path, Col[T]] =
        identity(Schema.Vector(wrapped.decoder, Internal.SeqFactoryVector[T, Col]))

      given IArrayAtPath: [Path <: String, T: ClassTag as tag]
        => (wrapped: AtPath[Path + "[]", T])
        => AtPath[Path, IArray[T]] =
        identity(Schema.Vector(wrapped.decoder, Internal.BuildIArray[T]))

      given ArrayAtPath: [Path <: String, T: ClassTag as tag]
        => (wrapped: AtPath[Path + "[]", T])
        => AtPath[Path, Array[T]] =
        identity(Schema.Vector(wrapped.decoder, Internal.BuildArray[T]))

      given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
        => (wrapped: AtPath[Path + ".*", T])
        => (factory: scala.collection.Factory[(String, T), Col[String, T]])
        => AtPath[Path, Col[String, T]] =
        identity(Schema.Dict(wrapped.decoder, Internal.MapFactoryDict[T, Col]))

      given OptionAtPath: [Path <: String, T]
        => NonNestedOption[Path, T]
        => (wrapped: AtPath[Path, T])
        => AtPath[Path, Option[T]] =
        identity(Schema.Option(wrapped.decoder))

      given NamedTupleCons: [Path <: String, N <: String, V, Ns <: Tuple, Vs <: Tuple]
        => (vn: ValueOf[N], ap: AtPath[Path + "." + N, V], rest: AtPath[Path, NamedTuple[Ns, Vs]])
        => AtPath[Path, NamedTuple[N *: Ns, V *: Vs]] =
        rest match
          case fs: List[Schema.Field[?]] =>
            val decoder: TaggedSchema[V] = ap.decoder
            Schema.Field(vn.value, decoder) :: fs
          case _ =>
            throw IllegalArgumentException(
              "Expected the rest of the named tuple to be a NamedTuple schema"
            )

      given NamedTupleEmpty: [Path <: String] => AtPath[Path, NamedTuple.Empty] =
        Nil
  }
}
