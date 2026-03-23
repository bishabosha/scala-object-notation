package scalanotation

import scalanotation.Reader.Builders.AtPath
import scalanotation.internal.CommonDerivationBuilders
import scalanotation.internal.PublicInternal
import scalanotation.internal.PublicInternal.showType
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.NamedTuple.NamedTuple
import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven

sealed trait Reader[T]:
  private[scalanotation] def schema: RawSchema

  final def map[U](f: T => U): Reader[U] =
    Reader.mapped(this)(f)

  final def emap[U](f: T => Result[U, DecodeError]): Reader[U] =
    Reader.mappedResult(this)(f)

object Reader {
  trait VectorBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, elem: Elem): Repr
    def finish(repr: Repr): A

  trait DictBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, key: String, elem: Elem): Repr
    def finish(repr: Repr): A

  final case class Field[A](name: String, decoder: Reader[A]):
    def schemaField: RawSchema.Field = RawSchema.Field(name, decoder.schema)

  final case class SumCase[A](name: String, decoder: Reader[A]):
    def schemaCase: RawSchema.SumCase = RawSchema.SumCase(name, decoder.schema)

  private[scalanotation] final class PrimitiveReader[T](val schema: RawSchema) extends Reader[T]

  private[scalanotation] final class NamedTupleReader[A](
      val fields: IArray[Field[?]],
      val build: Array[AnyRef] => A
  ) extends Reader[A]:
    lazy val schema: RawSchema =
      RawSchema.NamedTuple(IArray.from(fields.iterator.map(_.schemaField)))

  private[scalanotation] final class SumReader[A](
      val cases: Map[String, SumCase[? <: A]]
  ) extends Reader[A]:
    lazy val schema: RawSchema =
      RawSchema.Sum(cases.iterator.map((name, sumCase) => name -> sumCase.schemaCase).toMap)

  private[scalanotation] final class VectorReader[Elem, Repr, A](
      val element: Reader[Elem],
      val builder: VectorBuilder[Elem, Repr, A]
  ) extends Reader[A]:
    lazy val schema: RawSchema = RawSchema.Vector(element.schema)

  private[scalanotation] final class DictReader[Elem, Repr, A](
      val element: Reader[Elem],
      val builder: DictBuilder[Elem, Repr, A]
  ) extends Reader[A]:
    lazy val schema: RawSchema = RawSchema.Dict(element.schema)

  private[scalanotation] final class NullaryReader[A](val value: A) extends Reader[A]:
    val schema: RawSchema = RawSchema.Nullary

  private[scalanotation] final class OptionReader[A](val inner: Reader[A])
      extends Reader[Option[A]]:
    lazy val schema: RawSchema = RawSchema.Option(inner.schema)

  private[scalanotation] class MappedSchema[A, B](
      private[scalanotation] val base: Reader[A],
      private[scalanotation] val transform: A => Result[B, DecodeError]
  ) extends Reader[B]:
    val schema: RawSchema = base.schema

  private[scalanotation] def identity[T](schema0: RawSchema): Reader[T] =
    PrimitiveReader(schema0)

  def mapped[A, B](base: Reader[A])(transform: A => B): Reader[B] =
    mappedResult(base)(value => Result.Ok(transform(value)))

  def mappedResult[A, B](base: Reader[A])(
      transform: A => Result[B, DecodeError]
  ): Reader[B] = MappedSchema(base, transform)

  @tailrec
  private[scalanotation] def unwrap[T](
      reader: Reader[T],
      stack: List[Any => Result[Any, DecodeError]] = Nil
  ): (Reader[Any], List[Any => Result[Any, DecodeError]]) =
    reader match
      case mapped: MappedSchema[a, T] =>
        unwrap[a](
          mapped.base,
          mapped.transform.asInstanceOf[Any => Result[Any, DecodeError]] :: stack
        )
      case _ =>
        (reader.asInstanceOf[Reader[Any]], stack)

  private[scalanotation] def applyTransforms[T](
      result: Result[Any, DecodeError],
      stack: List[Any => Result[Any, DecodeError]]
  ): Result[T, DecodeError] =
    stack.foldLeft(result)(_.flatMap(_)).asInstanceOf[Result[T, DecodeError]]

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
    Builders.productTypeClass[T](atPath.fields)

  def singleton[T](using mirror: Mirror.ProductOf[T])(
      using label: ValueOf[mirror.MirroredLabel],
      noFields: mirror.MirroredElemTypes =:= EmptyTuple
  ): Reader[T] =
    Builders.singletonTypeClass[T](label.value)

  def forNull[T](value: T): Reader[T] =
    NullaryReader(value)

  def ofCases[T](using mirror: Mirror.SumOf[T])(
      using casesAtPath: Builders.SumCasesAtPath[
        "",
        T,
        mirror.MirroredElemLabels,
        mirror.MirroredElemTypes
      ]
  ): Reader[T] =
    var buf = Map.empty[String, SumCase[? <: T]]
    for sumCase <- casesAtPath.cases do buf = buf.updated(sumCase.name, sumCase)
    SumReader(buf)

  given ExprSchema: Reader[Expr] = identity(RawSchema.AnyExpr)

  given StringSchema: Reader[String] = identity(RawSchema.String)

  given CharSchema: Reader[Char] = identity(RawSchema.Char)

  given IntSchema: Reader[Int] = identity(RawSchema.Int)

  given LongSchema: Reader[Long] = identity(RawSchema.Long)

  given FloatSchema: Reader[Float] = identity(RawSchema.Float)

  given DoubleSchema: Reader[Double] = identity(RawSchema.Double)

  given BooleanSchema: Reader[Boolean] = identity(RawSchema.Boolean)

  given OptionSchema: [T] => (atPath: AtPath["", Option[T]]) => Reader[Option[T]] =
    atPath.typeclass

  given VectorSchema: [T] => (atPath: AtPath["", Vector[T]]) => Reader[Vector[T]] =
    atPath.typeclass

  given IArraySchema: [T] => (atPath: AtPath["", IArray[T]]) => Reader[IArray[T]] =
    atPath.typeclass

  given ArraySchema: [T] => (atPath: AtPath["", Array[T]]) => Reader[Array[T]] =
    atPath.typeclass

  given SeqSchema: [Col[X] <: scala.collection.Seq[X], T] => (atPath: AtPath["", Col[T]])
    => Reader[Col[T]] =
    atPath.typeclass

  given MapSchema
      : [Col[X, Y] <: scala.collection.Map[X, Y], T] => (atPath: AtPath["", Col[String, T]])
        => Reader[Col[String, T]] =
    atPath.typeclass

  given NamedTupleSchema: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: AtPath["", NT]) => Reader[NT] =
    atPath.typeclass

  object Builders extends CommonDerivationBuilders[Reader] {
    type FieldRepr      = Field[?]
    type SumCaseRepr[A] = SumCase[A]

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "Reader"

    private[scalanotation] def makeField[T](name: String, typeclass: Reader[T]): FieldRepr =
      Field(name, typeclass)

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): Reader[T] =
      NamedTupleReader(
        IArray.from(fields),
        PublicInternal.buildNamedTuple.asInstanceOf[Array[AnyRef] => T]
      )

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): Reader[T] =
      NamedTupleReader(
        IArray.from(fields),
        PublicInternal.caseClassBuilder[T]
      )

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): Reader[T] =
      val value = mirror.fromProduct(EmptyTuple)
      NamedTupleReader(
        IArray(Field(label, forNull(value))),
        _ => value
      )

    private[scalanotation] def nullaryEnumCaseTypeClass[T](
        using mirror: Mirror.ProductOf[T],
        empty: mirror.MirroredElemTypes =:= EmptyTuple
    ): Reader[T] =
      NullaryReader(mirror.fromProduct(EmptyTuple))

    private[scalanotation] def sumCaseTypeClass[A, T <: A](
        name: String,
        typeclass: Reader[T]
    ): SumCaseRepr[A] =
      SumCase(name, typeclass.asInstanceOf[Reader[A]])

    given VectorAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Vector[T]] =
      liftAtPath[Path, Vector[T]](
        Reader.VectorReader(wrapped.typeclass, PublicInternal.BuildVector[T])
      )

    given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
      => (wrapped: AtPath[Path + "[]", T])
      => (factory: scala.collection.Factory[T, Col[T]])
      => AtPath[Path, Col[T]] =
      liftAtPath[Path, Col[T]](
        Reader.VectorReader(wrapped.typeclass, PublicInternal.SeqFactoryVector[T, Col])
      )

    given IArrayAtPath: [Path <: String, T: ClassTag as tag]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, IArray[T]] =
      liftAtPath[Path, IArray[T]](
        Reader.VectorReader(wrapped.typeclass, PublicInternal.BuildIArray[T])
      )

    given ArrayAtPath: [Path <: String, T: ClassTag as tag]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Array[T]] =
      liftAtPath[Path, Array[T]](
        Reader.VectorReader(wrapped.typeclass, PublicInternal.BuildArray[T])
      )

    given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
      => (wrapped: AtPath[Path + ".*", T])
      => (factory: scala.collection.Factory[(String, T), Col[String, T]])
      => AtPath[Path, Col[String, T]] =
      liftAtPath[Path, Col[String, T]](
        Reader.DictReader(wrapped.typeclass, PublicInternal.MapFactoryDict[T, Col])
      )

    given OptionAtPath: [Path <: String, T]
      => NonNestedOption[Path, T]
      => (wrapped: AtPath[Path, T])
      => AtPath[Path, Option[T]] =
      liftAtPath[Path, Option[T]](
        Reader.OptionReader(wrapped.typeclass)
      )
  }
}
