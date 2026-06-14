package scalanotation

import scalanotation.internal.CommonTypeClassCompanion
import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven

sealed trait ReadWriter[T]:
  private[scalanotation] def schema: RawSchema

  final def reader: Reader[T] =
    Reader.fromSchema(schema)

  final def writer: Writer[T] =
    Writer.fromSchema(schema)

  final def bimap[U](read: T => U)(write: U => T): ReadWriter[U] =
    ReadWriter.mapped(this)(read)(write)

  final def bimapResult[U](read: T => Result[U, DecodeError])(write: U => T): ReadWriter[U] =
    ReadWriter.mappedResult(this)(read)(write)

object ReadWriter extends CommonTypeClassCompanion[ReadWriter]:
  private final class Instance[T](val schema: RawSchema) extends ReadWriter[T]

  private[scalanotation] def fromSchema[T](schema0: RawSchema): ReadWriter[T] =
    new Instance(schema0)

  private[scalanotation] def schemaOf[T](typeclass: ReadWriter[T]): RawSchema =
    typeclass.schema

  protected def primitiveTypeClass[T](schema: RawSchema): ReadWriter[T] =
    fromSchema(schema)

  def mapped[A, B](base: ReadWriter[A])(read: A => B)(write: B => A): ReadWriter[B] =
    mappedResult(base)(value => Result.Ok(read(value)))(write)

  def mappedResult[A, B](base: ReadWriter[A])(
      read: A => Result[B, DecodeError]
  )(
      write: B => A
  ): ReadWriter[B] =
    fromSchema(
      RawSchema.mapResultAndInput(base.schema)(
        resultMap0 = value => read(value.asInstanceOf[A]).asInstanceOf[Result[Any, DecodeError]],
        inputMap0 = value => write(value.asInstanceOf[B])
      )
    )

  export Builders.{derived, ofFields, singleton, ofCases}

  def forNull[T](value: T): ReadWriter[T] =
    summon[ReadWriter[Null]].bimap(_ => value)(_ => null)

  object skippable extends ReadWriterBuilders[true]:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type

    protected def allowSkippedNullableFields: Boolean = true

  object configured:
    inline def derived[T](using mirror: Mirror.Of[T], config: Configured[T]): ReadWriter[T] =
      fromSchema(Configured.applyToSchema(ReadWriter.derived[T].schema, config))

  private[scalanotation] trait ReadWriterBuilders[RejectAllOptionalProducts <: Boolean]
      extends CommonBuilders[RejectAllOptionalProducts, "ReadWriter"]:
    type FieldRepr      = RawSchema.Field
    type SumCaseRepr[A] = RawSchema.SumCase

    import compiletime.ops.string.+

    protected def allowSkippedNullableFields: Boolean

    private[scalanotation] def sumTypeClass[T](cases: List[SumCaseRepr[T]])(
        using mirror: Mirror.SumOf[T]
    ): ReadWriter[T] =
      fromSchema[T](
        RawSchema.Sum(
          IArray.from(cases),
          RawSchema.SumWrite.from[T](mirror.ordinal)
        )
      )

    private[scalanotation] def makeField[T](name: String, typeclass: ReadWriter[T]): FieldRepr =
      RawSchema.Field(name, typeclass.schema)

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): ReadWriter[T] =
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray.from(fields),
          RawSchema.NamedTupleRead.from(
            PublicInternal.buildNamedTuple.asInstanceOf[Array[AnyRef] => T],
            PublicInternal.namedTupleSlotsFactory
          ),
          RawSchema.NamedTupleWrite.productLike
        )
      )

    override private[scalanotation] def tupleTypeClass[T <: Tuple](
        slots: List[RawSchema]
    ): ReadWriter[T] =
      fromSchema[T](
        RawSchema.Tuple(
          IArray.from(slots),
          RawSchema.TupleRead.FromReaderBuilder(PublicInternal.BuildTupleSlots[T]),
          RawSchema.TupleWrite.productLike
        )
      )

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): ReadWriter[T] =
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray.from(fields),
          RawSchema.NamedTupleRead.from(
            PublicInternal.caseClassBuilder[T],
            PublicInternal.caseClassSlotsFactory[T]
          ),
          RawSchema.NamedTupleWrite.productLike,
          allowSkippedNullableFields = allowSkippedNullableFields
        )
      )

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): ReadWriter[T] =
      val value = mirror.fromProduct(EmptyTuple)
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray(RawSchema.Field(label, forNull(value).schema)),
          RawSchema.NamedTupleRead.from(_ => value),
          RawSchema.NamedTupleWrite.singleton
        )
      )

    private[scalanotation] def nullaryEnumCaseTypeClass[T](
        using mirror: Mirror.ProductOf[T],
        empty: mirror.MirroredElemTypes =:= EmptyTuple
    ): ReadWriter[T] =
      forNull(mirror.fromProduct(EmptyTuple))

    private[scalanotation] def sumCaseTypeClass[A, T <: A](
        name: String,
        typeclass: ReadWriter[T]
    ): SumCaseRepr[A] =
      RawSchema.SumCase(name, typeclass.schema)

    given VectorAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Vector[T]] =
      liftAtPath[Path, Vector[T]](
        fromSchema[Vector[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            RawSchema.VectorRead.FromReaderBuilder(PublicInternal.BuildVector[T]),
            RawSchema.VectorWrite.from[Vector[T], T](_.length, _.iterator)
          )
        )
      )

    given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
      => (wrapped: AtPath[Path + "[]", T])
      => (factory: scala.collection.Factory[T, Col[T]])
      => AtPath[Path, Col[T]] =
      liftAtPath[Path, Col[T]](
        fromSchema[Col[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            RawSchema.VectorRead.FromReaderBuilder(PublicInternal.SeqFactoryVector[T, Col]),
            RawSchema.VectorWrite.from[Col[T], T](_.size, _.iterator)
          )
        )
      )

    given IArrayAtPath: [Path <: String, T: ClassTag as tag]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, IArray[T]] =
      liftAtPath[Path, IArray[T]](
        fromSchema[IArray[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            PublicInternal.iarrayVectorRead[T],
            RawSchema.VectorWrite.from[IArray[T], T](_.length, _.iterator)
          )
        )
      )

    given ArrayAtPath: [Path <: String, T: ClassTag as tag]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Array[T]] =
      liftAtPath[Path, Array[T]](
        fromSchema[Array[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            PublicInternal.arrayVectorRead[T],
            RawSchema.VectorWrite.from[Array[T], T](_.length, _.iterator)
          )
        )
      )

    given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
      => (wrapped: AtPath[Path + ".*", T])
      => (factory: scala.collection.Factory[(String, T), Col[String, T]])
      => AtPath[Path, Col[String, T]] =
      liftAtPath[Path, Col[String, T]](
        fromSchema[Col[String, T]](
          RawSchema.Dict(
            wrapped.typeclass.schema,
            RawSchema.DictRead.FromReaderBuilder(PublicInternal.MapFactoryDict[T, Col]),
            RawSchema.DictWrite.from[Col[String, T], T](_.size, _.iterator)
          )
        )
      )

  override object Builders extends ReadWriterBuilders[false]:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type

    protected def allowSkippedNullableFields: Boolean = false
