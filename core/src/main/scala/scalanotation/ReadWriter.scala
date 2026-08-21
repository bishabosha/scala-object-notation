package scalanotation

import scalanotation.internal.CommonTypeClassCompanion
import scalanotation.internal.PublicInternal
import steps.result.Result

import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven

import scalanotation.schema.RawSchema

sealed trait ReadWriter[T]:
  def schema: RawSchema[T]

  final def reader: Reader[T] =
    Reader.fromSchema(schema)

  final def writer: Writer[T] =
    Writer.fromSchema(schema)

  final def bimap[U](read: T => U)(write: U => T): ReadWriter[U] =
    ReadWriter.mapped(this)(read)(write)

  final def bimapResult[U](read: T => Result[U, DecodeError])(write: U => T): ReadWriter[U] =
    ReadWriter.mappedResult(this)(read)(write)

object ReadWriter extends CommonTypeClassCompanion[ReadWriter]:
  private final class Instance[T](val schema: RawSchema[T]) extends ReadWriter[T]

  private[scalanotation] def fromSchema[T](schema0: RawSchema[T]): ReadWriter[T] =
    new Instance(schema0)

  private[scalanotation] def schemaOf[T](typeclass: ReadWriter[T]): RawSchema[T] =
    typeclass.schema

  def mapped[A, B](base: ReadWriter[A])(read: A => B)(write: B => A): ReadWriter[B] =
    fromSchema(
      RawSchema.mapPureAndInput(base.schema)(
        resultMap0 = value => read(value.asInstanceOf[A]),
        inputMap0 = value => write(value.asInstanceOf[B])
      )
    )

  def mappedResult[A, B](base: ReadWriter[A])(
      read: A => Result[B, DecodeError]
  )(
      write: B => A
  ): ReadWriter[B] =
    fromSchema(
      RawSchema.mapResultAndInput(base.schema)(
        resultMap0 = value => read(value.asInstanceOf[A]),
        inputMap0 = value => write(value.asInstanceOf[B])
      )
    )

  export Builders.{derived, ofFields, singleton, ofCases}

  def int[A](read: Reader.IntMap[A])(write: Writer.IntMap[A]): ReadWriter[A] =
    fromSchema(
      RawSchema.mapIntTotalAndInput(RawSchema.Int)(
        resultMap0 = read,
        inputMap0 = write
      )
    )

  def long[A](read: Reader.LongMap[A])(write: Writer.LongMap[A]): ReadWriter[A] =
    fromSchema(
      RawSchema.mapLongTotalAndInput(RawSchema.Long)(
        resultMap0 = read,
        inputMap0 = write
      )
    )

  def float[A](read: Reader.FloatMap[A])(write: Writer.FloatMap[A]): ReadWriter[A] =
    fromSchema(
      RawSchema.mapFloatTotalAndInput(RawSchema.Float)(
        resultMap0 = read,
        inputMap0 = write
      )
    )

  def double[A](read: Reader.DoubleMap[A])(write: Writer.DoubleMap[A]): ReadWriter[A] =
    fromSchema(
      RawSchema.mapDoubleTotalAndInput(RawSchema.Double)(
        resultMap0 = read,
        inputMap0 = write
      )
    )

  // Compatibility overloads for inline bodies compiled against 0.4.0. New source calls select the
  // public Writer.*Map overloads above and retain the unboxed write path end to end;
  // @publicInBinary keeps old TASTy expansions linkable after these variants become source-hidden.

  @deprecated("bincompat only — pass a Writer.IntMap for an unboxed write", "0.4.4")
  @annotation.publicInBinary
  private[scalanotation] def int[A](read: Reader.IntMap[A])(write: A => Int): ReadWriter[A] =
    int(read)((write(_)): Writer.IntMap[A])

  @deprecated("bincompat only — pass a Writer.LongMap for an unboxed write", "0.4.4")
  @annotation.publicInBinary
  private[scalanotation] def long[A](read: Reader.LongMap[A])(write: A => Long): ReadWriter[A] =
    long(read)((write(_)): Writer.LongMap[A])

  @deprecated("bincompat only — pass a Writer.FloatMap for an unboxed write", "0.4.4")
  @annotation.publicInBinary
  private[scalanotation] def float[A](read: Reader.FloatMap[A])(
      write: A => Float
  ): ReadWriter[A] =
    float(read)((write(_)): Writer.FloatMap[A])

  @deprecated("bincompat only — pass a Writer.DoubleMap for an unboxed write", "0.4.4")
  @annotation.publicInBinary
  private[scalanotation] def double[A](read: Reader.DoubleMap[A])(
      write: A => Double
  ): ReadWriter[A] =
    double(read)((write(_)): Writer.DoubleMap[A])

  def forNull[T](value: T): ReadWriter[T] =
    summon[ReadWriter[Null]].bimap(_ => value)(_ => null)

  def tuple[A, Repr](
      slots: Iterable[ReadWriter[?]],
      builder: Reader.TupleBuilder[Repr, A],
      size: A => Int,
      elementValue: (A, Int) => Any
  ): ReadWriter[A] =
    fromSchema(
      RawSchema.Tuple(
        IArray.from(slots.iterator.map(_.schema)),
        builder,
        RawSchema.TupleWrite.from(size, elementValue)
      )
    )

  def vector[A, Elem, Repr](
      element: ReadWriter[Elem],
      builder: Reader.VectorBuilder[Elem, Repr, A],
      size: A => Int,
      iterator: A => Iterator[Elem]
  ): ReadWriter[A] =
    fromSchema(
      RawSchema.Vector(
        element.schema,
        builder,
        RawSchema.VectorWrite.from(size, iterator)
      )
    )

  def tupleOf[A, Elem, Repr](
      element: ReadWriter[Elem],
      builder: Reader.VectorBuilder[Elem, Repr, A],
      size: A => Int,
      iterator: A => Iterator[Elem]
  ): ReadWriter[A] =
    fromSchema(
      RawSchema.TupleOf(
        element.schema,
        builder,
        RawSchema.VectorWrite.from(size, iterator)
      )
    )

  def dict[A, Elem, Repr](
      element: ReadWriter[Elem],
      builder: Reader.DictBuilder[Elem, Repr, A],
      size: A => Int,
      iterator: A => Iterator[(String, Elem)]
  ): ReadWriter[A] =
    fromSchema(
      RawSchema.Dict(
        element.schema,
        builder,
        RawSchema.DictWrite.from(size, iterator)
      )
    )

  /** Recommended if you want to derive both a Reader and a Writer for a `Map[String, V]` type and
    * use pair sequence syntax. This is because the default `given ReadWriter[Map[String, V]]` (of
    * `RawSchema.Dict` shape) will always be prioritized.
    *
    * Using the result of this method, declare `given Reader[Map[String, V]] = rw.reader` and
    * `given Writer[Map[String, V]] = rw.writer`.
    */
  def pairSeqAsDict[Final <: scala.collection.Map[String, ?]]()[V: ReadWriter as element, Col[
      X,
      Y
  ] <: scala.collection.Map[X, Y]](using Col[String, V] <:< Final)(
      using factory: scala.collection.Factory[(String, V), Col[String, V]]
  ): (reader: Reader[Col[String, V]], writer: Writer[Col[String, V]]) =
    // TODO: if we can make ReadWriter extend Reader and Writer,
    // then the implicit priority problem goes away, and we can just return a ReadWriter here.
    // We could then eliminate this and recommend `pairSeqAsMap` as a migration.
    val schema: RawSchema[Col[String, V]] = RawSchema.PairSeq(
      RawSchema.String,
      element.schema,
      PublicInternal.MapFactoryPairSeq[String, V, Col],
      RawSchema.PairSeqWrite.from[Col[String, V], String, V](_.size, _.iterator)
    )
    (Reader.fromSchema(schema), Writer.fromSchema(schema))

  def pairSeq[A, Key, Elem, Repr](
      key: ReadWriter[Key],
      element: ReadWriter[Elem],
      builder: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      size: A => Int,
      iterator: A => Iterator[(Key, Elem)]
  ): ReadWriter[A] =
    fromSchema(
      RawSchema.PairSeq(
        key.schema,
        element.schema,
        builder,
        RawSchema.PairSeqWrite.from(size, iterator)
      )
    )

  def router[A](
      name: String,
      selfKind: String,
      numberMode: RouterSchema.NumberMode = RouterSchema.NumberMode.Bounded
  )(
      cases: ReadWriter[A] => Iterable[RouterSchema.Route[A]],
      write: RouterSchema.Write[A]
  ): ReadWriter[A] =
    RouterSchema.readWriter(name, selfKind, numberMode)(cases, write)

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
        slots: List[RawSchema[?]]
    ): ReadWriter[T] =
      fromSchema[T](
        RawSchema.Tuple(
          IArray.from(slots),
          PublicInternal.BuildTupleSlots[T],
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
            PublicInternal.BuildVector[T],
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
            PublicInternal.SeqFactoryVector[T, Col],
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
            PublicInternal.arrayVectorWrite(
              wrapped.typeclass.schema,
              RawSchema.VectorWrite.from[IArray[T], T](_.length, _.iterator)
            )
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
            PublicInternal.arrayVectorWrite(
              wrapped.typeclass.schema,
              RawSchema.VectorWrite.from[Array[T], T](_.length, _.iterator)
            )
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
            PublicInternal.MapFactoryDict[T, Col],
            RawSchema.DictWrite.from[Col[String, T], T](_.size, _.iterator)
          )
        )
      )

    given [Path <: String, K, V, Col[X, Y] <: scala.collection.Map[X, Y]]
      => NotGiven[K =:= String]
      => (entry: AtPath[Path + "[]", (K, V)])
      => (factory: scala.collection.Factory[(K, V), Col[K, V]])
      => AtPath[Path, Col[K, V]] =
      liftAtPath[Path, Col[K, V]](
        fromSchema[Col[K, V]](
          pairSeqSchema(
            entry.typeclass.schema,
            PublicInternal.MapFactoryPairSeq[K, V, Col],
            RawSchema.PairSeqWrite.from[Col[K, V], K, V](_.size, _.iterator)
          )
        )
      )

  override object Builders extends ReadWriterBuilders[false]:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type

    protected def allowSkippedNullableFields: Boolean = false
