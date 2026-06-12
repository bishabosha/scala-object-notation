package scalanotation

import scalanotation.internal.CommonTypeClassCompanion
import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven

sealed trait Reader[T]:
  private[scalanotation] def schema: RawSchema

  final def map[U](f: T => U): Reader[U] =
    Reader.mapped(this)(f)

  final def mapResult[U](f: T => Result[U, DecodeError]): Reader[U] =
    Reader.mappedResult(this)(f)

private[scalanotation] trait ReaderLowPriority:
  given [T](using readWriter: ReadWriter[T]): Reader[T] =
    readWriter.reader

object Reader extends ReaderLowPriority, CommonTypeClassCompanion[Reader]:
  trait VectorBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, elem: Elem): Repr
    def finish(repr: Repr): A

    // typed adds, called by the decoder when the element decoded into a typed slot: each consumes
    // the state and returns the new state. The defaults box and delegate to `add`; builders over
    // primitive storage override the matching one to append without boxing.
    def addString(repr: Repr, elem: String): Repr   = add(repr, elem.asInstanceOf[Elem])
    def addChar(repr: Repr, elem: Char): Repr       = add(repr, elem.asInstanceOf[Elem])
    def addInt(repr: Repr, elem: Int): Repr         = add(repr, elem.asInstanceOf[Elem])
    def addLong(repr: Repr, elem: Long): Repr       = add(repr, elem.asInstanceOf[Elem])
    def addFloat(repr: Repr, elem: Float): Repr     = add(repr, elem.asInstanceOf[Elem])
    def addDouble(repr: Repr, elem: Double): Repr   = add(repr, elem.asInstanceOf[Elem])
    def addBoolean(repr: Repr, elem: Boolean): Repr = add(repr, elem.asInstanceOf[Elem])

  trait TupleBuilder[Repr, A]:
    def init(size: Int): Repr
    def initPooled(size: Int, pooled: BuilderSlots | Null): Repr =
      // overriden by pooled builders
      init(size)
    def add(repr: Repr, index: Int, elem: Any): Repr
    def finish(repr: Repr): A

    /** Optional low-boxing finalizer: when non-null, a pooling decoder fills its typed
      * [[BuilderSlots]] instead of threading `Repr`, and finalizes via [[TypedFactory.fromSlots]] —
      * e.g. `Tuple.fromProduct` directly over the slots.
      */
    def slotsFactory: TypedFactory | Null = null

    // typed adds: the defaults box and delegate to `add`
    def addString(repr: Repr, index: Int, elem: String): Repr   = add(repr, index, elem)
    def addChar(repr: Repr, index: Int, elem: Char): Repr       = add(repr, index, elem)
    def addInt(repr: Repr, index: Int, elem: Int): Repr         = add(repr, index, elem)
    def addLong(repr: Repr, index: Int, elem: Long): Repr       = add(repr, index, elem)
    def addFloat(repr: Repr, index: Int, elem: Float): Repr     = add(repr, index, elem)
    def addDouble(repr: Repr, index: Int, elem: Double): Repr   = add(repr, index, elem)
    def addBoolean(repr: Repr, index: Int, elem: Boolean): Repr = add(repr, index, elem)

  trait DictBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, key: String, elem: Elem): Repr
    def finish(repr: Repr): A

    // typed adds: the defaults box and delegate to `add`
    def addString(repr: Repr, key: String, elem: String): Repr =
      add(repr, key, elem.asInstanceOf[Elem])
    def addChar(repr: Repr, key: String, elem: Char): Repr =
      add(repr, key, elem.asInstanceOf[Elem])
    def addInt(repr: Repr, key: String, elem: Int): Repr =
      add(repr, key, elem.asInstanceOf[Elem])
    def addLong(repr: Repr, key: String, elem: Long): Repr =
      add(repr, key, elem.asInstanceOf[Elem])
    def addFloat(repr: Repr, key: String, elem: Float): Repr =
      add(repr, key, elem.asInstanceOf[Elem])
    def addDouble(repr: Repr, key: String, elem: Double): Repr =
      add(repr, key, elem.asInstanceOf[Elem])
    def addBoolean(repr: Repr, key: String, elem: Boolean): Repr =
      add(repr, key, elem.asInstanceOf[Elem])

  private final class Instance[T](val schema: RawSchema) extends Reader[T]

  private[scalanotation] def fromSchema[T](schema0: RawSchema): Reader[T] =
    new Instance(schema0)

  private[scalanotation] def schemaOf[T](typeclass: Reader[T]): RawSchema =
    typeclass.schema

  protected def primitiveTypeClass[T](schema: RawSchema): Reader[T] =
    fromSchema(schema)

  def mapped[A, B](base: Reader[A])(transform: A => B): Reader[B] =
    mappedResult(base)(value => Result.Ok(transform(value)))

  def mappedResult[A, B](base: Reader[A])(
      transform: A => Result[B, DecodeError]
  ): Reader[B] =
    fromSchema(
      RawSchema.mapResult(base.schema)(value =>
        transform(value.asInstanceOf[A]).asInstanceOf[Result[Any, DecodeError]]
      )
    )

  export Builders.{derived, ofFields, singleton, ofCases}

  def forNull[T](value: T): Reader[T] =
    summon[Reader[Null]].map(_ => value)

  object skippable extends ReaderBuilders[true]:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type

    protected def allowSkippedNullableFields: Boolean = true

  object configured:
    inline def derived[T](using mirror: Mirror.Of[T], config: Configured[T]): Reader[T] =
      fromSchema(Configured.applyToSchema(Reader.derived[T].schema, config))

  private[scalanotation] trait ReaderBuilders[RejectAllOptionalProducts <: Boolean]
      extends CommonBuilders[RejectAllOptionalProducts, "Reader"]:
    type FieldRepr      = RawSchema.Field
    type SumCaseRepr[A] = RawSchema.SumCase

    import compiletime.ops.string.+

    protected def allowSkippedNullableFields: Boolean

    private[scalanotation] def sumTypeClass[T](cases: List[SumCaseRepr[T]])(
        using mirror: Mirror.SumOf[T]
    ): Reader[T] =
      fromSchema[T](
        RawSchema.Sum(IArray.from(cases), write = null)
      )

    private[scalanotation] def makeField[T](name: String, typeclass: Reader[T]): FieldRepr =
      RawSchema.Field(name, typeclass.schema)

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): Reader[T] =
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray.from(fields),
          RawSchema.NamedTupleRead.from(
            PublicInternal.buildNamedTuple.asInstanceOf[Array[AnyRef] => T],
            PublicInternal.namedTupleSlotsFactory
          ),
          write = null
        )
      )

    override private[scalanotation] def tupleTypeClass[T <: Tuple](
        slots: List[RawSchema]
    ): Reader[T] =
      fromSchema[T](
        RawSchema.Tuple(
          IArray.from(slots),
          RawSchema.TupleRead.FromReaderBuilder(PublicInternal.BuildTupleSlots[T]),
          write = null
        )
      )

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): Reader[T] =
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray.from(fields),
          RawSchema.NamedTupleRead.from(
            PublicInternal.caseClassBuilder[T],
            PublicInternal.caseClassSlotsFactory[T]
          ),
          write = null,
          allowSkippedNullableFields = allowSkippedNullableFields
        )
      )

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): Reader[T] =
      val value = mirror.fromProduct(EmptyTuple)
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray(RawSchema.Field(label, forNull(value).schema)),
          RawSchema.NamedTupleRead.from(_ => value),
          write = null
        )
      )

    private[scalanotation] def nullaryEnumCaseTypeClass[T](
        using mirror: Mirror.ProductOf[T],
        empty: mirror.MirroredElemTypes =:= EmptyTuple
    ): Reader[T] =
      forNull(mirror.fromProduct(EmptyTuple))

    private[scalanotation] def sumCaseTypeClass[A, T <: A](
        name: String,
        typeclass: Reader[T]
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
            write = null
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
            write = null
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
            write = null
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
            write = null
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
            write = null
          )
        )
      )

  override object Builders extends ReaderBuilders[false]:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type

    protected def allowSkippedNullableFields: Boolean = false
