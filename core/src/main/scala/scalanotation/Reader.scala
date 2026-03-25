package scalanotation

import scalanotation.internal.CommonTypeClassCompanion
import scalanotation.internal.CompiledSchema
import scalanotation.internal.OpaqueSupport
import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven

sealed trait Reader[T]:
  private[scalanotation] def compiled: CompiledSchema

  private[scalanotation] final def schema: RawSchema = compiled.rawSchema

  final def map[U](f: T => U): Reader[U] =
    Reader.mapped(this)(f)

  final def emap[U](f: T => Result[U, DecodeError]): Reader[U] =
    Reader.mappedResult(this)(f)

private[scalanotation] trait ReaderLowPriority:
  given [T](using readWriter: ReadWriter[T]): Reader[T] =
    readWriter.reader

object Reader extends ReaderLowPriority, CommonTypeClassCompanion[Reader]:
  trait VectorBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, elem: Elem): Repr
    def finish(repr: Repr): A

  trait DictBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, key: String, elem: Elem): Repr
    def finish(repr: Repr): A

  private final class Instance[T](val compiled: CompiledSchema) extends Reader[T]

  private[scalanotation] def fromCompiled[T](compiled0: CompiledSchema): Reader[T] =
    new Instance(compiled0)

  private[scalanotation] def compiledOf[T](typeclass: Reader[T]): CompiledSchema =
    typeclass.compiled

  private def opaque[T](
      support: OpaqueSupport.Read[T]
  ): Reader[T] =
    fromCompiled[T](OpaqueSupport.compiled(support))

  protected def primitiveTypeClass[T](codec: OpaqueSupport.ReadWrite[T]): Reader[T] =
    opaque(codec.read)

  def mapped[A, B](base: Reader[A])(transform: A => B): Reader[B] =
    mappedResult(base)(value => Result.Ok(transform(value)))

  def mappedResult[A, B](base: Reader[A])(
      transform: A => Result[B, DecodeError]
  ): Reader[B] =
    opaque(OpaqueSupport.Read.mapped(base)(transform))

  export Builders.{derived, ofFields, singleton, ofCases}

  def forNull[T](value: T): Reader[T] =
    opaque(OpaqueSupport.Read.nullary(value))

  override object Builders extends CommonBuilders:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type
    type FieldRepr      = CompiledSchema.Field
    type SumCaseRepr[A] = CompiledSchema.SumCase

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "Reader"

    private[scalanotation] def sumTypeClass[T](cases: List[SumCaseRepr[T]])(
        using mirror: Mirror.SumOf[T]
    ): Reader[T] =
      fromCompiled[T](
        CompiledSchema.Sum(IArray.from(cases), write = null)
      )

    private[scalanotation] def makeField[T](name: String, typeclass: Reader[T]): FieldRepr =
      CompiledSchema.Field(name, typeclass.compiled)

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): Reader[T] =
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray.from(fields),
          CompiledSchema.NamedTupleRead.from(
            PublicInternal.buildNamedTuple.asInstanceOf[Array[AnyRef] => T]
          ),
          write = null
        )
      )

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): Reader[T] =
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray.from(fields),
          CompiledSchema.NamedTupleRead.from(PublicInternal.caseClassBuilder[T]),
          write = null
        )
      )

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): Reader[T] =
      val value = mirror.fromProduct(EmptyTuple)
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray(CompiledSchema.Field(label, forNull(value).compiled)),
          CompiledSchema.NamedTupleRead.from(_ => value),
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
      CompiledSchema.SumCase(name, typeclass.compiled)

    given VectorAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Vector[T]] =
      liftAtPath[Path, Vector[T]](
        fromCompiled[Vector[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            CompiledSchema.VectorRead.FromReaderBuilder(PublicInternal.BuildVector[T]),
            write = null
          )
        )
      )

    given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
      => (wrapped: AtPath[Path + "[]", T])
      => (factory: scala.collection.Factory[T, Col[T]])
      => AtPath[Path, Col[T]] =
      liftAtPath[Path, Col[T]](
        fromCompiled[Col[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            CompiledSchema.VectorRead.FromReaderBuilder(PublicInternal.SeqFactoryVector[T, Col]),
            write = null
          )
        )
      )

    given IArrayAtPath: [Path <: String, T: ClassTag as tag]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, IArray[T]] =
      liftAtPath[Path, IArray[T]](
        fromCompiled[IArray[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            CompiledSchema.VectorRead.FromReaderBuilder(PublicInternal.BuildIArray[T]),
            write = null
          )
        )
      )

    given ArrayAtPath: [Path <: String, T: ClassTag as tag]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Array[T]] =
      liftAtPath[Path, Array[T]](
        fromCompiled[Array[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            CompiledSchema.VectorRead.FromReaderBuilder(PublicInternal.BuildArray[T]),
            write = null
          )
        )
      )

    given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
      => (wrapped: AtPath[Path + ".*", T])
      => (factory: scala.collection.Factory[(String, T), Col[String, T]])
      => AtPath[Path, Col[String, T]] =
      liftAtPath[Path, Col[String, T]](
        fromCompiled[Col[String, T]](
          CompiledSchema.DictShape(
            wrapped.typeclass.compiled,
            CompiledSchema.DictRead.FromReaderBuilder(PublicInternal.MapFactoryDict[T, Col]),
            write = null
          )
        )
      )
