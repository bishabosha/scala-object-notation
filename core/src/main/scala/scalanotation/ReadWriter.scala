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

sealed trait ReadWriter[T]:
  private[scalanotation] def compiled: CompiledSchema

  private[scalanotation] final def schema: RawSchema = compiled.rawSchema

  final def reader: Reader[T] =
    Reader.fromCompiled(compiled)

  final def writer: Writer[T] =
    Writer.fromCompiled(compiled)

  final def map[U](read: T => U)(write: U => T): ReadWriter[U] =
    ReadWriter.mapped(this)(read)(write)

  final def emap[U](read: T => Result[U, DecodeError])(write: U => T): ReadWriter[U] =
    ReadWriter.mappedResult(this)(read)(write)

  final def write(value: T): Expr =
    writer.write(value)

  final def writeText(value: T): String =
    writer.writeText(value)

  final def writeText(value: T, format: TextFormat): String =
    writer.writeText(value, format)

  final def writePrettyText(value: T, indent: Int = 2): String =
    writer.writePrettyText(value, indent)

  final def writeDecl(name: String, value: T): String =
    writer.writeDecl(name, value)

  final def writeDecl(name: String, value: T, format: TextFormat): String =
    writer.writeDecl(name, value, format)

  final def writeDeclPretty(name: String, value: T, indent: Int = 2): String =
    writer.writeDeclPretty(name, value, indent)

object ReadWriter extends CommonTypeClassCompanion[ReadWriter]:
  private final class Instance[T](val compiled: CompiledSchema) extends ReadWriter[T]

  private[scalanotation] def fromCompiled[T](compiled0: CompiledSchema): ReadWriter[T] =
    new Instance(compiled0)

  private[scalanotation] def compiledOf[T](typeclass: ReadWriter[T]): CompiledSchema =
    typeclass.compiled

  private def opaque[T](
      support: OpaqueSupport.ReadWrite[T]
  ): ReadWriter[T] =
    fromCompiled[T](OpaqueSupport.compiled(support))

  protected def primitiveTypeClass[T](codec: OpaqueSupport.ReadWrite[T]): ReadWriter[T] =
    opaque(codec)

  def mapped[A, B](base: ReadWriter[A])(read: A => B)(write: B => A): ReadWriter[B] =
    mappedResult(base)(value => Result.Ok(read(value)))(write)

  def mappedResult[A, B](base: ReadWriter[A])(
      read: A => Result[B, DecodeError]
  )(
      write: B => A
  ): ReadWriter[B] =
    opaque(OpaqueSupport.ReadWrite.mapped(base)(read)(write))

  export Builders.{derived, ofFields, singleton, ofCases}

  def forNull[T](value: T): ReadWriter[T] =
    opaque(OpaqueSupport.ReadWrite.nullary(value))

  override object Builders extends CommonBuilders:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type
    type FieldRepr      = CompiledSchema.Field
    type SumCaseRepr[A] = CompiledSchema.SumCase

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "ReadWriter"

    private[scalanotation] def sumTypeClass[T](cases: List[SumCaseRepr[T]])(
        using mirror: Mirror.SumOf[T]
    ): ReadWriter[T] =
      fromCompiled[T](
        CompiledSchema.Sum(
          IArray.from(cases),
          CompiledSchema.SumWrite.from[T](mirror.ordinal)
        )
      )

    private[scalanotation] def makeField[T](name: String, typeclass: ReadWriter[T]): FieldRepr =
      CompiledSchema.Field(name, typeclass.compiled)

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): ReadWriter[T] =
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray.from(fields),
          CompiledSchema.NamedTupleRead.from(
            PublicInternal.buildNamedTuple.asInstanceOf[Array[AnyRef] => T]
          ),
          CompiledSchema.NamedTupleWrite.productLike
        )
      )

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): ReadWriter[T] =
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray.from(fields),
          CompiledSchema.NamedTupleRead.from(PublicInternal.caseClassBuilder[T]),
          CompiledSchema.NamedTupleWrite.productLike
        )
      )

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): ReadWriter[T] =
      val value = mirror.fromProduct(EmptyTuple)
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray(CompiledSchema.Field(label, forNull(value).compiled)),
          CompiledSchema.NamedTupleRead.from(_ => value),
          CompiledSchema.NamedTupleWrite.singleton
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
      CompiledSchema.SumCase(name, typeclass.compiled)

    given VectorAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Vector[T]] =
      liftAtPath[Path, Vector[T]](
        fromCompiled[Vector[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            CompiledSchema.VectorRead.FromReaderBuilder(PublicInternal.BuildVector[T]),
            CompiledSchema.VectorWrite.from[Vector[T], T](_.length, _.iterator)
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
            CompiledSchema.VectorWrite.from[Col[T], T](_.size, _.iterator)
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
            CompiledSchema.VectorWrite.from[IArray[T], T](_.length, _.iterator)
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
            CompiledSchema.VectorWrite.from[Array[T], T](_.length, _.iterator)
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
            CompiledSchema.DictWrite.from[Col[String, T], T](_.size, _.iterator)
          )
        )
      )
