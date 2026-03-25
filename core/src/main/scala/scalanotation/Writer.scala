package scalanotation

import scalanotation.internal.CommonTypeClassCompanion
import scalanotation.internal.CompiledSchema
import scalanotation.internal.ExprRenderer
import scalanotation.internal.OpaqueSupport
import scalanotation.internal.RawSchema

import scala.deriving.Mirror
import scala.util.NotGiven

sealed trait Writer[T]:
  private[scalanotation] def compiled: CompiledSchema

  private[scalanotation] final def schema: RawSchema = compiled.rawSchema

  final def write(value: T): Expr =
    CompiledSchema.writeExpr(compiled, value)

  private[scalanotation] final def renderText(
      value: T,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    CompiledSchema.renderText(compiled, value, out, depth)

  final def contramap[U](f: U => T): Writer[U] =
    Writer.contramapped(this)(f)

  final def writeText(value: T): String =
    Writer.renderText(this, value, TextFormat.compact)

  final def writeText(value: T, format: TextFormat): String =
    Writer.renderText(this, value, format)

  final def writePrettyText(value: T, indent: Int = 2): String =
    writeText(value, TextFormat.pretty(indent))

  final def writeDecl(name: String, value: T): String =
    Writer.renderDecl(this, name, value, TextFormat.compact)

  final def writeDecl(name: String, value: T, format: TextFormat): String =
    Writer.renderDecl(this, name, value, format)

  final def writeDeclPretty(name: String, value: T, indent: Int = 2): String =
    writeDecl(name, value, TextFormat.pretty(indent))

private[scalanotation] trait WriterLowPriority:
  given [T](using readWriter: ReadWriter[T]): Writer[T] =
    readWriter.writer

object Writer extends WriterLowPriority, CommonTypeClassCompanion[Writer]:
  private final class Instance[T](val compiled: CompiledSchema) extends Writer[T]

  private[scalanotation] def fromCompiled[T](compiled0: CompiledSchema): Writer[T] =
    new Instance(compiled0)

  private[scalanotation] def compiledOf[T](typeclass: Writer[T]): CompiledSchema =
    typeclass.compiled

  private def opaque[T](
      support: OpaqueSupport.Write[T]
  ): Writer[T] =
    fromCompiled[T](OpaqueSupport.compiled(support))

  protected def primitiveTypeClass[T](codec: OpaqueSupport.ReadWrite[T]): Writer[T] =
    opaque(codec.write)

  private[scalanotation] def renderText[T](
      writer: Writer[T],
      value: T,
      format: TextFormat
  ): String =
    val out = ExprRenderer.Output()
    writer.renderText(value, out, 0)(using format)
    out.result()

  private[scalanotation] def renderDecl[T](
      writer: Writer[T],
      name: String,
      value: T,
      format: TextFormat
  ): String =
    val out = ExprRenderer.Output()
    out.append("val ")
    out.append(name)
    out.append(" = ")
    writer.renderText(value, out, 0)(using format)
    out.result()

  def contramapped[A, B](base: Writer[A])(transform: B => A): Writer[B] =
    opaque(OpaqueSupport.Write.contramapped(base)(transform))

  export Builders.{derived, ofFields, singleton, ofCases}

  def forNull[T]: Writer[T] =
    opaque(OpaqueSupport.Write.nullary)

  override object Builders extends CommonBuilders:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type
    type FieldRepr      = CompiledSchema.Field
    type SumCaseRepr[A] = CompiledSchema.SumCase

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "Writer"

    private[scalanotation] def sumTypeClass[T](cases: List[SumCaseRepr[T]])(
        using mirror: Mirror.SumOf[T]
    ): Writer[T] =
      fromCompiled[T](
        CompiledSchema.Sum(
          IArray.from(cases),
          CompiledSchema.SumWrite.from[T](mirror.ordinal)
        )
      )

    private[scalanotation] def makeField[T](name: String, typeclass: Writer[T]): FieldRepr =
      CompiledSchema.Field(name, typeclass.compiled)

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): Writer[T] =
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray.from(fields),
          read = null,
          CompiledSchema.NamedTupleWrite.productLike
        )
      )

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): Writer[T] =
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray.from(fields),
          read = null,
          CompiledSchema.NamedTupleWrite.productLike
        )
      )

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): Writer[T] =
      fromCompiled[T](
        CompiledSchema.NamedTuple(
          IArray(CompiledSchema.Field(label, forNull[Unit].compiled)),
          read = null,
          CompiledSchema.NamedTupleWrite.singleton
        )
      )

    private[scalanotation] def nullaryEnumCaseTypeClass[T](
        using mirror: Mirror.ProductOf[T],
        empty: mirror.MirroredElemTypes =:= EmptyTuple
    ): Writer[T] =
      forNull[T]

    private[scalanotation] def sumCaseTypeClass[A, T <: A](
        name: String,
        typeclass: Writer[T]
    ): SumCaseRepr[A] =
      CompiledSchema.SumCase(name, typeclass.compiled)

    given VectorAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Vector[T]] =
      liftAtPath[Path, Vector[T]](
        fromCompiled[Vector[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            read = null,
            CompiledSchema.VectorWrite.from[Vector[T], T](_.length, _.iterator)
          )
        )
      )

    given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Col[T]] =
      liftAtPath[Path, Col[T]](
        fromCompiled[Col[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            read = null,
            CompiledSchema.VectorWrite.from[Col[T], T](_.size, _.iterator)
          )
        )
      )

    given IArrayAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, IArray[T]] =
      liftAtPath[Path, IArray[T]](
        fromCompiled[IArray[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            read = null,
            CompiledSchema.VectorWrite.from[IArray[T], T](_.length, _.iterator)
          )
        )
      )

    given ArrayAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Array[T]] =
      liftAtPath[Path, Array[T]](
        fromCompiled[Array[T]](
          CompiledSchema.VectorShape(
            wrapped.typeclass.compiled,
            read = null,
            CompiledSchema.VectorWrite.from[Array[T], T](_.length, _.iterator)
          )
        )
      )

    given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
      => (wrapped: AtPath[Path + ".*", T])
      => AtPath[Path, Col[String, T]] =
      liftAtPath[Path, Col[String, T]](
        fromCompiled[Col[String, T]](
          CompiledSchema.DictShape(
            wrapped.typeclass.compiled,
            read = null,
            CompiledSchema.DictWrite.from[Col[String, T], T](_.size, _.iterator)
          )
        )
      )
