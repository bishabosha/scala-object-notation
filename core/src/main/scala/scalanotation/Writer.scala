package scalanotation

import scalanotation.internal.CommonTypeClassCompanion
import scalanotation.internal.Encode
import scalanotation.internal.ExprRenderer
import scalanotation.internal.RawSchema

import scala.deriving.Mirror
import scala.util.NotGiven

sealed trait Writer[T]:
  private[scalanotation] def schema: RawSchema

  final def write(value: T): Expr =
    Encode.writeExpr(schema, value)

  private[scalanotation] final def renderText(
      value: T,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    Encode.renderText(schema, value, out, depth)

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
  private final class Instance[T](val schema: RawSchema) extends Writer[T]

  private[scalanotation] def fromSchema[T](schema0: RawSchema): Writer[T] =
    new Instance(schema0)

  private[scalanotation] def schemaOf[T](typeclass: Writer[T]): RawSchema =
    typeclass.schema

  protected def primitiveTypeClass[T](schema: RawSchema): Writer[T] =
    fromSchema(schema)

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
    fromSchema(
      RawSchema.mapInput(base.schema)(value => transform(value.asInstanceOf[B]))
    )

  export Builders.{derived, ofFields, singleton, ofCases}

  def forNull[T]: Writer[T] =
    summon[Writer[Null]].contramap(_ => null)

  override object Builders extends CommonBuilders:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type
    type FieldRepr            = RawSchema.Field
    type SumCaseRepr[A]       = RawSchema.SumCase

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "Writer"

    private[scalanotation] def sumTypeClass[T](cases: List[SumCaseRepr[T]])(
        using mirror: Mirror.SumOf[T]
    ): Writer[T] =
      fromSchema[T](
        RawSchema.Sum(
          IArray.from(cases),
          RawSchema.SumWrite.from[T](mirror.ordinal)
        )
      )

    private[scalanotation] def makeField[T](name: String, typeclass: Writer[T]): FieldRepr =
      RawSchema.Field(name, typeclass.schema)

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): Writer[T] =
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray.from(fields),
          read = null,
          RawSchema.NamedTupleWrite.productLike
        )
      )

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): Writer[T] =
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray.from(fields),
          read = null,
          RawSchema.NamedTupleWrite.productLike
        )
      )

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): Writer[T] =
      fromSchema[T](
        RawSchema.NamedTuple(
          IArray(RawSchema.Field(label, forNull[Unit].schema)),
          read = null,
          RawSchema.NamedTupleWrite.singleton
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
      RawSchema.SumCase(name, typeclass.schema)

    given VectorAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Vector[T]] =
      liftAtPath[Path, Vector[T]](
        fromSchema[Vector[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            read = null,
            RawSchema.VectorWrite.from[Vector[T], T](_.length, _.iterator)
          )
        )
      )

    given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Col[T]] =
      liftAtPath[Path, Col[T]](
        fromSchema[Col[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            read = null,
            RawSchema.VectorWrite.from[Col[T], T](_.size, _.iterator)
          )
        )
      )

    given IArrayAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, IArray[T]] =
      liftAtPath[Path, IArray[T]](
        fromSchema[IArray[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            read = null,
            RawSchema.VectorWrite.from[IArray[T], T](_.length, _.iterator)
          )
        )
      )

    given ArrayAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Array[T]] =
      liftAtPath[Path, Array[T]](
        fromSchema[Array[T]](
          RawSchema.Vector(
            wrapped.typeclass.schema,
            read = null,
            RawSchema.VectorWrite.from[Array[T], T](_.length, _.iterator)
          )
        )
      )

    given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
      => (wrapped: AtPath[Path + ".*", T])
      => AtPath[Path, Col[String, T]] =
      liftAtPath[Path, Col[String, T]](
        fromSchema[Col[String, T]](
          RawSchema.Dict(
            wrapped.typeclass.schema,
            read = null,
            RawSchema.DictWrite.from[Col[String, T], T](_.size, _.iterator)
          )
        )
      )
