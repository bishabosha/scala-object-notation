package scalanotation

import scalanotation.Writer.Builders.AtPath
import scalanotation.internal.CommonDerivationBuilders
import scalanotation.internal.CompiledSchema
import scalanotation.internal.ExprRenderer
import scalanotation.internal.OpaqueSupport
import scalanotation.internal.RawSchema

import scala.NamedTuple.NamedTuple
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

object Writer extends WriterLowPriority:
  type Field[A]   = CompiledSchema.Field
  type SumCase[A] = CompiledSchema.SumCase

  private final class Instance[T](val compiled: CompiledSchema) extends Writer[T]

  private[scalanotation] def fromCompiled[T](compiled0: CompiledSchema): Writer[T] =
    new Instance(compiled0)

  private def opaque[T](
      support: OpaqueSupport.Write[T]
  ): Writer[T] =
    fromCompiled[T](OpaqueSupport.compiled(support))

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

  inline def derived[T](using mirror: Mirror.Of[T]): Writer[T] =
    inline mirror match
      case m: Mirror.ProductOf[T] =>
        compiletime.summonFrom {
          case _: (m.MirroredElemTypes =:= EmptyTuple) =>
            val label = compiletime.constValue[m.MirroredLabel]
            singleton[T](using m)(using ValueOf(label))
          case _ =>
            ofFields[T](using m)(
              using compiletime.summonInline[
                Builders.ProductFieldsAtPath["", m.MirroredElemLabels, m.MirroredElemTypes]
              ]
            )
        }
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
  ): Writer[T] =
    Builders.productTypeClass[T](atPath.fields)

  def singleton[T](using mirror: Mirror.ProductOf[T])(
      using label: ValueOf[mirror.MirroredLabel],
      noFields: mirror.MirroredElemTypes =:= EmptyTuple
  ): Writer[T] =
    Builders.singletonTypeClass[T](label.value)

  def forNull[T]: Writer[T] =
    opaque(OpaqueSupport.Write.nullary)

  def ofCases[T](using mirror: Mirror.SumOf[T])(
      using casesAtPath: Builders.SumCasesAtPath[
        "",
        T,
        mirror.MirroredElemLabels,
        mirror.MirroredElemTypes
      ]
  ): Writer[T] =
    fromCompiled[T](
      CompiledSchema.Sum(
        IArray.from(casesAtPath.cases),
        CompiledSchema.SumWrite.from[T](mirror.ordinal)
      )
    )

  given ExprSchema: Writer[Expr] =
    opaque(OpaqueSupport.Primitives.ExprCodec.write)

  given StringSchema: Writer[String] =
    opaque(OpaqueSupport.Primitives.StringCodec.write)

  given CharSchema: Writer[Char] =
    opaque(OpaqueSupport.Primitives.CharCodec.write)

  given IntSchema: Writer[Int] =
    opaque(OpaqueSupport.Primitives.IntCodec.write)

  given LongSchema: Writer[Long] =
    opaque(OpaqueSupport.Primitives.LongCodec.write)

  given FloatSchema: Writer[Float] =
    opaque(OpaqueSupport.Primitives.FloatCodec.write)

  given DoubleSchema: Writer[Double] =
    opaque(OpaqueSupport.Primitives.DoubleCodec.write)

  given BooleanSchema: Writer[Boolean] =
    opaque(OpaqueSupport.Primitives.BooleanCodec.write)

  given OptionSchema: [T] => (atPath: AtPath["", Option[T]]) => Writer[Option[T]] =
    atPath.typeclass

  given VectorSchema: [T] => (atPath: AtPath["", Vector[T]]) => Writer[Vector[T]] =
    atPath.typeclass

  given IArraySchema: [T] => (atPath: AtPath["", IArray[T]]) => Writer[IArray[T]] =
    atPath.typeclass

  given ArraySchema: [T] => (atPath: AtPath["", Array[T]]) => Writer[Array[T]] =
    atPath.typeclass

  given SeqSchema: [Col[X] <: scala.collection.Seq[X], T] => (atPath: AtPath["", Col[T]])
    => Writer[Col[T]] =
    atPath.typeclass

  given MapSchema
      : [Col[X, Y] <: scala.collection.Map[X, Y], T] => (atPath: AtPath["", Col[String, T]])
        => Writer[Col[String, T]] =
    atPath.typeclass

  given NamedTupleSchema: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: AtPath["", NT]) => Writer[NT] =
    atPath.typeclass

  object Builders extends CommonDerivationBuilders[Writer]:
    type FieldRepr      = Field[?]
    type SumCaseRepr[A] = SumCase[A]

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "Writer"

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

    given OptionAtPath: [Path <: String, T]
      => NonNestedOption[Path, T]
      => (wrapped: AtPath[Path, T])
      => AtPath[Path, Option[T]] =
      liftAtPath[Path, Option[T]](
        fromCompiled[Option[T]](
          CompiledSchema.OptionShape(wrapped.typeclass.compiled)
        )
      )
