package scalanotation

import scalanotation.ReadWriter.Builders.AtPath
import scalanotation.internal.CommonDerivationBuilders
import scalanotation.internal.CompiledSchema
import scalanotation.internal.OpaqueSupport
import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.NamedTuple.NamedTuple
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

object ReadWriter:
  type Field[A]   = CompiledSchema.Field
  type SumCase[A] = CompiledSchema.SumCase

  private final class Instance[T](val compiled: CompiledSchema) extends ReadWriter[T]

  private[scalanotation] def fromCompiled[T](compiled0: CompiledSchema): ReadWriter[T] =
    new Instance(compiled0)

  private def opaque[T](
      support: OpaqueSupport.ReadWrite[T]
  ): ReadWriter[T] =
    fromCompiled[T](OpaqueSupport.compiled(support))

  def mapped[A, B](base: ReadWriter[A])(read: A => B)(write: B => A): ReadWriter[B] =
    mappedResult(base)(value => Result.Ok(read(value)))(write)

  def mappedResult[A, B](base: ReadWriter[A])(
      read: A => Result[B, DecodeError]
  )(
      write: B => A
  ): ReadWriter[B] =
    opaque(OpaqueSupport.ReadWrite.mapped(base)(read)(write))

  inline def derived[T](using mirror: Mirror.Of[T]): ReadWriter[T] =
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
  ): ReadWriter[T] =
    Builders.productTypeClass[T](atPath.fields)

  def singleton[T](using mirror: Mirror.ProductOf[T])(
      using label: ValueOf[mirror.MirroredLabel],
      noFields: mirror.MirroredElemTypes =:= EmptyTuple
  ): ReadWriter[T] =
    Builders.singletonTypeClass[T](label.value)

  def forNull[T](value: T): ReadWriter[T] =
    opaque(OpaqueSupport.ReadWrite.nullary(value))

  def ofCases[T](using mirror: Mirror.SumOf[T])(
      using casesAtPath: Builders.SumCasesAtPath[
        "",
        T,
        mirror.MirroredElemLabels,
        mirror.MirroredElemTypes
      ]
  ): ReadWriter[T] =
    fromCompiled[T](
      CompiledSchema.Sum(
        IArray.from(casesAtPath.cases),
        CompiledSchema.SumWrite.from[T](mirror.ordinal)
      )
    )

  given ExprSchema: ReadWriter[Expr] =
    opaque(OpaqueSupport.Primitives.ExprCodec)

  given StringSchema: ReadWriter[String] =
    opaque(OpaqueSupport.Primitives.StringCodec)

  given CharSchema: ReadWriter[Char] =
    opaque(OpaqueSupport.Primitives.CharCodec)

  given IntSchema: ReadWriter[Int] =
    opaque(OpaqueSupport.Primitives.IntCodec)

  given LongSchema: ReadWriter[Long] =
    opaque(OpaqueSupport.Primitives.LongCodec)

  given FloatSchema: ReadWriter[Float] =
    opaque(OpaqueSupport.Primitives.FloatCodec)

  given DoubleSchema: ReadWriter[Double] =
    opaque(OpaqueSupport.Primitives.DoubleCodec)

  given BooleanSchema: ReadWriter[Boolean] =
    opaque(OpaqueSupport.Primitives.BooleanCodec)

  given OptionSchema: [T] => (atPath: AtPath["", Option[T]]) => ReadWriter[Option[T]] =
    atPath.typeclass

  given VectorSchema: [T] => (atPath: AtPath["", Vector[T]]) => ReadWriter[Vector[T]] =
    atPath.typeclass

  given IArraySchema: [T] => (atPath: AtPath["", IArray[T]]) => ReadWriter[IArray[T]] =
    atPath.typeclass

  given ArraySchema: [T] => (atPath: AtPath["", Array[T]]) => ReadWriter[Array[T]] =
    atPath.typeclass

  given SeqSchema: [Col[X] <: scala.collection.Seq[X], T] => (atPath: AtPath["", Col[T]])
    => ReadWriter[Col[T]] =
    atPath.typeclass

  given MapSchema
      : [Col[X, Y] <: scala.collection.Map[X, Y], T] => (atPath: AtPath["", Col[String, T]])
        => ReadWriter[Col[String, T]] =
    atPath.typeclass

  given NamedTupleSchema: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: AtPath["", NT]) => ReadWriter[NT] =
    atPath.typeclass

  object Builders extends CommonDerivationBuilders[ReadWriter]:
    type FieldRepr      = Field[?]
    type SumCaseRepr[A] = SumCase[A]

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "ReadWriter"

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

    given OptionAtPath: [Path <: String, T]
      => NonNestedOption[Path, T]
      => (wrapped: AtPath[Path, T])
      => AtPath[Path, Option[T]] =
      liftAtPath[Path, Option[T]](
        fromCompiled[Option[T]](
          CompiledSchema.OptionShape(wrapped.typeclass.compiled)
        )
      )
