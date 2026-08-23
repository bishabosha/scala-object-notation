package scalanotation

import scalanotation.internal.CommonTypeClassCompanion
import scalanotation.internal.Encode
import scalanotation.internal.ExprRenderer
import scalanotation.internal.PublicInternal
import scalanotation.internal.IdentifierSyntax

import scalanotation.schema.RawSchema

import scala.deriving.Mirror
import scala.util.NotGiven

sealed trait Writer[T]:
  def schema: RawSchema[T]

  final def contramap[U](f: U => T): Writer[U] =
    Writer.contramapped(this)(f)

  /** A copy of this writer with `config` applied to its schema. The transform is agnostic to where
    * the schema came from, so any instance can be configured — in particular the implicitly
    * resolved one, with no Mirror-based derivation involved.
    */
  final def withConfig(using config: Configured[T]): Writer[T] =
    Writer.fromSchema(Configured.applyToSchema(schema, config))

private[scalanotation] trait WriterLowPriority:
  given [T](using readWriter: ReadWriter[T]): Writer[T] =
    readWriter.writer

object Writer extends WriterLowPriority, CommonTypeClassCompanion[Writer]:
  // The write-side duals of Reader.IntMap and friends: total functions into a primitive with an
  // unboxed return, called by the renderer's specialized dispatch.
  @FunctionalInterface
  trait IntMap[-A]:
    def apply(value: A): Int

  @FunctionalInterface
  trait LongMap[-A]:
    def apply(value: A): Long

  @FunctionalInterface
  trait FloatMap[-A]:
    def apply(value: A): Float

  @FunctionalInterface
  trait DoubleMap[-A]:
    def apply(value: A): Double

  private final class Instance[T](val schema: RawSchema[T]) extends Writer[T]

  private[scalanotation] def fromSchema[T](schema0: RawSchema[T]): Writer[T] =
    new Instance(schema0)

  private[scalanotation] def schemaOf[T](typeclass: Writer[T]): RawSchema[T] =
    typeclass.schema

  private[scalanotation] def renderText[T](
      writer: Writer[T],
      value: T,
      format: TextFormat
  ): String =
    val out = ExprRenderer.Output()
    Encode.renderText(writer.schema, value, out, 0)(using format)
    out.result()

  private[scalanotation] def renderDecl[T](
      writer: Writer[T],
      name: String,
      value: T,
      format: TextFormat
  ): String =
    renderDecl(writer, name, value, packageName = "", format)

  private[scalanotation] def renderDecl[T](
      writer: Writer[T],
      name: String,
      value: T,
      packageName: String,
      format: TextFormat
  ): String =
    val out = ExprRenderer.Output()
    if packageName.nonEmpty then
      out.append("package ")
      IdentifierSyntax.appendQualifiedIdentifier(packageName, out)
      if format.pretty then out.newlineAndIndent(0)(using format)
      else
        out.append(';')
        out.tokenSpacing()(using format)
    out.append("val ")
    IdentifierSyntax.appendIdentifier(name, out)
    out.append(" = ")
    Encode.renderText(writer.schema, value, out, 0)(using format)
    out.result()

  def contramapped[A, B](base: Writer[A])(transform: B => A): Writer[B] =
    fromSchema(
      RawSchema.mapInput(base.schema)(value => transform(value.asInstanceOf[B]))
    )

  export Builders.{derived, ofFields, singleton, ofCases}

  def int[A](write: IntMap[A]): Writer[A] =
    fromSchema(RawSchema.mapInput(RawSchema.Int)(schema.SchemaMapping.IntInput(write)))

  def long[A](write: LongMap[A]): Writer[A] =
    fromSchema(RawSchema.mapInput(RawSchema.Long)(schema.SchemaMapping.LongInput(write)))

  def float[A](write: FloatMap[A]): Writer[A] =
    fromSchema(RawSchema.mapInput(RawSchema.Float)(schema.SchemaMapping.FloatInput(write)))

  def double[A](write: DoubleMap[A]): Writer[A] =
    fromSchema(RawSchema.mapInput(RawSchema.Double)(schema.SchemaMapping.DoubleInput(write)))

  def forNull[T]: Writer[T] =
    summon[Writer[Null]].contramap(_ => null)

  def tuple[A](
      slots: Iterable[Writer[?]],
      size: A => Int,
      elementValue: (A, Int) => Any
  ): Writer[A] =
    fromSchema(
      RawSchema.Tuple(
        IArray.from(slots.iterator.map(_.schema)),
        read = null,
        RawSchema.TupleWrite.from(size, elementValue)
      )
    )

  def vector[A, Elem](
      element: Writer[Elem],
      size: A => Int,
      iterator: A => Iterator[Elem]
  ): Writer[A] =
    fromSchema(
      RawSchema.Vector(
        element.schema,
        read = null,
        RawSchema.VectorWrite.from(size, iterator)
      )
    )

  def tupleOf[A, Elem](
      element: Writer[Elem],
      size: A => Int,
      iterator: A => Iterator[Elem]
  ): Writer[A] =
    fromSchema(
      RawSchema.TupleOf(
        element.schema,
        read = null,
        RawSchema.VectorWrite.from(size, iterator)
      )
    )

  def dict[A, Elem](
      element: Writer[Elem],
      size: A => Int,
      iterator: A => Iterator[(String, Elem)]
  ): Writer[A] =
    fromSchema(
      RawSchema.Dict(
        element.schema,
        read = null,
        RawSchema.DictWrite.from(size, iterator)
      )
    )

  def pairSeq[A, Key, Elem](
      key: Writer[Key],
      element: Writer[Elem],
      size: A => Int,
      iterator: A => Iterator[(Key, Elem)]
  ): Writer[A] =
    fromSchema(
      RawSchema.PairSeq(
        key.schema,
        element.schema,
        read = null,
        RawSchema.PairSeqWrite.from(size, iterator)
      )
    )

  def pairSeqAsDict[V: Writer as element, Col[X, Y] <: scala.collection.Map[X, Y]]
      : Writer[Col[String, V]] =
    fromSchema[Col[String, V]](
      RawSchema.PairSeq(
        RawSchema.String,
        element.schema,
        null,
        RawSchema.PairSeqWrite.from[Col[String, V], String, V](_.size, _.iterator)
      )
    )

  def router[A](
      name: String,
      selfKind: String
  )(
      cases: Writer[A] => Iterable[RouterSchema.WriteRoute[A]],
      write: RouterSchema.Write[A]
  ): Writer[A] =
    RouterSchema.writer(name, selfKind)(cases, write)

  object configured:
    inline def derived[T](using mirror: Mirror.Of[T], config: Configured[T]): Writer[T] =
      fromSchema(Configured.applyToSchema(Writer.derived[T].schema, config))

  override object Builders extends CommonBuilders[false, "Writer"]:
    val thisBuilder: this.type = this
    override type ThisBuilder = thisBuilder.type
    type FieldRepr            = RawSchema.Field
    type SumCaseRepr[A]       = RawSchema.SumCase

    import compiletime.ops.string.+

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

    override private[scalanotation] def tupleTypeClass[T <: Tuple](
        slots: List[RawSchema[?]]
    ): Writer[T] =
      fromSchema[T](
        RawSchema.Tuple(
          IArray.from(slots),
          read = null,
          RawSchema.TupleWrite.productLike
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
            PublicInternal.arrayVectorWrite(
              wrapped.typeclass.schema,
              RawSchema.VectorWrite.from[IArray[T], T](_.length, _.iterator)
            )
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
            PublicInternal.arrayVectorWrite(
              wrapped.typeclass.schema,
              RawSchema.VectorWrite.from[Array[T], T](_.length, _.iterator)
            )
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

    given [Path <: String, K, V, Col[X, Y] <: scala.collection.Map[X, Y]]
      => NotGiven[K =:= String]
      => (entry: AtPath[Path + "[]", (K, V)])
      => AtPath[Path, Col[K, V]] =
      liftAtPath[Path, Col[K, V]](
        fromSchema[Col[K, V]](
          pairSeqSchema(
            entry.typeclass.schema,
            read = null,
            RawSchema.PairSeqWrite.from[Col[K, V], K, V](_.size, _.iterator)
          )
        )
      )
