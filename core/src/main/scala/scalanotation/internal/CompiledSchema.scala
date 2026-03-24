package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.TextFormat
import steps.result.Result

private[scalanotation] sealed trait CompiledSchema:
  def rawSchema: RawSchema

private[scalanotation] object CompiledSchema:
  type TokenRead = TokenDecoder => Result[Any, DecodeError]
  type ExprRead  = (ExprDecoder, Expr) => Result[Any, DecodeError]
  type ExprWrite = Any => Expr
  type TextWrite = (Any, ExprRenderer.Output, Int, TextFormat) => Unit

  final case class Opaque(
      rawSchema: RawSchema,
      tokenRead: TokenRead | Null = null,
      exprRead: ExprRead | Null = null,
      exprWrite: ExprWrite | Null = null,
      textWrite: TextWrite | Null = null
  ) extends CompiledSchema

  final case class Field(name: String, schema: CompiledSchema):
    def schemaField: RawSchema.Field = RawSchema.Field(name, schema.rawSchema)

  final case class SumCase(name: String, schema: CompiledSchema):
    def schemaCase: RawSchema.SumCase = RawSchema.SumCase(name, schema.rawSchema)

  trait NamedTupleRead:
    def build(values: Array[AnyRef]): Any

  object NamedTupleRead:
    def from[T](build0: Array[AnyRef] => T): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any = build0(values)

  trait NamedTupleWrite:
    def fieldValue(value: Any, index: Int): Any

  object NamedTupleWrite:
    val productLike: NamedTupleWrite = new:
      def fieldValue(value: Any, index: Int): Any =
        value.asInstanceOf[Product].productElement(index)

    val singleton: NamedTupleWrite = new:
      def fieldValue(value: Any, index: Int): Any = ()

  final case class NamedTuple(
      fields: IArray[Field],
      read: NamedTupleRead | Null,
      write: NamedTupleWrite | Null
  ) extends CompiledSchema:
    lazy val rawSchema: RawSchema =
      RawSchema.NamedTuple(IArray.from(fields.iterator.map(_.schemaField)))

  trait SumWrite:
    def caseIndex(value: Any): Int

  object SumWrite:
    def from[T](select: T => Int): SumWrite = new:
      def caseIndex(value: Any): Int = select(value.asInstanceOf[T])

  final case class Sum(
      cases: IArray[SumCase],
      write: SumWrite | Null
  ) extends CompiledSchema:
    lazy val rawSchema: RawSchema =
      RawSchema.Sum(cases.iterator.map(sumCase => sumCase.name -> sumCase.schemaCase).toMap)

    lazy val casesByName: Map[String, SumCase] =
      cases.iterator.map(sumCase => sumCase.name -> sumCase).toMap

  trait VectorRead:
    type State
    def init(): State
    def add(state: State, elem: Any): State
    def finish(state: State): Any

  object VectorRead:
    final case class FromReaderBuilder[Elem, Repr, A](
        builder: Reader.VectorBuilder[Elem, Repr, A]
    ) extends VectorRead:
      type State = Repr

      def init(): State = builder.init()

      def add(state: State, elem: Any): State =
        builder.add(state, elem.asInstanceOf[Elem])

      def finish(state: State): Any = builder.finish(state)

  trait VectorWrite:
    def size(value: Any): Int
    def iterator(value: Any): Iterator[Any]

  object VectorWrite:
    def from[A, Elem](size0: A => Int, iterator0: A => Iterator[Elem]): VectorWrite = new:
      def size(value: Any): Int = size0(value.asInstanceOf[A])

      def iterator(value: Any): Iterator[Any] =
        iterator0(value.asInstanceOf[A]).asInstanceOf[Iterator[Any]]

  final case class VectorShape(
      element: CompiledSchema,
      read: VectorRead | Null,
      write: VectorWrite | Null
  ) extends CompiledSchema:
    lazy val rawSchema: RawSchema = RawSchema.Vector(element.rawSchema)

  trait DictRead:
    type State
    def init(): State
    def add(state: State, key: String, elem: Any): State
    def finish(state: State): Any

  object DictRead:
    final case class FromReaderBuilder[Elem, Repr, A](
        builder: Reader.DictBuilder[Elem, Repr, A]
    ) extends DictRead:
      type State = Repr

      def init(): State = builder.init()

      def add(state: State, key: String, elem: Any): State =
        builder.add(state, key, elem.asInstanceOf[Elem])

      def finish(state: State): Any = builder.finish(state)

  trait DictWrite:
    def size(value: Any): Int
    def iterator(value: Any): Iterator[(String, Any)]

  object DictWrite:
    def from[A, Elem](size0: A => Int, iterator0: A => Iterator[(String, Elem)]): DictWrite = new:
      def size(value: Any): Int = size0(value.asInstanceOf[A])

      def iterator(value: Any): Iterator[(String, Any)] =
        iterator0(value.asInstanceOf[A]).map((key, elem) => key -> elem.asInstanceOf[Any])

  final case class DictShape(
      element: CompiledSchema,
      read: DictRead | Null,
      write: DictWrite | Null
  ) extends CompiledSchema:
    lazy val rawSchema: RawSchema = RawSchema.Dict(element.rawSchema)

  final case class OptionShape(inner: CompiledSchema) extends CompiledSchema:
    lazy val rawSchema: RawSchema = RawSchema.Option(inner.rawSchema)

  private def missingWriteCapability(schema: CompiledSchema): Nothing =
    throw IllegalStateException(
      s"write is not available for schema ${schema.rawSchema.describeSelf}"
    )

  def writeExpr(schema: CompiledSchema, value: Any): Expr =
    schema match
      case Opaque(_, _, _, exprWrite, _) =>
        if exprWrite == null then missingWriteCapability(schema)
        exprWrite.nn(value)
      case NamedTuple(fields, _, write) =>
        if write == null then missingWriteCapability(schema)
        val access     = write.nn
        val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
        var index      = 0
        while index < fields.length do
          val field = fields(index)
          fieldExprs += ((field.name, writeExpr(field.schema, access.fieldValue(value, index))))
          index += 1
        Expr.NamedTupleExpr(fieldExprs.result())
      case Sum(cases, write) =>
        if write == null then missingWriteCapability(schema)
        val sumCase = cases(write.nn.caseIndex(value))
        Expr.NamedTupleExpr(IndexedSeq(sumCase.name -> writeExpr(sumCase.schema, value)))
      case VectorShape(element, _, write) =>
        if write == null then missingWriteCapability(schema)
        Expr.VectorExpr(write.nn.iterator(value).map(writeExpr(element, _)).toIndexedSeq)
      case DictShape(element, _, write) =>
        if write == null then missingWriteCapability(schema)
        Expr.NamedTupleExpr(
          write.nn.iterator(value).map((key, elem) => key -> writeExpr(element, elem)).toIndexedSeq
        )
      case OptionShape(inner) =>
        value.asInstanceOf[Option[Any]] match
          case Some(innerValue) => writeExpr(inner, innerValue)
          case None             => Expr.NullConstant

  def renderText(
      schema: CompiledSchema,
      value: Any,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case Opaque(_, _, _, _, textWrite) =>
        if textWrite == null then missingWriteCapability(schema)
        textWrite.nn(value, out, depth, format)
      case NamedTuple(fields, _, write) =>
        if write == null then missingWriteCapability(schema)
        val access = write.nn
        ExprRenderer.renderNamedTuple(out, depth, fields.length) { index =>
          val field = fields(index)
          out.append(field.name)
          out.append(" = ")
          renderText(field.schema, access.fieldValue(value, index), out, depth + 1)
        }
      case Sum(cases, write) =>
        if write == null then missingWriteCapability(schema)
        val sumCase = cases(write.nn.caseIndex(value))
        ExprRenderer.renderNamedTuple(out, depth, 1) { _ =>
          out.append(sumCase.name)
          out.append(" = ")
          renderText(sumCase.schema, value, out, depth + 1)
        }
      case VectorShape(element, _, write) =>
        if write == null then missingWriteCapability(schema)
        val values = write.nn.iterator(value)
        ExprRenderer.renderVector(out, depth, write.nn.size(value)) { _ =>
          renderText(element, values.next(), out, depth + 1)
        }
      case DictShape(element, _, write) =>
        if write == null then missingWriteCapability(schema)
        val values = write.nn.iterator(value)
        ExprRenderer.renderNamedTuple(out, depth, write.nn.size(value)) { _ =>
          val (key, elem) = values.next()
          out.append(key)
          out.append(" = ")
          renderText(element, elem, out, depth + 1)
        }
      case OptionShape(inner) =>
        value.asInstanceOf[Option[Any]] match
          case Some(innerValue) => renderText(inner, innerValue, out, depth)
          case None             => out.append("null")
