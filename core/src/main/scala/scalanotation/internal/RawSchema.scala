package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.internal.Internal
import steps.result.Result

import Result.eval.check
import Result.eval.raise
import scalanotation.internal.RawSchema.SchemaMapping

/** Internal API of scalanotation that describes expected structure, builtin primitive schemas, and
  * mapped schema adapters used to read and write typed values.
  */
private[scalanotation] enum RawSchema:
  case NamedTuple(
      fields: IArray[RawSchema.Field],
      read: RawSchema.NamedTupleRead | Null = null,
      write: RawSchema.NamedTupleWrite | Null = null,
      allowSkippedNullableFields: Boolean = false
  )
  case PartialNamedTuple(base: RawSchema, alreadySeenField: String)
  case Sum(
      cases: IArray[RawSchema.SumCase],
      write: RawSchema.SumWrite | Null = null
  )
  case DiscriminatorSum(
      cases: IArray[RawSchema.SumCase],
      write: RawSchema.SumWrite | Null,
      discriminatorField: String
  )
  case Vector(
      element: RawSchema,
      read: RawSchema.VectorRead | Null = null,
      write: RawSchema.VectorWrite | Null = null
  )
  case Dict(
      element: RawSchema,
      read: RawSchema.DictRead | Null = null,
      write: RawSchema.DictWrite | Null = null
  )
  case Option(inner: RawSchema)
  case Mapped(base: RawSchema, mapping: RawSchema.SchemaMapping)
  case AnyExpr
  case String
  case Char
  case Int
  case Long
  case Float
  case Double
  case Boolean
  case Null

  private[scalanotation] final def withMapping(
      f: RawSchema.SchemaMapping => RawSchema.SchemaMapping
  ): RawSchema =
    this match
      case RawSchema.Mapped(base, mapping0) => RawSchema.Mapped(base, f(mapping0))
      case _                                => RawSchema.Mapped(this, f(SchemaMapping.empty))

  private lazy val properties: java.util.concurrent.ConcurrentHashMap[RawSchema.Key[?], AnyRef] =
    java.util.concurrent.ConcurrentHashMap()

  private def getOrComputeProperty[T <: AnyRef](key: RawSchema.Key[T])(
      compute: => T
  ): T =
    properties.computeIfAbsent(key, _ => compute).asInstanceOf[T]

  def isValidNamedTuple[T: Internal.NameSet](
      pool: Internal.LocalPool[T]
  ): Result[Unit, DecodeError] =
    getOrComputeProperty(RawSchema.IsValidNamedTupleSchema) {
      validateNamedTuple(pool)
    }

  private def validateNamedTuple[T: Internal.NameSet](
      pool: Internal.LocalPool[T]
  ): Result[Unit, DecodeError] = pool.withBorrowed { seenNames =>
    Result.task:
      this match
        case namedTuple: RawSchema.NamedTuple =>
          val fields = namedTuple.fields
          val len    = fields.length
          var i      = 0
          while i < len do
            val field                    = fields(i)
            val name                     = field.name
            def fmtErr(err: DecodeError) = err.atPath(s".${name}")
            if seenNames.alreadySeen(name) then
              raise(fmtErr(DecodeError.DuplicateSchemaField(name)))
            i += 1
        case RawSchema.Mapped(base, _) =>
          base.validateNamedTuple(pool).check
        case _ => ()
  }

  final def describeSelf: String =
    this match
      case namedTuple: RawSchema.NamedTuple =>
        val fields = namedTuple.fields
        if fields.isEmpty then "AnyNamedTuple"
        else fields.map(f => s"${f.name}: ...").mkString("(", ", ", ")")
      case partial: RawSchema.PartialNamedTuple =>
        partial.base.describeSelf
      case sum: RawSchema.Sum =>
        val cases = sum.cases
        if cases.isEmpty then "AnyNamedTuple"
        else cases.iterator.map(k => s"(${k.name}: ...)").mkString(" | ")
      case sum: RawSchema.DiscriminatorSum =>
        val cases = sum.cases
        if cases.isEmpty then "AnyNamedTuple"
        else
          val field = sum.discriminatorField
          cases.iterator.map(k => s"""($field: "${k.name}", ...)""").mkString(" | ")
      case _: RawSchema.Vector      => "Vector[...]"
      case _: RawSchema.Dict        => "AnyNamedTuple"
      case RawSchema.AnyExpr        => "Any"
      case RawSchema.String         => "String"
      case RawSchema.Char           => "Char"
      case RawSchema.Int            => "Int"
      case RawSchema.Long           => "Long"
      case RawSchema.Float          => "Float"
      case RawSchema.Double         => "Double"
      case RawSchema.Boolean        => "Boolean"
      case RawSchema.Null           => "Null"
      case option: RawSchema.Option =>
        option.inner match
          case _: RawSchema.Option => "... | Null"
          case other               => s"${other.describeSelf} | Null"
      case RawSchema.Mapped(base, _) =>
        base.describeSelf

private[scalanotation] object RawSchema:
  type ResultMap = Any => Result[Any, DecodeError]
  type InputMap  = Any => Any

  final case class SchemaMapping(
      resultMap: ResultMap | Null = null,
      inputMap: InputMap | Null = null
  ):
    def mapResult(result: Result[Any, DecodeError]): Result[Any, DecodeError] =
      val fn = resultMap
      if fn == null then result
      else result.flatMap(fn)

    def mapInput(value: Any): Any =
      val fn = inputMap
      if fn == null then value
      else fn(value)

    def withResultMap(f: ResultMap): SchemaMapping =
      copy(
        resultMap =
          if resultMap == null then f
          else value => resultMap.nn(value).flatMap(f)
      )

    def withInputMap(f: InputMap): SchemaMapping =
      copy(
        inputMap =
          if inputMap == null then f
          else value => inputMap.nn(f(value))
      )

    def withMapped(resultMap0: ResultMap, inputMap0: InputMap): SchemaMapping =
      withResultMap(resultMap0).withInputMap(inputMap0)

  object SchemaMapping:
    val empty: SchemaMapping = SchemaMapping()

  final class Key[T]()
  val IsValidNamedTupleSchema: Key[Result[Unit, DecodeError]] = Key()

  final case class Field(name: String, schema: RawSchema)

  final case class SumCase(name: String, schema: RawSchema)

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

  trait SumWrite:
    def caseIndex(value: Any): Int

  object SumWrite:
    def from[T](select: T => Int): SumWrite = new:
      def caseIndex(value: Any): Int = select(value.asInstanceOf[T])

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

  def mapResult(
      base: RawSchema
  )(
      f: ResultMap
  ): RawSchema =
    base.withMapping(_.withResultMap(f))

  def mapInput(
      base: RawSchema
  )(
      f: InputMap
  ): RawSchema =
    base.withMapping(_.withInputMap(f))

  def mapResultAndInput(
      base: RawSchema
  )(
      resultMap0: ResultMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withMapped(resultMap0, inputMap0))
