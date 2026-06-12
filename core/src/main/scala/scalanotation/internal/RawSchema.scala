package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.TypedFactory
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
  case Tuple(
      slots: IArray[RawSchema],
      read: RawSchema.TupleRead | Null = null,
      write: RawSchema.TupleWrite | Null = null
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
      case tuple: RawSchema.Tuple =>
        RawSchema.describeTupleSlots(tuple.slots.length)
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

  def describeTupleSlots(size: Int): String =
    size match
      case 0 => "EmptyTuple"
      case 1 => "... *: EmptyTuple"
      case _ => Iterator.fill(size)("...").mkString("(", ", ", ")")

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
    /** Abstract builder state, consumed and returned by the `add` methods. Implementations that
      * only provide the legacy [[build]] inherit the defaults, where the state is the
      * `Array[AnyRef]` of boxed field values handed to [[build]] by [[finish]].
      */
    type State

    /** legacy entry point: builds the result from boxed field values */
    def build(values: Array[AnyRef]): Any

    def init(size: Int, slots: scalanotation.BuilderSlots | Null): State =
      if slots != null && slotsFactory != null then slots.reset(size).asInstanceOf[State]
      else (new Array[AnyRef](size)).asInstanceOf[State]

    def add(state: State, index: Int, value: Any): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setRef(index, value)
      state

    // typed adds: the defaults box and delegate to `add`; specialized states override to
    // consume the primitive directly
    def addString(state: State, index: Int, value: String): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setString(index, value)
      state

    def addChar(state: State, index: Int, value: Char): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setChar(index, value)
      state
    def addInt(state: State, index: Int, value: Int): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setInt(index, value)
      state
    def addLong(state: State, index: Int, value: Long): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setLong(index, value)
      state
    def addFloat(state: State, index: Int, value: Float): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setFloat(index, value)
      state
    def addDouble(state: State, index: Int, value: Double): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setDouble(index, value)
      state
    def addBoolean(state: State, index: Int, value: Boolean): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setBoolean(index, value)
      state

    def finish(state: State): Any = state match
      case arr: Array[AnyRef]                => build(arr)
      case slots: scalanotation.BuilderSlots =>
        slotsFactory.nn.fromSlots(slots)

    /** Optional low-boxing finalizer: when non-null, a decoder may fill its pooled
      * [[scalanotation.BuilderSlots]] with typed field values instead of threading [[State]], and
      * finalize via [[scalanotation.TypedFactory.fromSlots]].
      */
    def slotsFactory: TypedFactory | Null = null

  object NamedTupleRead:
    def from[T](build0: Array[AnyRef] => T): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any = build0(values)

    def from[T](
        build0: Array[AnyRef] => T,
        slotsFactory0: TypedFactory | Null
    ): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any          = build0(values)
      override def slotsFactory: TypedFactory | Null = slotsFactory0

    /** attaches (or replaces) a [[scalanotation.TypedFactory]] on an existing read */
    def withSlotsFactory(read: NamedTupleRead, factory: TypedFactory): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any          = read.build(values)
      override def slotsFactory: TypedFactory | Null = factory

  trait NamedTupleWrite:
    def fieldValue(value: Any, index: Int): Any

  object NamedTupleWrite:
    val productLike: NamedTupleWrite = new:
      def fieldValue(value: Any, index: Int): Any =
        value.asInstanceOf[Product].productElement(index)

    val singleton: NamedTupleWrite = new:
      def fieldValue(value: Any, index: Int): Any = ()

  trait TupleRead:
    type State
    def init(size: Int): State
    def initPooled(size: Int, pooled: scalanotation.BuilderSlots | Null): State
    def add(state: State, index: Int, elem: Any): State

    // typed adds: the defaults box and delegate to `add`
    def addString(state: State, index: Int, elem: String): State   = add(state, index, elem)
    def addChar(state: State, index: Int, elem: Char): State       = add(state, index, elem)
    def addInt(state: State, index: Int, elem: Int): State         = add(state, index, elem)
    def addLong(state: State, index: Int, elem: Long): State       = add(state, index, elem)
    def addFloat(state: State, index: Int, elem: Float): State     = add(state, index, elem)
    def addDouble(state: State, index: Int, elem: Double): State   = add(state, index, elem)
    def addBoolean(state: State, index: Int, elem: Boolean): State = add(state, index, elem)

    def finish(state: State): Any

    /** Optional low-boxing finalizer: when non-null, a pooling decoder fills its
      * [[scalanotation.BuilderSlots]] with typed values instead of threading [[State]], and
      * finalizes via [[scalanotation.TypedFactory.fromSlots]].
      */
    def slotsFactory: TypedFactory | Null = null

  object TupleRead:
    final case class FromReaderBuilder[Repr, A](
        builder: Reader.TupleBuilder[Repr, A]
    ) extends TupleRead:
      type State = Repr

      override def slotsFactory: TypedFactory | Null = builder.slotsFactory

      def init(size: Int): State = builder.init(size)
      def initPooled(size: Int, pooled: scalanotation.BuilderSlots | Null): State =
        builder.initPooled(size, pooled)

      def add(state: State, index: Int, elem: Any): State =
        builder.add(state, index, elem)

      override def addString(state: State, index: Int, elem: String): State =
        builder.addString(state, index, elem)
      override def addChar(state: State, index: Int, elem: Char): State =
        builder.addChar(state, index, elem)
      override def addInt(state: State, index: Int, elem: Int): State =
        builder.addInt(state, index, elem)
      override def addLong(state: State, index: Int, elem: Long): State =
        builder.addLong(state, index, elem)
      override def addFloat(state: State, index: Int, elem: Float): State =
        builder.addFloat(state, index, elem)
      override def addDouble(state: State, index: Int, elem: Double): State =
        builder.addDouble(state, index, elem)
      override def addBoolean(state: State, index: Int, elem: Boolean): State =
        builder.addBoolean(state, index, elem)

      def finish(state: State): Any = builder.finish(state)

  trait TupleWrite:
    def size(value: Any): Int
    def elementValue(value: Any, index: Int): Any

  object TupleWrite:
    val productLike: TupleWrite = new:
      def size(value: Any): Int =
        value.asInstanceOf[Product].productArity

      def elementValue(value: Any, index: Int): Any =
        value.asInstanceOf[Product].productElement(index)

  trait SumWrite:
    def caseIndex(value: Any): Int

  object SumWrite:
    def from[T](select: T => Int): SumWrite = new:
      def caseIndex(value: Any): Int = select(value.asInstanceOf[T])

  trait VectorRead:
    type State
    def init(): State
    def add(state: State, elem: Any): State

    // typed adds: the defaults box and delegate to `add`
    def addString(state: State, elem: String): State   = add(state, elem)
    def addChar(state: State, elem: Char): State       = add(state, elem)
    def addInt(state: State, elem: Int): State         = add(state, elem)
    def addLong(state: State, elem: Long): State       = add(state, elem)
    def addFloat(state: State, elem: Float): State     = add(state, elem)
    def addDouble(state: State, elem: Double): State   = add(state, elem)
    def addBoolean(state: State, elem: Boolean): State = add(state, elem)

    def finish(state: State): Any

  object VectorRead:
    final case class FromReaderBuilder[Elem, Repr, A](
        builder: Reader.VectorBuilder[Elem, Repr, A]
    ) extends VectorRead:
      type State = Repr

      def init(): State = builder.init()

      def add(state: State, elem: Any): State =
        builder.add(state, elem.asInstanceOf[Elem])

      override def addString(state: State, elem: String): State =
        builder.addString(state, elem)
      override def addChar(state: State, elem: Char): State =
        builder.addChar(state, elem)
      override def addInt(state: State, elem: Int): State =
        builder.addInt(state, elem)
      override def addLong(state: State, elem: Long): State =
        builder.addLong(state, elem)
      override def addFloat(state: State, elem: Float): State =
        builder.addFloat(state, elem)
      override def addDouble(state: State, elem: Double): State =
        builder.addDouble(state, elem)
      override def addBoolean(state: State, elem: Boolean): State =
        builder.addBoolean(state, elem)

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

    // typed adds: the defaults box and delegate to `add`
    def addString(state: State, key: String, elem: String): State   = add(state, key, elem)
    def addChar(state: State, key: String, elem: Char): State       = add(state, key, elem)
    def addInt(state: State, key: String, elem: Int): State         = add(state, key, elem)
    def addLong(state: State, key: String, elem: Long): State       = add(state, key, elem)
    def addFloat(state: State, key: String, elem: Float): State     = add(state, key, elem)
    def addDouble(state: State, key: String, elem: Double): State   = add(state, key, elem)
    def addBoolean(state: State, key: String, elem: Boolean): State = add(state, key, elem)

    def finish(state: State): Any

  object DictRead:
    final case class FromReaderBuilder[Elem, Repr, A](
        builder: Reader.DictBuilder[Elem, Repr, A]
    ) extends DictRead:
      type State = Repr

      def init(): State = builder.init()

      def add(state: State, key: String, elem: Any): State =
        builder.add(state, key, elem.asInstanceOf[Elem])

      override def addString(state: State, key: String, elem: String): State =
        builder.addString(state, key, elem)
      override def addChar(state: State, key: String, elem: Char): State =
        builder.addChar(state, key, elem)
      override def addInt(state: State, key: String, elem: Int): State =
        builder.addInt(state, key, elem)
      override def addLong(state: State, key: String, elem: Long): State =
        builder.addLong(state, key, elem)
      override def addFloat(state: State, key: String, elem: Float): State =
        builder.addFloat(state, key, elem)
      override def addDouble(state: State, key: String, elem: Double): State =
        builder.addDouble(state, key, elem)
      override def addBoolean(state: State, key: String, elem: Boolean): State =
        builder.addBoolean(state, key, elem)

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
