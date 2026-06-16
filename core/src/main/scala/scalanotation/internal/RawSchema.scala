package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.TypedFactory
import scalanotation.internal.Internal
import steps.result.Result

import scala.collection.mutable.ArrayBuffer
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
  case TupleOf(
      element: RawSchema,
      read: RawSchema.VectorRead | Null = null,
      write: RawSchema.VectorWrite | Null = null
  )
  case Dict(
      element: RawSchema,
      read: RawSchema.DictRead | Null = null,
      write: RawSchema.DictWrite | Null = null
  )
  case Router(
      cases: IArray[RawSchema.RouterCase],
      read: RawSchema.RouterRead | Null,
      write: RawSchema.RouterWrite | Null,
      numberMode: RawSchema.RouterNumberMode
  )
  case Ref(name: String, target: () => RawSchema)
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
        case RawSchema.Ref(_, target) =>
          target().validateNamedTuple(pool).check
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
      case _: RawSchema.TupleOf     => "Tuple[...]"
      case _: RawSchema.Dict        => "AnyNamedTuple"
      case router: RawSchema.Router =>
        val cases = router.cases
        if cases.isEmpty then "Nothing"
        else cases.iterator.map(_.name).mkString("Router[", " | ", "]")
      case RawSchema.Ref(name, _)   => name
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
  @FunctionalInterface
  trait IntTotalMap:
    def apply(value: Int): Any

  @FunctionalInterface
  trait LongTotalMap:
    def apply(value: Long): Any

  @FunctionalInterface
  trait FloatTotalMap:
    def apply(value: Float): Any

  @FunctionalInterface
  trait DoubleTotalMap:
    def apply(value: Double): Any

  private final val UnsupportedRouterCase = -1

  def describeTupleSlots(size: Int): String =
    size match
      case 0 => "EmptyTuple"
      case 1 => "... *: EmptyTuple"
      case _ => Iterator.fill(size)("...").mkString("(", ", ", ")")

  final case class SchemaMapping(
      resultMap: ResultMap | Null = null,
      inputMap: InputMap | Null = null
  ):
    private var intTotalMap0: IntTotalMap | Null       = null
    private var longTotalMap0: LongTotalMap | Null     = null
    private var floatTotalMap0: FloatTotalMap | Null   = null
    private var doubleTotalMap0: DoubleTotalMap | Null = null
    private var totalMap0: InputMap | Null             = null

    def intTotalMap: IntTotalMap | Null = intTotalMap0

    def longTotalMap: LongTotalMap | Null = longTotalMap0

    def floatTotalMap: FloatTotalMap | Null = floatTotalMap0

    def doubleTotalMap: DoubleTotalMap | Null = doubleTotalMap0

    def totalMap: InputMap | Null = totalMap0

    def mapResult(result: Result[Any, DecodeError]): Result[Any, DecodeError] =
      if intTotalMap == null && longTotalMap == null && floatTotalMap == null && doubleTotalMap == null && totalMap == null
      then
        val fn = resultMap
        if fn == null then result
        else result.flatMap(fn)
      else
        result match
          case Result.Ok(value) =>
            var mappedValue = value
            var mapped      = false

            val intFn = intTotalMap
            if intFn != null && value.isInstanceOf[Int] then
              mappedValue = intFn(value.asInstanceOf[Int])
              mapped = true
            else
              val longFn = longTotalMap
              if longFn != null && value.isInstanceOf[Long] then
                mappedValue = longFn(value.asInstanceOf[Long])
                mapped = true
              else
                val floatFn = floatTotalMap
                if floatFn != null && value.isInstanceOf[Float] then
                  mappedValue = floatFn(value.asInstanceOf[Float])
                  mapped = true
                else
                  val doubleFn = doubleTotalMap
                  if doubleFn != null && value.isInstanceOf[Double] then
                    mappedValue = doubleFn(value.asInstanceOf[Double])
                    mapped = true

            if !mapped then
              val anyFn = totalMap
              if anyFn != null then
                mappedValue = anyFn(value)
                mapped = true

            val fn = resultMap
            if fn != null then fn(mappedValue)
            else if mapped then okAny(mappedValue)
            else result
          case err @ Result.Err(_) => err

    def mapInput(value: Any): Any =
      val fn = inputMap
      if fn == null then value
      else fn(value)

    private def copyWith(
        resultMap0: ResultMap | Null = resultMap,
        inputMap0: InputMap | Null = inputMap,
        intTotalMap1: IntTotalMap | Null = intTotalMap0,
        longTotalMap1: LongTotalMap | Null = longTotalMap0,
        floatTotalMap1: FloatTotalMap | Null = floatTotalMap0,
        doubleTotalMap1: DoubleTotalMap | Null = doubleTotalMap0,
        totalMap1: InputMap | Null = totalMap0
    ): SchemaMapping =
      val next = SchemaMapping(resultMap0, inputMap0)
      next.intTotalMap0 = intTotalMap1
      next.longTotalMap0 = longTotalMap1
      next.floatTotalMap0 = floatTotalMap1
      next.doubleTotalMap0 = doubleTotalMap1
      next.totalMap0 = totalMap1
      next

    def withResultMap(f: ResultMap): SchemaMapping =
      copyWith(
        resultMap0 =
          if resultMap == null then f
          else value => resultMap.nn(value).flatMap(f)
      )

    def withInputMap(f: InputMap): SchemaMapping =
      copyWith(
        inputMap0 =
          if inputMap == null then f
          else value => inputMap.nn(f(value))
      )

    def withMapped(resultMap0: ResultMap, inputMap0: InputMap): SchemaMapping =
      withResultMap(resultMap0).withInputMap(inputMap0)

    def withIntTotalMap(f: IntTotalMap): SchemaMapping =
      copyWith(intTotalMap1 = f)

    def withLongTotalMap(f: LongTotalMap): SchemaMapping =
      copyWith(longTotalMap1 = f)

    def withFloatTotalMap(f: FloatTotalMap): SchemaMapping =
      copyWith(floatTotalMap1 = f)

    def withDoubleTotalMap(f: DoubleTotalMap): SchemaMapping =
      copyWith(doubleTotalMap1 = f)

    def withTotalMap(f: InputMap): SchemaMapping =
      copyWith(totalMap1 = f)

  object SchemaMapping:
    val empty: SchemaMapping = SchemaMapping()

  final class Key[T]()
  val IsValidNamedTupleSchema: Key[Result[Unit, DecodeError]] = Key()

  final case class Field(name: String, schema: RawSchema)

  final case class SumCase(name: String, schema: RawSchema)

  final case class RouterCase(name: String, schema: RawSchema)

  enum RouterConstruct:
    case Record
    case Tuple
    case Vector
    case String
    case Char
    case Int
    case Long
    case Float
    case Double
    case Boolean
    case Null
    case RawNumber

  enum RouterNumberMode:
    case Bounded
    case Raw

  trait RouterRead:
    final def route(construct: RouterConstruct): Int =
      construct match
        case RouterConstruct.Record    => onRecord()
        case RouterConstruct.Tuple     => onTuple()
        case RouterConstruct.Vector    => onVector()
        case RouterConstruct.String    => onString()
        case RouterConstruct.Char      => onChar()
        case RouterConstruct.Int       => onInt()
        case RouterConstruct.Long      => onLong()
        case RouterConstruct.Float     => onFloat()
        case RouterConstruct.Double    => onDouble()
        case RouterConstruct.Boolean   => onBoolean()
        case RouterConstruct.Null      => onNull()
        case RouterConstruct.RawNumber => onRawNumber()

    def onRecord(): Int    = UnsupportedRouterCase
    def onTuple(): Int     = UnsupportedRouterCase
    def onVector(): Int    = UnsupportedRouterCase
    def onString(): Int    = UnsupportedRouterCase
    def onChar(): Int      = UnsupportedRouterCase
    def onInt(): Int       = UnsupportedRouterCase
    def onLong(): Int      = UnsupportedRouterCase
    def onFloat(): Int     = UnsupportedRouterCase
    def onDouble(): Int    = UnsupportedRouterCase
    def onBoolean(): Int   = UnsupportedRouterCase
    def onNull(): Int      = UnsupportedRouterCase
    def onRawNumber(): Int = UnsupportedRouterCase

  trait RouterWrite:
    def caseIndex(value: Any): Int

  /** linear scan for the case named `name` — unlike `cases.iterator.find`, allocates no iterator,
    * closure or `Some` on the decode hot path
    */
  def findCase(cases: IArray[SumCase], name: String): SumCase | Null =
    var i = 0
    while i < cases.length do
      val sumCase = cases(i)
      if sumCase.name == name then return sumCase
      i += 1
    null

  private def invalidExprRouterInput(expected: String, value: Expr): Nothing =
    throw IllegalArgumentException(s"Expected Expr.$expected but found $value")

  private def okAny(value: Any): Result[Any, DecodeError] =
    Result.Ok(value).asInstanceOf[Result[Any, DecodeError]]

  private def exprMappedPrimitive[A](
      base: RawSchema,
      read: A => Expr,
      write: Expr => A
  ): RawSchema =
    mapTotalAndInput(base)(
      resultMap0 = value => read(value.asInstanceOf[A]),
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedIntPrimitive(
      read: IntTotalMap,
      write: Expr => Int
  ): RawSchema =
    mapIntTotalAndInput(RawSchema.Int)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedLongPrimitive(
      read: LongTotalMap,
      write: Expr => Long
  ): RawSchema =
    mapLongTotalAndInput(RawSchema.Long)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedFloatPrimitive(
      read: FloatTotalMap,
      write: Expr => Float
  ): RawSchema =
    mapFloatTotalAndInput(RawSchema.Float)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedDoublePrimitive(
      read: DoubleTotalMap,
      write: Expr => Double
  ): RawSchema =
    mapDoubleTotalAndInput(RawSchema.Double)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private object ExprTupleRead extends VectorRead:
    type State = ArrayBuffer[Expr]

    def init(): State =
      ArrayBuffer.empty[Expr]

    def add(state: State, elem: Any): State =
      state += elem.asInstanceOf[Expr]
      state

    def finish(state: State): Any =
      Expr.TupleExpr(state.toIndexedSeq)

  private object ExprTupleWrite extends VectorWrite:
    def size(value: Any): Int =
      value.asInstanceOf[Expr] match
        case Expr.TupleExpr(elements) => elements.length
        case other                    => invalidExprRouterInput("TupleExpr", other)

    def iterator(value: Any): Iterator[Any] =
      value.asInstanceOf[Expr] match
        case Expr.TupleExpr(elements) => elements.iterator
        case other                    => invalidExprRouterInput("TupleExpr", other)

  private object ExprVectorRead extends VectorRead:
    type State = collection.mutable.Builder[Expr, IArray[Expr]]

    def init(): State =
      IArray.newBuilder[Expr]

    def add(state: State, elem: Any): State =
      state.addOne(elem.asInstanceOf[Expr])

    def finish(state: State): Any =
      Expr.VectorExpr(state.result())

  private object ExprVectorWrite extends VectorWrite:
    def size(value: Any): Int =
      value.asInstanceOf[Expr] match
        case Expr.VectorExpr(elements) => elements.length
        case other                     => invalidExprRouterInput("VectorExpr", other)

    def iterator(value: Any): Iterator[Any] =
      value.asInstanceOf[Expr] match
        case Expr.VectorExpr(elements) => elements.iterator
        case other                     => invalidExprRouterInput("VectorExpr", other)

  private object ExprNamedTupleRead extends DictRead:
    type State = collection.mutable.Builder[(String, Expr), IArray[(String, Expr)]]

    def init(): State =
      IArray.newBuilder[(String, Expr)]

    def add(state: State, key: String, elem: Any): State =
      state.addOne((key, elem.asInstanceOf[Expr]))

    def finish(state: State): Any =
      Expr.NamedTupleExpr(state.result())

  private object ExprNamedTupleWrite extends DictWrite:
    def size(value: Any): Int =
      value.asInstanceOf[Expr] match
        case Expr.NamedTupleExpr(elements) => elements.length
        case other                         => invalidExprRouterInput("NamedTupleExpr", other)

    def iterator(value: Any): Iterator[(String, Any)] =
      value.asInstanceOf[Expr] match
        case Expr.NamedTupleExpr(elements) => elements.iterator.map((key, expr) => key -> expr)
        case other                         => invalidExprRouterInput("NamedTupleExpr", other)

  object ExprRouter:
    final val NamedTupleCase = 0
    final val TupleCase      = 1
    final val VectorCase     = 2
    final val StringCase     = 3
    final val CharCase       = 4
    final val IntCase        = 5
    final val LongCase       = 6
    final val FloatCase      = 7
    final val DoubleCase     = 8
    final val BooleanCase    = 9
    final val NullCase       = 10

  private object ExprRouterRead extends RouterRead:
    override def onRecord(): Int  = ExprRouter.NamedTupleCase
    override def onTuple(): Int   = ExprRouter.TupleCase
    override def onVector(): Int  = ExprRouter.VectorCase
    override def onString(): Int  = ExprRouter.StringCase
    override def onChar(): Int    = ExprRouter.CharCase
    override def onInt(): Int     = ExprRouter.IntCase
    override def onLong(): Int    = ExprRouter.LongCase
    override def onFloat(): Int   = ExprRouter.FloatCase
    override def onDouble(): Int  = ExprRouter.DoubleCase
    override def onBoolean(): Int = ExprRouter.BooleanCase
    override def onNull(): Int    = ExprRouter.NullCase

  private object ExprRouterWrite extends RouterWrite:
    def caseIndex(value: Any): Int =
      value match
        case Expr.NamedTupleExpr(_)  => ExprRouter.NamedTupleCase
        case Expr.TupleExpr(_)       => ExprRouter.TupleCase
        case Expr.VectorExpr(_)      => ExprRouter.VectorCase
        case Expr.StringConstant(_)  => ExprRouter.StringCase
        case Expr.CharConstant(_)    => ExprRouter.CharCase
        case Expr.IntConstant(_)     => ExprRouter.IntCase
        case Expr.LongConstant(_)    => ExprRouter.LongCase
        case Expr.FloatConstant(_)   => ExprRouter.FloatCase
        case Expr.DoubleConstant(_)  => ExprRouter.DoubleCase
        case Expr.BooleanConstant(_) =>
          ExprRouter.BooleanCase
        case Expr.NullConstant => ExprRouter.NullCase
        case _                 => UnsupportedRouterCase

  lazy val ExprRouterSchema: RawSchema =
    lazy val self: RawSchema = RawSchema.Ref("Expr", () => ExprRouterSchema)
    RawSchema.Router(
      IArray(
        RouterCase(
          "NamedTupleExpr",
          RawSchema.Dict(
            self,
            ExprNamedTupleRead,
            ExprNamedTupleWrite
          )
        ),
        RouterCase(
          "TupleExpr",
          RawSchema.TupleOf(
            self,
            ExprTupleRead,
            ExprTupleWrite
          )
        ),
        RouterCase(
          "VectorExpr",
          RawSchema.Vector(
            self,
            ExprVectorRead,
            ExprVectorWrite
          )
        ),
        RouterCase(
          "StringConstant",
          exprMappedPrimitive[String](
            RawSchema.String,
            Expr.StringConstant(_),
            {
              case Expr.StringConstant(value) => value
              case other                      => invalidExprRouterInput("StringConstant", other)
            }
          )
        ),
        RouterCase(
          "CharConstant",
          exprMappedPrimitive[Char](
            RawSchema.Char,
            Expr.CharConstant(_),
            {
              case Expr.CharConstant(value) => value
              case other                    => invalidExprRouterInput("CharConstant", other)
            }
          )
        ),
        RouterCase(
          "IntConstant",
          exprMappedIntPrimitive(
            Expr.IntConstant(_),
            {
              case Expr.IntConstant(value) => value
              case other                   => invalidExprRouterInput("IntConstant", other)
            }
          )
        ),
        RouterCase(
          "LongConstant",
          exprMappedLongPrimitive(
            Expr.LongConstant(_),
            {
              case Expr.LongConstant(value) => value
              case other                    => invalidExprRouterInput("LongConstant", other)
            }
          )
        ),
        RouterCase(
          "FloatConstant",
          exprMappedFloatPrimitive(
            Expr.FloatConstant(_),
            {
              case Expr.FloatConstant(value) => value
              case other                     => invalidExprRouterInput("FloatConstant", other)
            }
          )
        ),
        RouterCase(
          "DoubleConstant",
          exprMappedDoublePrimitive(
            Expr.DoubleConstant(_),
            {
              case Expr.DoubleConstant(value) => value
              case other                      => invalidExprRouterInput("DoubleConstant", other)
            }
          )
        ),
        RouterCase(
          "BooleanConstant",
          exprMappedPrimitive[Boolean](
            RawSchema.Boolean,
            Expr.BooleanConstant(_),
            {
              case Expr.BooleanConstant(value) => value
              case other                       => invalidExprRouterInput("BooleanConstant", other)
            }
          )
        ),
        RouterCase(
          "NullConstant",
          mapTotalAndInput(RawSchema.Null)(
            resultMap0 = _ => Expr.NullConstant,
            inputMap0 = value =>
              value.asInstanceOf[Expr] match
                case Expr.NullConstant => null
                case other             => invalidExprRouterInput("NullConstant", other)
          )
        )
      ),
      ExprRouterRead,
      ExprRouterWrite,
      RouterNumberMode.Bounded
    )

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
      * finalize via [[scalanotation.TypedFactory.OfProduct.fromSlots]].
      */
    def slotsFactory: TypedFactory.OfProduct[?] | Null = null

  object NamedTupleRead:
    def from[T](build0: Array[AnyRef] => T): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any = build0(values)

    def from[T](
        build0: Array[AnyRef] => T,
        slotsFactory0: TypedFactory.OfProduct[?] | Null
    ): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any                       = build0(values)
      override def slotsFactory: TypedFactory.OfProduct[?] | Null = slotsFactory0

    /** attaches (or replaces) a [[scalanotation.TypedFactory]] on an existing read */
    def withSlotsFactory(read: NamedTupleRead, factory: TypedFactory.OfProduct[?]): NamedTupleRead =
      new:
        def build(values: Array[AnyRef]): Any                       = read.build(values)
        override def slotsFactory: TypedFactory.OfProduct[?] | Null = factory

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
    def initPooled(size: Int, pooled: scalanotation.BuilderSlots | Null): State =
      init(size)
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
      * finalizes via [[scalanotation.TypedFactory.OfProduct.fromSlots]].
      */
    def slotsFactory: TypedFactory.OfProduct[?] | Null = null

  object TupleRead:
    final case class FromReaderBuilder[Repr, A](
        builder: Reader.TupleBuilder[Repr, A]
    ) extends TupleRead:
      type State = Repr

      override def slotsFactory: TypedFactory.OfProduct[?] | Null = builder.slotsFactory

      def init(size: Int): State = builder.init(size)
      override def initPooled(size: Int, pooled: scalanotation.BuilderSlots | Null): State =
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

  private def mapTotalAndInput(
      base: RawSchema
  )(
      resultMap0: InputMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withTotalMap(resultMap0).withInputMap(inputMap0))

  private def mapIntTotalAndInput(
      base: RawSchema
  )(
      resultMap0: IntTotalMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withIntTotalMap(resultMap0).withInputMap(inputMap0))

  private def mapLongTotalAndInput(
      base: RawSchema
  )(
      resultMap0: LongTotalMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withLongTotalMap(resultMap0).withInputMap(inputMap0))

  private def mapFloatTotalAndInput(
      base: RawSchema
  )(
      resultMap0: FloatTotalMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withFloatTotalMap(resultMap0).withInputMap(inputMap0))

  private def mapDoubleTotalAndInput(
      base: RawSchema
  )(
      resultMap0: DoubleTotalMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withDoubleTotalMap(resultMap0).withInputMap(inputMap0))
