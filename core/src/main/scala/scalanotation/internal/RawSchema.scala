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
  case PairSeq(
      key: RawSchema,
      value: RawSchema,
      read: RawSchema.PairSeqRead | Null = null,
      write: RawSchema.PairSeqWrite | Null = null
  )
  case Dict(
      element: RawSchema,
      read: RawSchema.DictRead | Null = null,
      write: RawSchema.DictWrite | Null = null
  )
  case Router(
      name: String,
      selfKind: String,
      cases: IArray[RawSchema.RouterCase],
      read: RawSchema.RouterRead | Null,
      write: RawSchema.RouterWrite | Null,
      numberMode: RawSchema.RouterNumberMode
  )
  case Ref(name: String, target: () => RawSchema)
  case Option(inner: RawSchema)
  case Mapped(base: RawSchema, mapping: RawSchema.SchemaMapping)
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
      case _: RawSchema.PairSeq     => "Vector[(..., ...)]"
      case _: RawSchema.Dict        => "AnyNamedTuple"
      case router: RawSchema.Router => router.selfKind
      case RawSchema.Ref(name, _)   => name
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
  type ResultMap   = Any => Result[Any, DecodeError]
  type InputMap    = Any => Any
  type TupleRead   = Reader.TupleBuilder[?, ?]
  type VectorRead  = Reader.VectorBuilder[?, ?, ?]
  type DictRead    = Reader.DictBuilder[?, ?, ?]
  type PairSeqRead = Reader.PairSeqBuilder[
    ?,
    ?,
    ?,
    ?
  ]

  private final val UnsupportedRouterCase = -1

  def describeTupleSlots(size: Int): String =
    size match
      case 0 => "EmptyTuple"
      case 1 => "Tuple(...)"
      case _ => Iterator.fill(size)("...").mkString("(", ", ", ")")

  final case class SchemaMapping(
      resultMap: ResultMap | Null = null,
      inputMap: InputMap | Null = null,
      totalMaps: SchemaMapping.TotalMap = SchemaMapping.TotalMap.Empty
  ):

    def mapInput(value: Any): Any =
      val fn = inputMap
      if fn == null then value
      else fn(value)

    def withResultMap(f: ResultMap): SchemaMapping =
      copy(
        resultMap =
          if resultMap == null then f
          else value => resultMap(value).flatMap(f)
      )

    def withInputMap(f: InputMap): SchemaMapping =
      copy(
        inputMap =
          if inputMap == null then f
          else value => inputMap(f(value))
      )

    def withMapped(resultMap0: ResultMap, inputMap0: InputMap): SchemaMapping =
      withResultMap(resultMap0).withInputMap(inputMap0)

    def withPureMap(f: InputMap): SchemaMapping =
      if resultMap == null then
        copy(totalMaps = totalMaps match
          case SchemaMapping.TotalMap.Empty      => SchemaMapping.TotalMap.AnyMap(f)
          case SchemaMapping.TotalMap.IntMap(fn) =>
            SchemaMapping.TotalMap.IntMap(value => f(fn(value)))
          case SchemaMapping.TotalMap.LongMap(fn) =>
            SchemaMapping.TotalMap.LongMap(value => f(fn(value)))
          case SchemaMapping.TotalMap.FloatMap(fn) =>
            SchemaMapping.TotalMap.FloatMap(value => f(fn(value)))
          case SchemaMapping.TotalMap.DoubleMap(fn) =>
            SchemaMapping.TotalMap.DoubleMap(value => f(fn(value)))
          case SchemaMapping.TotalMap.AnyMap(fn) =>
            SchemaMapping.TotalMap.AnyMap(value => f(fn(value))))
      else
        copy(resultMap =
          value =>
            resultMap(value) match
              case Result.Ok(mapped)   => okAny(f(mapped))
              case err @ Result.Err(_) => err
        )

    def withIntMap(f: Reader.IntMap[Any]): SchemaMapping =
      copy(totalMaps = SchemaMapping.TotalMap.IntMap(f))

    def withLongMap(f: Reader.LongMap[Any]): SchemaMapping =
      copy(totalMaps = SchemaMapping.TotalMap.LongMap(f))

    def withFloatMap(f: Reader.FloatMap[Any]): SchemaMapping =
      copy(totalMaps = SchemaMapping.TotalMap.FloatMap(f))

    def withDoubleMap(f: Reader.DoubleMap[Any]): SchemaMapping =
      copy(totalMaps = SchemaMapping.TotalMap.DoubleMap(f))

    def withTotalMap(f: InputMap): SchemaMapping =
      copy(totalMaps = SchemaMapping.TotalMap.AnyMap(f))

  object SchemaMapping:
    enum TotalMap:
      case Empty
      case IntMap(fn: Reader.IntMap[Any])
      case LongMap(fn: Reader.LongMap[Any])
      case FloatMap(fn: Reader.FloatMap[Any])
      case DoubleMap(fn: Reader.DoubleMap[Any])
      case AnyMap(fn: InputMap)

      def isEmpty: Boolean = this eq TotalMap.Empty

    val empty: SchemaMapping = SchemaMapping()

    def apply(resultMap: ResultMap | Null, inputMap: InputMap | Null): SchemaMapping =
      new SchemaMapping(resultMap, inputMap)

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
    mapPureAndInput(base)(
      resultMap0 = value => read(value.asInstanceOf[A]),
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedIntPrimitive(
      read: Reader.IntMap[Any],
      write: Expr => Int
  ): RawSchema =
    mapIntTotalAndInput(RawSchema.Int)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedLongPrimitive(
      read: Reader.LongMap[Any],
      write: Expr => Long
  ): RawSchema =
    mapLongTotalAndInput(RawSchema.Long)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedFloatPrimitive(
      read: Reader.FloatMap[Any],
      write: Expr => Float
  ): RawSchema =
    mapFloatTotalAndInput(RawSchema.Float)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedDoublePrimitive(
      read: Reader.DoubleMap[Any],
      write: Expr => Double
  ): RawSchema =
    mapDoubleTotalAndInput(RawSchema.Double)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private object ExprTupleRead
      extends Reader.VectorBuilder[Expr, ArrayBuffer[Expr], Expr.TupleExpr]:

    def init(): ArrayBuffer[Expr] =
      ArrayBuffer.empty[Expr]

    def add(state: ArrayBuffer[Expr], elem: Expr): ArrayBuffer[Expr] =
      state += elem
      state

    def finish(state: ArrayBuffer[Expr]): Expr.TupleExpr =
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

  private object ExprVectorRead
      extends Reader.VectorBuilder[
        Expr,
        collection.mutable.Builder[Expr, IArray[Expr]],
        Expr.VectorExpr
      ]:

    def init(): collection.mutable.Builder[Expr, IArray[Expr]] =
      IArray.newBuilder[Expr]

    def add(
        state: collection.mutable.Builder[Expr, IArray[Expr]],
        elem: Expr
    ): collection.mutable.Builder[Expr, IArray[Expr]] =
      state.addOne(elem)

    def finish(state: collection.mutable.Builder[Expr, IArray[Expr]]): Expr.VectorExpr =
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

  private object ExprNamedTupleRead
      extends Reader.DictBuilder[
        Expr,
        collection.mutable.Builder[(String, Expr), IArray[(String, Expr)]],
        Expr.NamedTupleExpr
      ]:

    def init(): collection.mutable.Builder[(String, Expr), IArray[(String, Expr)]] =
      IArray.newBuilder[(String, Expr)]

    def add(
        state: collection.mutable.Builder[(String, Expr), IArray[(String, Expr)]],
        key: String,
        elem: Expr
    ): collection.mutable.Builder[(String, Expr), IArray[(String, Expr)]] =
      state.addOne((key, elem))

    def finish(
        state: collection.mutable.Builder[(String, Expr), IArray[(String, Expr)]]
    ): Expr.NamedTupleExpr =
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
      name = "Expr",
      selfKind = "any expression",
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
          mapPureAndInput(RawSchema.Null)(
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

  trait TupleWrite:
    def size(value: Any): Int
    def elementValue(value: Any, index: Int): Any

  object TupleWrite:
    val productLike: TupleWrite = new:
      def size(value: Any): Int =
        value.asInstanceOf[Product].productArity

      def elementValue(value: Any, index: Int): Any =
        value.asInstanceOf[Product].productElement(index)

    def from[A](size0: A => Int, elementValue0: (A, Int) => Any): TupleWrite = new:
      def size(value: Any): Int =
        size0(value.asInstanceOf[A])

      def elementValue(value: Any, index: Int): Any =
        elementValue0(value.asInstanceOf[A], index)

  trait SumWrite:
    def caseIndex(value: Any): Int

  object SumWrite:
    def from[T](select: T => Int): SumWrite = new:
      def caseIndex(value: Any): Int = select(value.asInstanceOf[T])

  trait VectorWrite:
    def size(value: Any): Int
    def iterator(value: Any): Iterator[Any]

  object VectorWrite:
    def from[A, Elem](size0: A => Int, iterator0: A => Iterator[Elem]): VectorWrite = new:
      def size(value: Any): Int = size0(value.asInstanceOf[A])

      def iterator(value: Any): Iterator[Any] =
        iterator0(value.asInstanceOf[A]).asInstanceOf[Iterator[Any]]

  trait PairSeqWrite:
    def size(value: Any): Int
    def iterator(value: Any): Iterator[(Any, Any)]

  object PairSeqWrite:
    def from[A, Key, Elem](size0: A => Int, iterator0: A => Iterator[(Key, Elem)]): PairSeqWrite =
      new:
        def size(value: Any): Int = size0(value.asInstanceOf[A])

        def iterator(value: Any): Iterator[(Any, Any)] =
          iterator0(value.asInstanceOf[A]).map((key, elem) => key -> elem.asInstanceOf[Any])

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

  def mapPure(
      base: RawSchema
  )(
      f: InputMap
  ): RawSchema =
    base.withMapping(_.withPureMap(f))

  def mapIntTotal(
      base: RawSchema
  )(
      resultMap0: Reader.IntMap[Any]
  ): RawSchema =
    base.withMapping(_.withIntMap(resultMap0))

  def mapLongTotal(
      base: RawSchema
  )(
      resultMap0: Reader.LongMap[Any]
  ): RawSchema =
    base.withMapping(_.withLongMap(resultMap0))

  def mapFloatTotal(
      base: RawSchema
  )(
      resultMap0: Reader.FloatMap[Any]
  ): RawSchema =
    base.withMapping(_.withFloatMap(resultMap0))

  def mapDoubleTotal(
      base: RawSchema
  )(
      resultMap0: Reader.DoubleMap[Any]
  ): RawSchema =
    base.withMapping(_.withDoubleMap(resultMap0))

  def mapResultAndInput(
      base: RawSchema
  )(
      resultMap0: ResultMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withMapped(resultMap0, inputMap0))

  def mapPureAndInput(
      base: RawSchema
  )(
      resultMap0: InputMap,
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withPureMap(resultMap0).withInputMap(inputMap0))

  private def mapIntTotalAndInput(
      base: RawSchema
  )(
      resultMap0: Reader.IntMap[Any],
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withIntMap(resultMap0).withInputMap(inputMap0))

  private def mapLongTotalAndInput(
      base: RawSchema
  )(
      resultMap0: Reader.LongMap[Any],
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withLongMap(resultMap0).withInputMap(inputMap0))

  private def mapFloatTotalAndInput(
      base: RawSchema
  )(
      resultMap0: Reader.FloatMap[Any],
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withFloatMap(resultMap0).withInputMap(inputMap0))

  private def mapDoubleTotalAndInput(
      base: RawSchema
  )(
      resultMap0: Reader.DoubleMap[Any],
      inputMap0: InputMap
  ): RawSchema =
    base.withMapping(_.withDoubleMap(resultMap0).withInputMap(inputMap0))
