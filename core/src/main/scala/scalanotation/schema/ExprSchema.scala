package scalanotation.schema

import scalanotation.Expr
import scalanotation.Reader

import scala.collection.mutable
import scalanotation.RouterSchema

object ExprSchema:

  val ExprRouterSchema: RawSchema[Expr] =
    val self = RawSchema.Ref("Expr", () => ExprSchema.ExprRouterSchema)
    RawSchema.Router(
      name = "Expr",
      selfKind = "any expression",
      IArray(
        RawSchema.RouterCase(
          "NamedTupleExpr",
          RawSchema.Dict(
            self,
            ExprNamedTupleRead,
            ExprNamedTupleWrite
          )
        ),
        RawSchema.RouterCase(
          "TupleExpr",
          RawSchema.TupleOf(
            self,
            ExprTupleRead,
            ExprTupleWrite
          )
        ),
        RawSchema.RouterCase(
          "VectorExpr",
          RawSchema.Vector(
            self,
            ExprVectorRead,
            ExprVectorWrite
          )
        ),
        RawSchema.RouterCase(
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
        RawSchema.RouterCase(
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
        RawSchema.RouterCase(
          "IntConstant",
          exprMappedIntPrimitive(
            Expr.IntConstant(_),
            {
              case Expr.IntConstant(value) => value
              case other                   => invalidExprRouterInput("IntConstant", other)
            }
          )
        ),
        RawSchema.RouterCase(
          "LongConstant",
          exprMappedLongPrimitive(
            Expr.LongConstant(_),
            {
              case Expr.LongConstant(value) => value
              case other                    => invalidExprRouterInput("LongConstant", other)
            }
          )
        ),
        RawSchema.RouterCase(
          "FloatConstant",
          exprMappedFloatPrimitive(
            Expr.FloatConstant(_),
            {
              case Expr.FloatConstant(value) => value
              case other                     => invalidExprRouterInput("FloatConstant", other)
            }
          )
        ),
        RawSchema.RouterCase(
          "DoubleConstant",
          exprMappedDoublePrimitive(
            Expr.DoubleConstant(_),
            {
              case Expr.DoubleConstant(value) => value
              case other                      => invalidExprRouterInput("DoubleConstant", other)
            }
          )
        ),
        RawSchema.RouterCase(
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
        RawSchema.RouterCase(
          "NullConstant",
          RawSchema.mapPureAndInput(RawSchema.Null)(
            resultMap0 = _ => Expr.NullConstant,
            inputMap0 = value =>
              value.asInstanceOf[Expr] match
                case Expr.NullConstant => null
                case other             => invalidExprRouterInput("NullConstant", other)
          )
        )
      ),
      RouterSchema.Router(
        ExprRouter.NamedTupleCase,
        ExprRouter.TupleCase,
        ExprRouter.VectorCase,
        ExprRouter.StringCase,
        ExprRouter.CharCase,
        ExprRouter.IntCase,
        ExprRouter.LongCase,
        ExprRouter.FloatCase,
        ExprRouter.DoubleCase,
        ExprRouter.BooleanCase,
        ExprRouter.NullCase,
        RawSchema.UnsupportedRouterCase,
        RawSchema.UnsupportedRouterCase
      ),
      ExprRouterWrite,
      RouterSchema.NumberMode.Bounded
    )
  end ExprRouterSchema

  private def invalidExprRouterInput(expected: String, value: Expr): Nothing =
    throw IllegalArgumentException(s"Expected Expr.$expected but found $value")

  private def exprMappedPrimitive[A](
      base: RawSchema[A],
      read: A => Expr,
      write: Expr => A
  ): RawSchema[Expr] =
    RawSchema.mapPureAndInput(base)(
      resultMap0 = value => read(value.asInstanceOf[A]),
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedIntPrimitive(
      read: Reader.IntMap[Expr],
      write: Expr => Int
  ): RawSchema[Expr] =
    RawSchema.mapIntTotalAndInput(RawSchema.Int)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedLongPrimitive(
      read: Reader.LongMap[Expr],
      write: Expr => Long
  ): RawSchema[Expr] =
    RawSchema.mapLongTotalAndInput(RawSchema.Long)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedFloatPrimitive(
      read: Reader.FloatMap[Expr],
      write: Expr => Float
  ): RawSchema[Expr] =
    RawSchema.mapFloatTotalAndInput(RawSchema.Float)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private def exprMappedDoublePrimitive(
      read: Reader.DoubleMap[Expr],
      write: Expr => Double
  ): RawSchema[Expr] =
    RawSchema.mapDoubleTotalAndInput(RawSchema.Double)(
      resultMap0 = read,
      inputMap0 = value => write(value.asInstanceOf[Expr])
    )

  private object ExprTupleRead
      extends Reader.VectorBuilder[Expr, mutable.ArrayBuffer[Expr], Expr.TupleExpr]:

    def init(): mutable.ArrayBuffer[Expr] =
      mutable.ArrayBuffer.empty[Expr]

    def add(state: mutable.ArrayBuffer[Expr], elem: Expr): mutable.ArrayBuffer[Expr] =
      state += elem
      state

    def finish(state: mutable.ArrayBuffer[Expr]): Expr.TupleExpr =
      Expr.TupleExpr(state.toIndexedSeq)

  private object ExprTupleWrite extends RawSchema.VectorWrite:
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

  private object ExprVectorWrite extends RawSchema.VectorWrite:
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

  private object ExprNamedTupleWrite extends RawSchema.DictWrite:
    def size(value: Any): Int =
      value.asInstanceOf[Expr] match
        case Expr.NamedTupleExpr(elements) => elements.length
        case other                         => invalidExprRouterInput("NamedTupleExpr", other)

    def iterator(value: Any): Iterator[(String, Any)] =
      value.asInstanceOf[Expr] match
        case Expr.NamedTupleExpr(elements) => elements.iterator.map((key, expr) => key -> expr)
        case other                         => invalidExprRouterInput("NamedTupleExpr", other)

  private object ExprRouter:
    inline val NamedTupleCase = 0
    inline val TupleCase      = 1
    inline val VectorCase     = 2
    inline val StringCase     = 3
    inline val CharCase       = 4
    inline val IntCase        = 5
    inline val LongCase       = 6
    inline val FloatCase      = 7
    inline val DoubleCase     = 8
    inline val BooleanCase    = 9
    inline val NullCase       = 10

  private object ExprRouterWrite extends RouterSchema.Write[Expr]:
    def caseIndex(router: RouterSchema.Router, value: Expr): RouterSchema.Index =
      if value == null then router.unsupportedIndex
      else RouterSchema.Index.fromInt(value.ordinal)
