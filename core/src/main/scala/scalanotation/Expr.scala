package scalanotation

import steps.result.Result
import scala.collection.immutable.SeqMap
import scalanotation.internal.ExprDecoder

enum Expr:
  case NamedTupleExpr private[scalanotation] (elements: IndexedSeq[(name: String, value: Expr)])
  case VectorExpr private[scalanotation] (elements: IndexedSeq[Expr])
  case StringConstant private[scalanotation] (value: String)
  case CharConstant private[scalanotation] (value: Char)
  case IntConstant private[scalanotation] (value: Int)
  case LongConstant private[scalanotation] (value: Long)
  case FloatConstant private[scalanotation] (value: Float)
  case DoubleConstant private[scalanotation] (value: Double)
  case BooleanConstant private[scalanotation] (value: Boolean)
  case NullConstant

  def decodeAs[T: Reader]: Result[T, DecodeError] =
    ExprDecoder.decodeExpr(this)

object Expr:
  final case class SourceFile[T](declarations: Map[String, T])
