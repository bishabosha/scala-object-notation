package scalanotation

import steps.result.Result
import scala.collection.immutable.SeqMap
import scalanotation.internal.ExprDecoder
import scalanotation.internal.ExprRenderer

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

  def render: String =
    ExprRenderer.renderExpr(this)

  def render(format: TextFormat): String =
    ExprRenderer.renderExpr(this, format)

  def renderPretty(indent: Int = 2): String =
    ExprRenderer.renderExpr(this, TextFormat.pretty(indent))

object Expr:
  final case class SourceFile[T](declarations: Map[String, T])
