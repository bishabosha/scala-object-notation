package scalanotation

import steps.result.Result
import scala.collection.immutable.SeqMap

final case class SourceFile[T](declaration: ValDecl[T])

object SourceFile:
  extension (sourceFile: SourceFile[Expr])
    def decodeValueAs[T: TaggedSchema as decoder](name: String): Result[T, DecodeError] =
      if sourceFile.declaration.name != name then
        Result.Err(DecodeError.UnexpectedRoot(sourceFile.declaration.name))
      else sourceFile.declaration.value.decodeAs[T]

final case class ValDecl[T](name: String, value: T)

enum Expr:
  case NamedTupleExpr(elements: IndexedSeq[(name: String, value: Expr)])
  case VectorExpr(elements: IndexedSeq[Expr])
  case StringConstant(value: String)
  case CharConstant(value: Char)
  case IntConstant(value: Int)
  case LongConstant(value: Long)
  case FloatConstant(value: Float)
  case DoubleConstant(value: Double)
  case BooleanConstant(value: Boolean)
  case NullConstant

object Expr:
  extension (expr: Expr)
    def decodeAs[T: TaggedSchema as decoder]: Result[T, DecodeError] =
      decoder.decode(expr)
