package scalanotation

final case class SourceFile[T](declaration: ValDecl[T])

final case class ValDecl[T](name: String, value: T)

opaque type TupleOf[T] = Tuple
object TupleOf:
  def apply[T](data: IArray[T]): TupleOf[T] = Tuple.fromIArray(data)
  def unapplySeq[T](ts: TupleOf[T]): UnapplySeqWrapper[T] = UnapplySeqWrapper(ts)

  final class UnapplySeqWrapper[T](private val a: TupleOf[T]) extends AnyVal {
    def isEmpty: false = false
    def get: UnapplySeqWrapper[T] = this
    def lengthCompare(len: Int): Int = a.productArity.compareTo(len)
    def apply(i: Int): T = a.productElement(i).asInstanceOf[T]
    def drop(n: Int): scala.Seq[T] = toSeq.drop(n)
    def toSeq: scala.Seq[T] = (a.toIArray: scala.Seq[Object]).asInstanceOf[scala.Seq[T]] // clones the array
  }

enum Expr:
  case NamedTupleExpr(names: IArray[String], elements: IArray[Expr])
  case VectorExpr(elements: IArray[Expr])
  case StringConstant(value: String)
  case CharConstant(value: Char)
  case IntConstant(value: Int)
  case LongConstant(value: Long)
  case FloatConstant(value: Float)
  case DoubleConstant(value: Double)
  case BooleanConstant(value: Boolean)
  case NullConstant
