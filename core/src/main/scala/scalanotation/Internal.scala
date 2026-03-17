package scalanotation

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.constructorOnly
import scala.annotation.publicInBinary
import scala.compiletime.uninitialized
import scala.deriving.Mirror
import scala.util.boundary
import scala.collection.mutable
import scala.reflect.ClassTag

@publicInBinary
private[scalanotation] object Internal {
  import quoted.{Expr as QExpr, *}

  import scala.util.boundary.Label
  inline def loop[A](inline body: Label[A] ?=> Unit): A = {
    boundary[A] {
      while true do body
      ???
    }
  }
  object loop {
    inline def break[A](a: A)(using Label[A]): A = boundary.break(a)
  }

  val buildNamedTuple: (values: Array[AnyRef]) => AnyNamedTuple = values =>
    val asTuple = Tuple.fromIArray(IArray.unsafeFromArray(values))
    NamedTuple.build()(asTuple)

  class BuildVector[Elem]
      extends Schema.VectorBuilder[Elem, mutable.Builder[Elem, Vector[Elem]], Vector[Elem]]:
    def init(): mutable.Builder[Elem, Vector[Elem]] = Vector.newBuilder[Elem]
    def add(
        repr: mutable.Builder[Elem, Vector[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, Vector[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, Vector[Elem]]): Vector[Elem] = repr.result()

  class SeqFactoryVector[Elem, Col[X] <: scala.collection.Seq[X]](
      using factory: scala.collection.Factory[Elem, Col[Elem]]
  ) extends Schema.VectorBuilder[Elem, mutable.Builder[Elem, Col[Elem]], Col[Elem]]:
    def init(): mutable.Builder[Elem, Col[Elem]] = factory.newBuilder
    def add(
        repr: mutable.Builder[Elem, Col[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, Col[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, Col[Elem]]): Col[Elem] = repr.result()
  class MapFactoryDict[Elem, Col[X, Y] <: scala.collection.Map[X, Y]](
      using factory: scala.collection.Factory[(String, Elem), Col[String, Elem]]
  ) extends Schema.DictBuilder[Elem, mutable.Builder[(String, Elem), Col[String, Elem]], Col[
        String,
        Elem
      ]]:
    def init(): mutable.Builder[(String, Elem), Col[String, Elem]] = factory.newBuilder
    def add(
        repr: mutable.Builder[(String, Elem), Col[String, Elem]],
        key: String,
        elem: Elem
    ): mutable.Builder[(String, Elem), Col[String, Elem]] =
      repr.addOne((key, elem))
    def finish(repr: mutable.Builder[(String, Elem), Col[String, Elem]]): Col[String, Elem] =
      repr.result()

  class BuildArray[Elem: ClassTag]
      extends Schema.VectorBuilder[Elem, mutable.Builder[Elem, Array[Elem]], Array[Elem]]:
    def init(): mutable.Builder[Elem, Array[Elem]] = Array.newBuilder[Elem]
    def add(
        repr: mutable.Builder[Elem, Array[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, Array[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, Array[Elem]]): Array[Elem] = repr.result()

  class BuildIArray[Elem: ClassTag]
      extends Schema.VectorBuilder[Elem, mutable.Builder[Elem, IArray[Elem]], IArray[Elem]]:
    def init(): mutable.Builder[Elem, IArray[Elem]] = IArray.newBuilder[Elem]
    def add(
        repr: mutable.Builder[Elem, IArray[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, IArray[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, IArray[Elem]]): IArray[Elem] = repr.result()

  // TODO: add to standard library!
  inline def showType[T] = ${ showTypeImpl[T] }

  private object ArrProduct extends Product:
    private var arr: Array[AnyRef]                      = uninitialized
    def push(arr: Array[AnyRef])[A](f: Product => A): A =
      this.arr = arr
      try f(this)
      finally this.arr = null
    def productArity: Int            = arr.length
    def productElement(n: Int): Any  = arr(n)
    def canEqual(that: Any): Boolean = false
  def arrAsProduct[A](f: Product => A)(arr: Array[AnyRef]): A =
    ArrProduct.push(arr)(f)
  def caseClassBuilder[T](using m: Mirror.ProductOf[T]): Array[AnyRef] => T =
    arrAsProduct(m.fromProduct)

  def showTypeImpl[T: Type](using Quotes): QExpr[String] =
    import quotes.reflect.*
    QExpr(Type.show[T])

  trait HasDefault[T] {
    def Default: T
  }

  /** a small abstraction around token iteration - but perhaps we would change to a fused char
    * reader with tokenizer.
    */
  private[scalanotation] class TokenStream[T: Internal.HasDefault as default](
      @constructorOnly tokens: List[T]
  ) {
    private var curr: T       = uninitialized
    private var rest: List[T] = tokens
    advance() // initialize curr and rest

    protected def currentToken(): T = curr

    protected def peekToken(): T =
      rest match
        case t :: _ => t
        case _      => default.Default

    protected def advance(): Unit =
      rest match
        case curr1 :: rest1 =>
          curr = curr1
          rest = rest1
        case _ =>
          curr = default.Default
          rest = Nil
  }
}
