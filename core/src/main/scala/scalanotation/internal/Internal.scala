package scalanotation.internal

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.constructorOnly
import scala.annotation.publicInBinary
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.boundary

@publicInBinary
private[internal] object Internal {

  final class LocalPool[T: Alloc as factory] extends Pool[T]:
    // non-atomic pull/push, but this should be single-threaded code
    private val pool = mutable.ArrayDeque.empty[T]
    def borrow(): T  =
      if pool.isEmpty then factory.alloc() else factory.prepare(pool.removeHead())
    def release(t: T): Unit =
      pool.prepend(t)

  trait Pool[T]:
    def borrow(): T
    def release(t: T): Unit
    inline def withBorrowed[A](inline f: T => A): A =
      val t = borrow()
      try f(t)
      finally release(t)

  class JumboNameSet:
    val underlying: mutable.HashSet[String] = mutable.HashSet.empty[String]
  object JumboNameSet:
    given Alloc[JumboNameSet]:
      def alloc(): JumboNameSet            = new JumboNameSet
      def prepare(t: JumboNameSet): t.type =
        t.clear()
        t

    given NameSet[JumboNameSet]:
      extension (seen: JumboNameSet)
        def alreadySeen(name: String): Boolean =
          !seen.underlying.add(name)

        def clear(): Unit = seen.underlying.clear()

  trait NameSet[T] {
    extension (seen: T)
      def alreadySeen(name: String): Boolean
      def clear(): Unit
  }

  trait Alloc[T] {
    def alloc(): T
    def prepare(t: T): t.type
  }

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

  private[internal] abstract class PoolHolder:
    // TODO: should think how this could scale to making a global shared object.
    private[internal] val namesPool = LocalPool[JumboNameSet]()

  /** a small abstraction around token iteration - but perhaps we would change to a fused char
    * reader with tokenizer.
    */
  private[internal] open class TokenStream[T: PublicInternal.HasDefault as default](
      @constructorOnly tokens: List[T]
  ) extends PoolHolder {
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

@publicInBinary
private[scalanotation] object PublicInternal {
  import scalanotation.Reader
  import quoted.{Expr as QExpr, *}

  val buildNamedTuple: (values: Array[AnyRef]) => AnyNamedTuple = values =>
    val asTuple = Tuple.fromIArray(IArray.unsafeFromArray(values))
    NamedTuple.build()(asTuple)

  class BuildVector[Elem]
      extends Reader.VectorBuilder[Elem, mutable.Builder[Elem, Vector[Elem]], Vector[Elem]]:
    def init(): mutable.Builder[Elem, Vector[Elem]] = Vector.newBuilder[Elem]
    def add(
        repr: mutable.Builder[Elem, Vector[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, Vector[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, Vector[Elem]]): Vector[Elem] = repr.result()

  class SeqFactoryVector[Elem, Col[X] <: scala.collection.Seq[X]](
      using factory: scala.collection.Factory[Elem, Col[Elem]]
  ) extends Reader.VectorBuilder[Elem, mutable.Builder[Elem, Col[Elem]], Col[Elem]]:
    def init(): mutable.Builder[Elem, Col[Elem]] = factory.newBuilder
    def add(
        repr: mutable.Builder[Elem, Col[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, Col[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, Col[Elem]]): Col[Elem] = repr.result()

  class MapFactoryDict[Elem, Col[X, Y] <: scala.collection.Map[X, Y]](
      using factory: scala.collection.Factory[(String, Elem), Col[String, Elem]]
  ) extends Reader.DictBuilder[Elem, mutable.Builder[(String, Elem), Col[String, Elem]], Col[
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
      extends Reader.VectorBuilder[Elem, mutable.Builder[Elem, Array[Elem]], Array[Elem]]:
    def init(): mutable.Builder[Elem, Array[Elem]] = Array.newBuilder[Elem]
    def add(
        repr: mutable.Builder[Elem, Array[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, Array[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, Array[Elem]]): Array[Elem] = repr.result()

  class BuildIArray[Elem: ClassTag]
      extends Reader.VectorBuilder[Elem, mutable.Builder[Elem, IArray[Elem]], IArray[Elem]]:
    def init(): mutable.Builder[Elem, IArray[Elem]] = IArray.newBuilder[Elem]
    def add(
        repr: mutable.Builder[Elem, IArray[Elem]],
        elem: Elem
    ): mutable.Builder[Elem, IArray[Elem]] =
      repr += elem
    def finish(repr: mutable.Builder[Elem, IArray[Elem]]): IArray[Elem] = repr.result()

  // TODO: add to standard library!
  inline def showType[T] = ${ showTypeImpl[T] }

  def showTypeImpl[T: Type](using Quotes): QExpr[String] =
    import quotes.reflect.*
    QExpr(Type.show[T])

  private object ArrProduct extends Product:
    private var arr: Array[AnyRef]                      = uninitialized
    def push(arr: Array[AnyRef])[A](f: Product => A): A =
      this.arr = arr
      try f(this)
      finally this.arr = null
    def productArity: Int            = arr.length
    def productElement(n: Int): Any  = arr(n)
    def canEqual(that: Any): Boolean = false
  private def arrAsProduct[A](f: Product => A)(arr: Array[AnyRef]): A =
    ArrProduct.push(arr)(f)

  def caseClassBuilder[T](using m: Mirror.ProductOf[T]): Array[AnyRef] => T =
    arrAsProduct(m.fromProduct)

  trait HasDefault[T] {
    def Default: T
  }
}
