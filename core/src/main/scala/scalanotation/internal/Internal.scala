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

  /** A lock-free, fixed-capacity pool that can be shared between threads — including virtual
    * threads, where a ThreadLocal cache would never be reused. Borrow and release probe the slot
    * array with CAS starting from a per-thread index, so an uncontended borrow/release pair is one
    * volatile read plus one CAS each. An empty pool allocates a fresh instance; a full pool drops
    * the released instance for the GC. The CAS pair also safely publishes the pooled instance's
    * state between threads.
    */
  final class SharedPool[T <: AnyRef: Alloc as factory](capacityHint: Int) extends Pool[T]:
    val capacity: Int = Integer.highestOneBit(math.max(capacityHint, 2))
    private val mask  = capacity - 1
    private val slots = java.util.concurrent.atomic.AtomicReferenceArray[T](capacity)

    private def probeStart: Int =
      System.identityHashCode(Thread.currentThread()) & mask

    def borrow(): T =
      var i = probeStart
      var n = 0
      while n < capacity do
        val t = slots.get(i)
        if t != null && slots.compareAndSet(i, t, null.asInstanceOf[T]) then
          return factory.prepare(t)
        i = (i + 1) & mask
        n += 1
      factory.alloc()

    def release(t: T): Unit =
      var i = probeStart
      var n = 0
      while n < capacity do
        if slots.get(i) == null && slots.compareAndSet(i, null.asInstanceOf[T], t) then return
        i = (i + 1) & mask
        n += 1

  class JumboNameSet:
    @deprecated("Kept for binary compatibility; will be removed in a future version", "0.3.6")
    @publicInBinary
    private[JumboNameSet] def underlying: mutable.HashSet[String] =
      mutable.HashSet.from(underlying0.keysIterator)

    private val underlying0: mutable.AnyRefMap[String, Unit] = mutable.AnyRefMap.empty[String, Unit]

  object JumboNameSet:
    given Alloc[JumboNameSet]:
      def alloc(): JumboNameSet            = new JumboNameSet
      def prepare(t: JumboNameSet): t.type =
        t.clear()
        t

    given NameSet[JumboNameSet]:
      extension (seen: JumboNameSet)
        def alreadySeen(name: String): Boolean =
          val map = seen.underlying0
          val sizeBefore = map.size
          map.addOne(name, ()).size == sizeBefore

        def clear(): Unit = seen.underlying0.clear()

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

  /** Retained only for binary compatibility — superseded by the bounded, slot-based
    * [[scalanotation.internal.TokenStream]].
    */
  @deprecated("superseded by scalanotation.internal.TokenStream", "0.3.6")
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

    protected def currentAndRest: List[T] =
      curr :: rest

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

  class BuildTuple[A <: Tuple] extends Reader.TupleBuilder[Array[AnyRef], A]:
    def init(size: Int): Array[AnyRef] =
      new Array[AnyRef](size)

    def add(repr: Array[AnyRef], index: Int, elem: Any): Array[AnyRef] =
      repr(index) = elem.asInstanceOf[AnyRef]
      repr

    def finish(repr: Array[AnyRef]): A =
      Tuple.fromIArray(IArray.unsafeFromArray(repr)).asInstanceOf[A]

  object BuildTupleSlots:
    type State = scalanotation.BuilderSlots | Array[AnyRef]

  class BuildTupleSlots[A <: Tuple] extends Reader.TupleBuilder[BuildTupleSlots.State, A]:

    def init(size: Int): BuildTupleSlots.State =
      new Array[AnyRef](size)

    override def initPooled(
        size: Int,
        pooled: scalanotation.BuilderSlots | Null
    ): BuildTupleSlots.State =
      if pooled != null then pooled.reset(size)
      else new Array[AnyRef](size)

    def add(repr: BuildTupleSlots.State, index: Int, elem: Any): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setRef(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem.asInstanceOf[AnyRef]
      repr

    override def addString(
        repr: BuildTupleSlots.State,
        index: Int,
        elem: String
    ): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setString(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem
      repr

    override def addChar(
        repr: BuildTupleSlots.State,
        index: Int,
        elem: Char
    ): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setChar(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem.asInstanceOf[AnyRef]
      repr

    override def addInt(
        repr: BuildTupleSlots.State,
        index: Int,
        elem: Int
    ): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setInt(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem.asInstanceOf[AnyRef]
      repr

    override def addLong(
        repr: BuildTupleSlots.State,
        index: Int,
        elem: Long
    ): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setLong(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem.asInstanceOf[AnyRef]
      repr

    override def addFloat(
        repr: BuildTupleSlots.State,
        index: Int,
        elem: Float
    ): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setFloat(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem.asInstanceOf[AnyRef]
      repr

    override def addDouble(
        repr: BuildTupleSlots.State,
        index: Int,
        elem: Double
    ): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setDouble(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem.asInstanceOf[AnyRef]
      repr

    override def addBoolean(
        repr: BuildTupleSlots.State,
        index: Int,
        elem: Boolean
    ): BuildTupleSlots.State =
      repr match
        case repr: scalanotation.BuilderSlots =>
          repr.setBoolean(index, elem)
        case arr: Array[AnyRef] =>
          arr(index) = elem.asInstanceOf[AnyRef]
      repr

    def finish(repr: BuildTupleSlots.State): A =
      repr match
        case repr: scalanotation.BuilderSlots =>
          slotsFactory.fromSlots(repr)
        case arr: Array[AnyRef] =>
          Tuple.fromIArray(IArray.unsafeFromArray(arr)).asInstanceOf[A]

    override def slotsFactory: scalanotation.TypedFactory.OfProduct[A] =
      tupleSlotsFactory.asInstanceOf[scalanotation.TypedFactory.OfProduct[A]]

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

  // specialized array builders: the typed `addX` override receives the unboxed primitive from the
  // decoder's typed slot and appends it directly to the unboxed array builder, so an element is
  // never boxed at any point
  class BuildIntArray extends Reader.VectorBuilder[Int, mutable.ArrayBuilder.ofInt, Array[Int]]:
    def init(): mutable.ArrayBuilder.ofInt = new mutable.ArrayBuilder.ofInt
    def add(repr: mutable.ArrayBuilder.ofInt, elem: Int): mutable.ArrayBuilder.ofInt =
      addInt(repr, elem)
    override def addInt(repr: mutable.ArrayBuilder.ofInt, elem: Int): mutable.ArrayBuilder.ofInt =
      repr.addOne(elem)
      repr
    def finish(repr: mutable.ArrayBuilder.ofInt): Array[Int] = repr.result()

  class BuildLongArray extends Reader.VectorBuilder[Long, mutable.ArrayBuilder.ofLong, Array[Long]]:
    def init(): mutable.ArrayBuilder.ofLong = new mutable.ArrayBuilder.ofLong
    def add(repr: mutable.ArrayBuilder.ofLong, elem: Long): mutable.ArrayBuilder.ofLong =
      addLong(repr, elem)
    override def addLong(
        repr: mutable.ArrayBuilder.ofLong,
        elem: Long
    ): mutable.ArrayBuilder.ofLong =
      repr.addOne(elem)
      repr
    def finish(repr: mutable.ArrayBuilder.ofLong): Array[Long] = repr.result()

  class BuildFloatArray
      extends Reader.VectorBuilder[Float, mutable.ArrayBuilder.ofFloat, Array[Float]]:
    def init(): mutable.ArrayBuilder.ofFloat = new mutable.ArrayBuilder.ofFloat
    def add(repr: mutable.ArrayBuilder.ofFloat, elem: Float): mutable.ArrayBuilder.ofFloat =
      addFloat(repr, elem)
    override def addFloat(
        repr: mutable.ArrayBuilder.ofFloat,
        elem: Float
    ): mutable.ArrayBuilder.ofFloat =
      repr.addOne(elem)
      repr
    def finish(repr: mutable.ArrayBuilder.ofFloat): Array[Float] = repr.result()

  class BuildDoubleArray
      extends Reader.VectorBuilder[Double, mutable.ArrayBuilder.ofDouble, Array[Double]]:
    def init(): mutable.ArrayBuilder.ofDouble = new mutable.ArrayBuilder.ofDouble
    def add(repr: mutable.ArrayBuilder.ofDouble, elem: Double): mutable.ArrayBuilder.ofDouble =
      addDouble(repr, elem)
    override def addDouble(
        repr: mutable.ArrayBuilder.ofDouble,
        elem: Double
    ): mutable.ArrayBuilder.ofDouble =
      repr.addOne(elem)
      repr
    def finish(repr: mutable.ArrayBuilder.ofDouble): Array[Double] = repr.result()

  class BuildBooleanArray
      extends Reader.VectorBuilder[Boolean, mutable.ArrayBuilder.ofBoolean, Array[Boolean]]:
    def init(): mutable.ArrayBuilder.ofBoolean = new mutable.ArrayBuilder.ofBoolean
    def add(repr: mutable.ArrayBuilder.ofBoolean, elem: Boolean): mutable.ArrayBuilder.ofBoolean =
      addBoolean(repr, elem)
    override def addBoolean(
        repr: mutable.ArrayBuilder.ofBoolean,
        elem: Boolean
    ): mutable.ArrayBuilder.ofBoolean =
      repr.addOne(elem)
      repr
    def finish(repr: mutable.ArrayBuilder.ofBoolean): Array[Boolean] = repr.result()

  class BuildCharArray extends Reader.VectorBuilder[Char, mutable.ArrayBuilder.ofChar, Array[Char]]:
    def init(): mutable.ArrayBuilder.ofChar = new mutable.ArrayBuilder.ofChar
    def add(repr: mutable.ArrayBuilder.ofChar, elem: Char): mutable.ArrayBuilder.ofChar =
      addChar(repr, elem)
    override def addChar(
        repr: mutable.ArrayBuilder.ofChar,
        elem: Char
    ): mutable.ArrayBuilder.ofChar =
      repr.addOne(elem)
      repr
    def finish(repr: mutable.ArrayBuilder.ofChar): Array[Char] = repr.result()

  /** vector read for `Array[T]`, specialized to append without boxing when `T` is primitive */
  def arrayVectorRead[T](using tag: ClassTag[T]): RawSchema.VectorRead =
    tag.runtimeClass match
      case java.lang.Integer.TYPE   => RawSchema.VectorRead.FromReaderBuilder(BuildIntArray())
      case java.lang.Long.TYPE      => RawSchema.VectorRead.FromReaderBuilder(BuildLongArray())
      case java.lang.Float.TYPE     => RawSchema.VectorRead.FromReaderBuilder(BuildFloatArray())
      case java.lang.Double.TYPE    => RawSchema.VectorRead.FromReaderBuilder(BuildDoubleArray())
      case java.lang.Boolean.TYPE   => RawSchema.VectorRead.FromReaderBuilder(BuildBooleanArray())
      case java.lang.Character.TYPE => RawSchema.VectorRead.FromReaderBuilder(BuildCharArray())
      case _                        => RawSchema.VectorRead.FromReaderBuilder(BuildArray[T]())

  /** vector read for `IArray[T]`: a specialized array builder's result is freshly allocated, so for
    * primitive elements it can be safely viewed as the immutable array
    */
  def iarrayVectorRead[T](using tag: ClassTag[T]): RawSchema.VectorRead =
    if tag.runtimeClass.isPrimitive then arrayVectorRead[T]
    else RawSchema.VectorRead.FromReaderBuilder(BuildIArray[T]())

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

  /** finalizes a derived case class directly from pooled builder slots:
    * [[scalanotation.BuilderSlots]] is itself a [[Product]] over the typed slots, so no boxed
    * values array is allocated
    */
  def caseClassSlotsFactory[T](
      using m: Mirror.ProductOf[T]
  ): scalanotation.TypedFactory.OfProduct[T] =
    slots => m.fromProduct(slots)

  /** Finalizes a tuple (and named tuples, which are tuples at runtime) from pooled builder slots:
    * [[scalanotation.BuilderSlots]] is itself a [[Product]], so `Tuple.fromProduct` consumes the
    * typed slots directly without materializing a boxed values array first.
    */
  val tupleSlotsFactory: scalanotation.TypedFactory.OfProduct[Tuple] =
    slots => Tuple.fromProduct(slots)

  /** finalizes a named tuple from pooled builder slots — see [[tupleSlotsFactory]] */
  val namedTupleSlotsFactory: scalanotation.TypedFactory.OfProduct[Tuple] =
    tupleSlotsFactory

  trait HasDefault[T] {
    def Default: T
  }
}
