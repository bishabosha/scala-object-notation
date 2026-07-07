package scalanotation.internal

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.publicInBinary
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.boundary
import scala.annotation.threadUnsafe
import BuilderSlotsPool.given
import scalanotation.schema.RawSchema

@publicInBinary
private[internal] object Internal {

  final class LocalPool[T: Alloc as factory] extends PublicInternal.Pool[T]:
    // non-atomic pull/push, but this should be single-threaded code
    private val pool = mutable.ArrayDeque.empty[T]
    def borrow(): T  =
      if pool.isEmpty then factory.alloc() else factory.prepare(pool.removeHead())
    def release(t: T): Unit =
      pool.prepend(t)

  /** A lock-free, fixed-capacity pool that can be shared between threads — including virtual
    * threads, where a ThreadLocal cache would never be reused. Borrow and release probe the slot
    * array with CAS starting from a per-thread index, so an uncontended borrow/release pair is one
    * volatile read plus one CAS each. An empty pool allocates a fresh instance; a full pool drops
    * the released instance for the GC. The CAS pair also safely publishes the pooled instance's
    * state between threads.
    */
  final class SharedPool[T <: AnyRef: Alloc as factory](capacityHint: Int)
      extends PublicInternal.Pool[T]:
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
    private val underlying0: mutable.AnyRefMap[String, Unit] = mutable.AnyRefMap.empty[String, Unit]

  object JumboNameSet:
    given Alloc[JumboNameSet]:
      def alloc(): JumboNameSet            = new JumboNameSet
      def prepare(t: JumboNameSet): t.type =
        t.clear()
        t

    given PublicInternal.NameSet[JumboNameSet]:
      extension (seen: JumboNameSet)
        def alreadySeen(name: String): Boolean =
          val map        = seen.underlying0
          val sizeBefore = map.size
          map.addOne(name, ()).size == sizeBefore

        def clear(): Unit = seen.underlying0.clear()

  final class FieldIndexSet:
    private var bits: Array[Long] = new Array[Long](1)
    private var words: Int        = 0

    def reset(fieldCount: Int): this.type =
      val requiredWords = (fieldCount + java.lang.Long.SIZE - 1) >>> 6
      if bits.length < requiredWords then bits = new Array[Long](requiredWords)
      else java.util.Arrays.fill(bits, 0, words, 0L)
      words = requiredWords
      this

    def mark(index: Int): Unit =
      bits(index >>> 6) |= 1L << (index & 63)

    def contains(index: Int): Boolean =
      (bits(index >>> 6) & (1L << (index & 63))) != 0L

    def nextMarked(fromIndex: Int): Int =
      var wordIndex = fromIndex >>> 6
      if wordIndex >= words then -1
      else
        var word = bits(wordIndex) & (-1L << (fromIndex & 63))
        while word == 0L do
          wordIndex += 1
          if wordIndex >= words then return -1
          word = bits(wordIndex)
        (wordIndex << 6) + java.lang.Long.numberOfTrailingZeros(word)

    def clear(): Unit =
      java.util.Arrays.fill(bits, 0, words, 0L)
      words = 0

  object FieldIndexSet:
    given Alloc[FieldIndexSet]:
      def alloc(): FieldIndexSet            = new FieldIndexSet
      def prepare(t: FieldIndexSet): t.type =
        t.clear()
        t

  trait Alloc[T] {
    def alloc(): T
    def prepare(t: T): t.type
  }

  import scala.util.boundary.Label
  import steps.result.Result
  inline def loop[A](inline body: Label[A] ?=> Unit): A = {
    boundary[A] {
      while true do body
      loop.never
    }
  }
  object loop {
    def never: Nothing                                                                = ???
    inline def task[E](inline body: Label[Result[Unit, E]] ?=> Unit): Result[Unit, E] = loop(body)
    inline def break[A](a: A)(using Label[A]): Nothing          = boundary.break(a)
    inline def break()(using Label[Unit]): Nothing              = boundary.break()
    inline def done[E](using Label[Result[Unit, E]])(): Nothing = boundary.break(Result.done)
  }

  // TODO: publish version of steps with this method!
  inline def breakErr[E](using label: boundary.Label[Result.Err[E]])(
      inline r: Result.Err[E]
  ): Nothing =
    boundary.break(r)

  private[internal] abstract class PoolHolder:
    // TODO: should think how this could scale to making a global shared object.
    private[internal] val namesPool = LocalPool[JumboNameSet]()

    private[internal] def slotsPooling: Boolean

    // pooled builder slots for product-like schemas with a slots factory; borrow/release pairs nest,
    // so a reentrant decode inside a field value borrows a fresh instance. Lazy: a one-shot decoder
    // skips the slots path and must not pay for the pool either.
    @threadUnsafe
    private[internal] lazy val slotsPool = Internal.LocalPool[scalanotation.BuilderSlots]()

    @threadUnsafe
    private[internal] lazy val fieldIndexSetPool = Internal.LocalPool[Internal.FieldIndexSet]()

    private[internal] inline def withBorrowSlots[T](
        factory: scalanotation.TypedFactory.OfProduct[?] | Null
    )(inline f: (scalanotation.BuilderSlots | Null) => T): T =
      def useSlots(slots: scalanotation.BuilderSlots | Null): T =
        f(slots)
      if !slotsPooling || factory == null then useSlots(null)
      else slotsPool.withBorrowed(useSlots)
}

@publicInBinary
private[scalanotation] object PublicInternal {
  import scalanotation.Reader
  import quoted.{Expr as QExpr, *}

  trait Pool[T]:
    def borrow(): T
    def release(t: T): Unit

    inline def withBorrowed[A](inline f: T => A): A =
      val t = borrow()
      try f(t)
      finally release(t)

  trait NameSet[T]:
    extension (seen: T)
      def alreadySeen(name: String): Boolean
      def clear(): Unit

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

  object MapFactoryPairSeq:
    final class State[Key, Elem, Col[X, Y] <: scala.collection.Map[X, Y]](
        val builder: mutable.Builder[(Key, Elem), Col[Key, Elem]]
    ):
      var key: Any = null

    def apply[Key, Elem, Col[X, Y] <: scala.collection.Map[X, Y]](
        using factory: scala.collection.Factory[(Key, Elem), Col[Key, Elem]]
    ): MapFactoryPairSeq[Key, Elem, Col] =
      new MapFactoryPairSeq[Key, Elem, Col]

  class MapFactoryPairSeq[Key, Elem, Col[X, Y] <: scala.collection.Map[X, Y]](
      using factory: scala.collection.Factory[(Key, Elem), Col[Key, Elem]]
  ) extends Reader.PairSeqBuilder[Key, Elem, MapFactoryPairSeq.State[Key, Elem, Col], Col[
        Key,
        Elem
      ]]:
    def init(): MapFactoryPairSeq.State[Key, Elem, Col] =
      new MapFactoryPairSeq.State(factory.newBuilder)

    def addKey(
        repr: MapFactoryPairSeq.State[Key, Elem, Col],
        key: Key
    ): MapFactoryPairSeq.State[Key, Elem, Col] =
      repr.key = key
      repr

    def addValue(
        repr: MapFactoryPairSeq.State[Key, Elem, Col],
        elem: Elem
    ): MapFactoryPairSeq.State[Key, Elem, Col] =
      repr.builder.addOne((repr.key.asInstanceOf[Key], elem))
      repr.key = null
      repr

    def finish(repr: MapFactoryPairSeq.State[Key, Elem, Col]): Col[Key, Elem] =
      repr.builder.result()

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

  // specialized array writes — the write-side mirror of the specialized array builders: the typed
  // `xElementValue` override reads the unboxed primitive straight from the array, so an element is
  // never boxed on the way to the renderer
  object IntArrayWrite extends RawSchema.IndexedVectorWrite:
    def size(value: Any): Int                     = value.asInstanceOf[Array[Int]].length
    def elementValue(value: Any, index: Int): Any = value.asInstanceOf[Array[Int]](index)
    def iterator(value: Any): Iterator[Any]       =
      value.asInstanceOf[Array[Int]].iterator.asInstanceOf[Iterator[Any]]
    override def intElementValue(value: Any, index: Int): Int =
      value.asInstanceOf[Array[Int]](index)

  object LongArrayWrite extends RawSchema.IndexedVectorWrite:
    def size(value: Any): Int                     = value.asInstanceOf[Array[Long]].length
    def elementValue(value: Any, index: Int): Any = value.asInstanceOf[Array[Long]](index)
    def iterator(value: Any): Iterator[Any]       =
      value.asInstanceOf[Array[Long]].iterator.asInstanceOf[Iterator[Any]]
    override def longElementValue(value: Any, index: Int): Long =
      value.asInstanceOf[Array[Long]](index)

  object FloatArrayWrite extends RawSchema.IndexedVectorWrite:
    def size(value: Any): Int                     = value.asInstanceOf[Array[Float]].length
    def elementValue(value: Any, index: Int): Any = value.asInstanceOf[Array[Float]](index)
    def iterator(value: Any): Iterator[Any]       =
      value.asInstanceOf[Array[Float]].iterator.asInstanceOf[Iterator[Any]]
    override def floatElementValue(value: Any, index: Int): Float =
      value.asInstanceOf[Array[Float]](index)

  object DoubleArrayWrite extends RawSchema.IndexedVectorWrite:
    def size(value: Any): Int                     = value.asInstanceOf[Array[Double]].length
    def elementValue(value: Any, index: Int): Any = value.asInstanceOf[Array[Double]](index)
    def iterator(value: Any): Iterator[Any]       =
      value.asInstanceOf[Array[Double]].iterator.asInstanceOf[Iterator[Any]]
    override def doubleElementValue(value: Any, index: Int): Double =
      value.asInstanceOf[Array[Double]](index)

  object BooleanArrayWrite extends RawSchema.IndexedVectorWrite:
    def size(value: Any): Int                     = value.asInstanceOf[Array[Boolean]].length
    def elementValue(value: Any, index: Int): Any = value.asInstanceOf[Array[Boolean]](index)
    def iterator(value: Any): Iterator[Any]       =
      value.asInstanceOf[Array[Boolean]].iterator.asInstanceOf[Iterator[Any]]
    override def booleanElementValue(value: Any, index: Int): Boolean =
      value.asInstanceOf[Array[Boolean]](index)

  object CharArrayWrite extends RawSchema.IndexedVectorWrite:
    def size(value: Any): Int                     = value.asInstanceOf[Array[Char]].length
    def elementValue(value: Any, index: Int): Any = value.asInstanceOf[Array[Char]](index)
    def iterator(value: Any): Iterator[Any]       =
      value.asInstanceOf[Array[Char]].iterator.asInstanceOf[Iterator[Any]]
    override def charElementValue(value: Any, index: Int): Char =
      value.asInstanceOf[Array[Char]](index)

  /** Vector write for `Array[T]`/`IArray[T]`, specialized to read elements without boxing when the
    * element schema pins `T` to a primitive. A non-atomic element schema (e.g. mapped or derived)
    * falls back to the caller's generic iterator-based write, which is erased-safe for any array.
    */
  def arrayVectorWrite(
      elementSchema: RawSchema[?],
      fallback: RawSchema.VectorWrite
  ): RawSchema.VectorWrite =
    elementSchema match
      case RawSchema.Int     => IntArrayWrite
      case RawSchema.Long    => LongArrayWrite
      case RawSchema.Float   => FloatArrayWrite
      case RawSchema.Double  => DoubleArrayWrite
      case RawSchema.Boolean => BooleanArrayWrite
      case RawSchema.Char    => CharArrayWrite
      case _                 => fallback

  /** vector read for `Array[T]`, specialized to append without boxing when `T` is primitive */
  def arrayVectorRead[T](using tag: ClassTag[T]): RawSchema.VectorRead =
    tag.runtimeClass match
      case java.lang.Integer.TYPE   => BuildIntArray()
      case java.lang.Long.TYPE      => BuildLongArray()
      case java.lang.Float.TYPE     => BuildFloatArray()
      case java.lang.Double.TYPE    => BuildDoubleArray()
      case java.lang.Boolean.TYPE   => BuildBooleanArray()
      case java.lang.Character.TYPE => BuildCharArray()
      case _                        => BuildArray[T]()

  /** vector read for `IArray[T]`: a specialized array builder's result is freshly allocated, so for
    * primitive elements it can be safely viewed as the immutable array
    */
  def iarrayVectorRead[T](using tag: ClassTag[T]): RawSchema.VectorRead =
    if tag.runtimeClass.isPrimitive then arrayVectorRead[T]
    else BuildIArray[T]()

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
