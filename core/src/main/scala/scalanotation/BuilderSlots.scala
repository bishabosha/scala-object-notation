package scalanotation

import scalanotation.internal.SlotKind

import scala.annotation.switch

/** Appendable typed storage for the field values of a single product-like decode, pooled per
  * decoder and reused. Primitive fields are packed into a `Long` per slot, so filling never
  * allocates or boxes. Finalization either pulls typed slots directly (a derived [[TypedFactory]]),
  * boxes on demand through the [[Product]] interface for `Mirror.fromProduct`, or copies to a boxed
  * array for `Tuple.fromIArray`.
  */
final class BuilderSlots private[scalanotation] () extends Product:
  private var kinds: Array[Int]   = new Array[Int](8)
  private var refs: Array[AnyRef] = new Array[AnyRef](8)
  private var prims: Array[Long]  = new Array[Long](8)
  private var arity: Int          = 0

  /** Re-sizes for a product of `size` fields. Stale references from the previous use are NOT
    * cleared: every decode path writes all `size` slots before [[TypedFactory.fromSlots]] reads
    * them (skipped nullable fields store `None`, and a failed decode never reaches the factory), so
    * clearing per record would only bound retention — which is already limited to the last record's
    * field values per pooled instance.
    */
  private[scalanotation] def reset(size: Int): this.type =
    if kinds.length < size then
      var capacity = kinds.length
      while capacity < size do capacity *= 2
      kinds = new Array[Int](capacity)
      refs = new Array[AnyRef](capacity)
      prims = new Array[Long](capacity)
    arity = size
    this

  private[scalanotation] def setRef(index: Int, value: Any): Unit =
    refs(index) = value.asInstanceOf[AnyRef]
    kinds(index) = SlotKind.Ref

  private[scalanotation] def setString(index: Int, value: String): Unit =
    refs(index) = value
    kinds(index) = SlotKind.String

  private[scalanotation] def setChar(index: Int, value: Char): Unit =
    prims(index) = value.toLong
    kinds(index) = SlotKind.Char

  private[scalanotation] def setInt(index: Int, value: Int): Unit =
    prims(index) = value.toLong
    kinds(index) = SlotKind.Int

  private[scalanotation] def setLong(index: Int, value: Long): Unit =
    prims(index) = value
    kinds(index) = SlotKind.Long

  private[scalanotation] def setFloat(index: Int, value: Float): Unit =
    prims(index) = java.lang.Float.floatToRawIntBits(value).toLong
    kinds(index) = SlotKind.Float

  private[scalanotation] def setDouble(index: Int, value: Double): Unit =
    prims(index) = java.lang.Double.doubleToRawLongBits(value)
    kinds(index) = SlotKind.Double

  private[scalanotation] def setBoolean(index: Int, value: Boolean): Unit =
    prims(index) = if value then 1L else 0L
    kinds(index) = SlotKind.Boolean

  def getRef(index: Int): Any = productElement(index)

  def getString(index: Int): String =
    refs(index).asInstanceOf[String]

  def getChar(index: Int): Char =
    if kinds(index) == SlotKind.Char then prims(index).toChar
    else refs(index).asInstanceOf[Char]

  def getInt(index: Int): Int =
    if kinds(index) == SlotKind.Int then prims(index).toInt
    else refs(index).asInstanceOf[Int]

  def getLong(index: Int): Long =
    if kinds(index) == SlotKind.Long then prims(index)
    else refs(index).asInstanceOf[Long]

  def getFloat(index: Int): Float =
    if kinds(index) == SlotKind.Float then java.lang.Float.intBitsToFloat(prims(index).toInt)
    else refs(index).asInstanceOf[Float]

  def getDouble(index: Int): Double =
    if kinds(index) == SlotKind.Double then java.lang.Double.longBitsToDouble(prims(index))
    else refs(index).asInstanceOf[Double]

  def getBoolean(index: Int): Boolean =
    if kinds(index) == SlotKind.Boolean then prims(index) != 0L
    else refs(index).asInstanceOf[Boolean]

  def productArity: Int = arity

  def productElement(n: Int): Any =
    (kinds(n): @switch) match
      case SlotKind.Char    => prims(n).toChar
      case SlotKind.Int     => prims(n).toInt
      case SlotKind.Long    => prims(n)
      case SlotKind.Float   => java.lang.Float.intBitsToFloat(prims(n).toInt)
      case SlotKind.Double  => java.lang.Double.longBitsToDouble(prims(n))
      case SlotKind.Boolean => prims(n) != 0L
      case _                => refs(n)

  def canEqual(that: Any): Boolean = false
