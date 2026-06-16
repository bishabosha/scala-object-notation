package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.internal.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.{raise, ok}

import scala.annotation.switch

/** Push-model plumbing shared by the decoders: a decode step marks success by returning
  * [[Result.done]] (a shared constant — no [[Result.Ok]] is allocated on the happy path) after
  * pushing its decoded value into one of the slots and tagging that slot in [[lastSlotKind]].
  * Composite and reference results go to the Any slot via [[pushRef]]; primitive decoders push into
  * a typed slot, so no box is allocated for the push. The caller queries [[lastSlotKind]] to pull
  * from the right slot — forwarding the unboxed value to a typed builder method via
  * [[addSlot]]/[[storeSlot]], or boxing once at the user-facing boundary via [[pullAny]]. A
  * [[Result.Ok]] carrying the final value is only allocated at the boundary where the result is
  * returned back to the user.
  */
private[scalanotation] abstract class PushSlots extends Internal.PoolHolder:
  /** the most recently decoded reference value; written by the callee via [[pushRef]] */
  private var anySlot: Any = null

  /** the field index where the most recent [[fillSkippedNullableFields]] stopped */
  private var skipFillIndex: Int = 0

  /** the [[SlotKind]] of the most recent push: tells the caller which slot is live */
  private var lastSlotKind: Int = SlotKind.Ref

  // typed slots for atomic values: an atomic decoder pushes here without boxing, and the caller
  // pulls the typed value to wrap or append to its builder
  private var stringSlot: String   = ""
  private var charSlot: Char       = '\u0000'
  private var intSlot: Int         = 0
  private var longSlot: Long       = 0L
  private var floatSlot: Float     = 0.0f
  private var doubleSlot: Double   = 0.0d
  private var booleanSlot: Boolean = false

  protected final def pullSkipFillIndex(): Int =
    val res = skipFillIndex
    skipFillIndex = 0
    res

  /** fills skipped nullable fields with `None` from `startIndex` until the field named
    * `actualName`, returning the new builder state and leaving the next field index in
    * [[skipFillIndex]]
    */
  protected final def fillSkippedNullableFields(
      read: RawSchema.NamedTupleRead
  )(
      fields: IArray[Field],
      state0: read.State,
      startIndex: Int,
      actualName: String
  ): read.State =
    var index = startIndex
    var state = state0
    while index < fields.length
      && fields(index).name != actualName
      && TokenDecoder.isNullable(fields(index).schema)
    do
      state = read.add(state, index, None)
      index += 1
    skipFillIndex = index
    state

  /** pushes a reference (or already-boxed) value, tagging the Any slot as the live one */
  protected final def pushRef(value: Any): Unit =
    anySlot = value
    lastSlotKind = SlotKind.Ref

  protected final def pullRefStrict(): Any =
    assert(lastSlotKind == SlotKind.Ref, "pullRefStrict called when lastSlotKind != Ref")
    anySlot

  protected final def pushString(value: String): Unit =
    stringSlot = value
    lastSlotKind = SlotKind.String

  protected final def pullStringStrict(): String =
    assert(lastSlotKind == SlotKind.String, "pullStringStrict called when lastSlotKind != String")
    stringSlot

  protected final def pushChar(value: Char): Unit =
    charSlot = value
    lastSlotKind = SlotKind.Char

  protected final def pullCharStrict(): Char =
    assert(lastSlotKind == SlotKind.Char, "pullCharStrict called when lastSlotKind != Char")
    charSlot

  protected final def pushInt(value: Int): Unit =
    intSlot = value
    lastSlotKind = SlotKind.Int

  protected final def pullIntStrict(): Int =
    assert(lastSlotKind == SlotKind.Int, "pullIntStrict called when lastSlotKind != Int")
    intSlot

  protected final def pushLong(value: Long): Unit =
    longSlot = value
    lastSlotKind = SlotKind.Long

  protected final def pullLongStrict(): Long =
    assert(lastSlotKind == SlotKind.Long, "pullLongStrict called when lastSlotKind != Long")
    longSlot

  protected final def pushFloat(value: Float): Unit =
    floatSlot = value
    lastSlotKind = SlotKind.Float

  protected final def pullFloatStrict(): Float =
    assert(lastSlotKind == SlotKind.Float, "pullFloatStrict called when lastSlotKind != Float")
    floatSlot

  protected final def pushDouble(value: Double): Unit =
    doubleSlot = value
    lastSlotKind = SlotKind.Double

  protected final def pullDoubleStrict(): Double =
    assert(lastSlotKind == SlotKind.Double, "pullDoubleStrict called when lastSlotKind != Double")
    doubleSlot

  protected final def pushBoolean(value: Boolean): Unit =
    booleanSlot = value
    lastSlotKind = SlotKind.Boolean

  protected final def pullBooleanStrict(): Boolean =
    assert(
      lastSlotKind == SlotKind.Boolean,
      "pullBooleanStrict called when lastSlotKind != Boolean"
    )
    booleanSlot

  /** pulls the most recent push as a single value, boxing the live typed slot if necessary */
  protected final def pullAny(): Any =
    (lastSlotKind: @switch) match
      case SlotKind.String  => stringSlot
      case SlotKind.Char    => charSlot
      case SlotKind.Int     => intSlot
      case SlotKind.Long    => longSlot
      case SlotKind.Float   => floatSlot
      case SlotKind.Double  => doubleSlot
      case SlotKind.Boolean => booleanSlot
      case _                => anySlot

  /** clears slot state for reuse via a pool */
  protected final def resetSlots(): Unit =
    anySlot = null
    stringSlot = ""
    lastSlotKind = SlotKind.Ref

  /** appends the live slot to a vector builder state, unboxed when a typed slot is live */
  protected final def addSlot(read: RawSchema.VectorRead)(state: read.State): read.State =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addString(state, stringSlot)
      case SlotKind.Char    => read.addChar(state, charSlot)
      case SlotKind.Int     => read.addInt(state, intSlot)
      case SlotKind.Long    => read.addLong(state, longSlot)
      case SlotKind.Float   => read.addFloat(state, floatSlot)
      case SlotKind.Double  => read.addDouble(state, doubleSlot)
      case SlotKind.Boolean => read.addBoolean(state, booleanSlot)
      case _                => read.add(state, anySlot)

  /** adds the live slot at `index` of a tuple builder state, unboxed when a typed slot is live */
  protected final def addSlot(
      read: RawSchema.TupleRead
  )(state: read.State, index: Int): read.State =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addString(state, index, stringSlot)
      case SlotKind.Char    => read.addChar(state, index, charSlot)
      case SlotKind.Int     => read.addInt(state, index, intSlot)
      case SlotKind.Long    => read.addLong(state, index, longSlot)
      case SlotKind.Float   => read.addFloat(state, index, floatSlot)
      case SlotKind.Double  => read.addDouble(state, index, doubleSlot)
      case SlotKind.Boolean => read.addBoolean(state, index, booleanSlot)
      case _                => read.add(state, index, anySlot)

  /** adds the live slot at `index` of a named-tuple builder state, unboxed when typed */
  protected final def addSlot(
      read: RawSchema.NamedTupleRead
  )(state: read.State, index: Int): read.State =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addString(state, index, stringSlot)
      case SlotKind.Char    => read.addChar(state, index, charSlot)
      case SlotKind.Int     => read.addInt(state, index, intSlot)
      case SlotKind.Long    => read.addLong(state, index, longSlot)
      case SlotKind.Float   => read.addFloat(state, index, floatSlot)
      case SlotKind.Double  => read.addDouble(state, index, doubleSlot)
      case SlotKind.Boolean => read.addBoolean(state, index, booleanSlot)
      case _                => read.add(state, index, anySlot)

  /** adds the live slot at `key` of a dict builder state, unboxed when a typed slot is live */
  protected final def addSlot(
      read: RawSchema.DictRead
  )(state: read.State, key: String): read.State =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addString(state, key, stringSlot)
      case SlotKind.Char    => read.addChar(state, key, charSlot)
      case SlotKind.Int     => read.addInt(state, key, intSlot)
      case SlotKind.Long    => read.addLong(state, key, longSlot)
      case SlotKind.Float   => read.addFloat(state, key, floatSlot)
      case SlotKind.Double  => read.addDouble(state, key, doubleSlot)
      case SlotKind.Boolean => read.addBoolean(state, key, booleanSlot)
      case _                => read.add(state, key, anySlot)

  /** applies a [[RawSchema.Mapped]] mapping to the live slot after a successful push */
  protected final def mapSlot(
      mapping: RawSchema.SchemaMapping
  ): Result[Unit, DecodeError] = Result.task {
    mapping.totalMaps match
      case RawSchema.SchemaMapping.TotalMap.IntMap(fn) =>
        if lastSlotKind == SlotKind.Int then pushRef(fn(intSlot))
        else pushRef(fn(pullAny().asInstanceOf[Int]))
      case RawSchema.SchemaMapping.TotalMap.LongMap(fn) =>
        if lastSlotKind == SlotKind.Long then pushRef(fn(longSlot))
        else pushRef(fn(pullAny().asInstanceOf[Long]))
      case RawSchema.SchemaMapping.TotalMap.FloatMap(fn) =>
        if lastSlotKind == SlotKind.Float then pushRef(fn(floatSlot))
        else pushRef(fn(pullAny().asInstanceOf[Float]))
      case RawSchema.SchemaMapping.TotalMap.DoubleMap(fn) =>
        if lastSlotKind == SlotKind.Double then pushRef(fn(doubleSlot))
        else pushRef(fn(pullAny().asInstanceOf[Double]))
      case RawSchema.SchemaMapping.TotalMap.AnyMap(fn) =>
        pushRef(fn(pullAny()))
      case RawSchema.SchemaMapping.TotalMap.Empty =>
        val fn = mapping.resultMap
        if fn == null then () // preserve the live slot as-is
        else
          val value1 = fn(pullAny()).ok
          pushRef(value1)
  }

  /** [[Result.eval.check]] with error decoration that allocates only on the error path */
  protected inline def checkOrRaise(inline r: Result[Unit, DecodeError])(
      inline decorate: DecodeError => DecodeError
  )(using scala.util.boundary.Label[Result.Err[DecodeError]]): Unit =
    r match
      case Result.Err(error) => raise(decorate(error))
      case _                 => ()
