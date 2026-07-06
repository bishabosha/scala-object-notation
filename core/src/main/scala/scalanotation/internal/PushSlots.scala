package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.schema.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.{raise, ok}

import scala.annotation.switch
import scalanotation.schema.RawSchema
import scalanotation.schema

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
private[scalanotation] object PushSlots:
  /** Upper bound on input-driven decode nesting (composite/collection levels). Bounds stack use on
    * malicious deeply-nested input — e.g. ten thousand repetitions of `Vector(` — which would
    * otherwise crash the decoding thread with a [[StackOverflowError]] instead of returning a
    * [[DecodeError]]. Generous: real configuration data stays far below it.
    */
  inline val MaxNestingDepth = 512

private[scalanotation] abstract class PushSlots extends Internal.PoolHolder:
  /** the most recently decoded reference value; written by the callee via [[pushRef]] */
  private var anySlot: Any = null

  /** current input-driven nesting depth — see [[PushSlots.MaxNestingDepth]] */
  private var nestingDepth: Int = 0

  /** Enters one nesting level, or reports `false` when [[PushSlots.MaxNestingDepth]] is exhausted.
    * Balanced by [[exitNesting]] on normal return paths; an exception aborting the whole decode
    * leaves the count stale, which [[resetSlots]] clears on pooled reuse.
    */
  protected final def enterNesting(): Boolean =
    if nestingDepth >= PushSlots.MaxNestingDepth then false
    else
      nestingDepth += 1
      true

  protected final def exitNesting(): Unit =
    nestingDepth -= 1

  protected final def nestingLimitError(): DecodeError =
    DecodeError.Custom(
      s"Nesting depth exceeds the supported maximum of ${PushSlots.MaxNestingDepth}"
    )

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

  protected final def pushString(value: String): Unit =
    stringSlot = value
    lastSlotKind = SlotKind.String

  protected final def pullStringStrict(): String =
    assert(lastSlotKind == SlotKind.String, "pullStringStrict called when lastSlotKind != String")
    stringSlot

  protected final def pushChar(value: Char): Unit =
    charSlot = value
    lastSlotKind = SlotKind.Char

  protected final def pushInt(value: Int): Unit =
    intSlot = value
    lastSlotKind = SlotKind.Int

  protected final def pushLong(value: Long): Unit =
    longSlot = value
    lastSlotKind = SlotKind.Long

  protected final def pushFloat(value: Float): Unit =
    floatSlot = value
    lastSlotKind = SlotKind.Float

  protected final def pushDouble(value: Double): Unit =
    doubleSlot = value
    lastSlotKind = SlotKind.Double

  protected final def pushBoolean(value: Boolean): Unit =
    booleanSlot = value
    lastSlotKind = SlotKind.Boolean

  // Strict typed pulls: read the slot a caller knows is live from its decode plan, skipping the
  // kind dispatch of [[pullAny]]/[[addSlot]]. Assertions document the contract without cost in
  // production (elided like the one in [[pullStringStrict]]).
  protected final def pullIntValue(): Int =
    assert(lastSlotKind == SlotKind.Int, "pullIntValue called when lastSlotKind != Int")
    intSlot

  protected final def pullLongValue(): Long =
    assert(lastSlotKind == SlotKind.Long, "pullLongValue called when lastSlotKind != Long")
    longSlot

  protected final def pullFloatValue(): Float =
    assert(lastSlotKind == SlotKind.Float, "pullFloatValue called when lastSlotKind != Float")
    floatSlot

  protected final def pullDoubleValue(): Double =
    assert(lastSlotKind == SlotKind.Double, "pullDoubleValue called when lastSlotKind != Double")
    doubleSlot

  protected final def pullBooleanValue(): Boolean =
    assert(lastSlotKind == SlotKind.Boolean, "pullBooleanValue called when lastSlotKind != Boolean")
    booleanSlot

  protected final def pullCharValue(): Char =
    assert(lastSlotKind == SlotKind.Char, "pullCharValue called when lastSlotKind != Char")
    charSlot

  /** Control-flow channel for cold decode helpers that must hand an index or a small code back
    * through a [[steps.result.Result.task]] boundary without boxing a Result value. A dedicated
    * slot — separate from the value slots and from [[lastSlotKind]] — so it can be live at the same
    * time as a decoded value.
    */
  private var controlSlot: Int = 0

  protected final def pushControl(value: Int): Unit =
    controlSlot = value

  protected final def pullControl(): Int = controlSlot

  /** The error channel of the value-returning decoders: a failed decode parks its error here and
    * returns a zero value, and the caller checks-and-consumes in the same frame — no boundary is
    * entered per value (a `boundary` allocates its Label whenever the label escapes into a
    * non-inlined callee), and decoded values return through registers, not slots.
    */
  // public on this internal class: inline bodies in several traits read it, and any tighter
  // visibility makes each trait mint a clashing `inline$` accessor
  var pendingValueError: DecodeError | Null = null

  /** parks `error` as the pending value error and returns `zero` to the caller's checked frame */
  protected inline def failValue[T](error: DecodeError, inline zero: T): T =
    pendingValueError = error
    zero

  /** consumes the pending value error — call only when [[pendingValueError]] was seen non-null */
  protected final def takeValueError(): DecodeError =
    val error = pendingValueError.nn
    pendingValueError = null
    error

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
    nestingDepth = 0
    pendingValueError = null

  /** appends the live slot to a vector builder state, unboxed when a typed slot is live */
  protected final def addSlot[Elem, Repr, A](
      read: Reader.VectorBuilder[Elem, Repr, A]
  )(state: Repr): Repr =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addString(state, stringSlot)
      case SlotKind.Char    => read.addChar(state, charSlot)
      case SlotKind.Int     => read.addInt(state, intSlot)
      case SlotKind.Long    => read.addLong(state, longSlot)
      case SlotKind.Float   => read.addFloat(state, floatSlot)
      case SlotKind.Double  => read.addDouble(state, doubleSlot)
      case SlotKind.Boolean => read.addBoolean(state, booleanSlot)
      case _                => read.add(state, anySlot.asInstanceOf[Elem])

  /** adds the live slot at `index` of a tuple builder state, unboxed when a typed slot is live */
  protected final def addSlot[Repr, A](
      read: Reader.TupleBuilder[Repr, A]
  )(state: Repr, index: Int): Repr =
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
  protected final def addSlot[Elem, Repr, A](
      read: Reader.DictBuilder[Elem, Repr, A]
  )(state: Repr, key: String): Repr =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addString(state, key, stringSlot)
      case SlotKind.Char    => read.addChar(state, key, charSlot)
      case SlotKind.Int     => read.addInt(state, key, intSlot)
      case SlotKind.Long    => read.addLong(state, key, longSlot)
      case SlotKind.Float   => read.addFloat(state, key, floatSlot)
      case SlotKind.Double  => read.addDouble(state, key, doubleSlot)
      case SlotKind.Boolean => read.addBoolean(state, key, booleanSlot)
      case _                => read.add(state, key, anySlot.asInstanceOf[Elem])

  /** adds the live key slot to a key/value sequence builder state */
  protected final def addPairKeySlot[Key, Elem, Repr, A](
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A]
  )(state: Repr): Repr =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addStringKey(state, stringSlot)
      case SlotKind.Char    => read.addCharKey(state, charSlot)
      case SlotKind.Int     => read.addIntKey(state, intSlot)
      case SlotKind.Long    => read.addLongKey(state, longSlot)
      case SlotKind.Float   => read.addFloatKey(state, floatSlot)
      case SlotKind.Double  => read.addDoubleKey(state, doubleSlot)
      case SlotKind.Boolean => read.addBooleanKey(state, booleanSlot)
      case _                => read.addKey(state, anySlot.asInstanceOf[Key])

  /** adds the live value slot to a key/value sequence builder state */
  protected final def addPairValueSlot[Key, Elem, Repr, A](
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A]
  )(state: Repr): Repr =
    (lastSlotKind: @switch) match
      case SlotKind.String  => read.addStringValue(state, stringSlot)
      case SlotKind.Char    => read.addCharValue(state, charSlot)
      case SlotKind.Int     => read.addIntValue(state, intSlot)
      case SlotKind.Long    => read.addLongValue(state, longSlot)
      case SlotKind.Float   => read.addFloatValue(state, floatSlot)
      case SlotKind.Double  => read.addDoubleValue(state, doubleSlot)
      case SlotKind.Boolean => read.addBooleanValue(state, booleanSlot)
      case _                => read.addValue(state, anySlot.asInstanceOf[Elem])

  /** applies a [[RawSchema.Mapped]] mapping to the live slot after a successful push */
  protected final def mapSlot(
      mapping: schema.SchemaMapping[?, ?]
  ): Result[Unit, DecodeError] = Result.task {
    mapping.totalMaps match
      case schema.SchemaMapping.TotalMap.IntMap(fn) =>
        if lastSlotKind == SlotKind.Int then pushRef(fn(intSlot))
        else pushRef(fn(pullAny().asInstanceOf[Int]))
      case schema.SchemaMapping.TotalMap.LongMap(fn) =>
        if lastSlotKind == SlotKind.Long then pushRef(fn(longSlot))
        else pushRef(fn(pullAny().asInstanceOf[Long]))
      case schema.SchemaMapping.TotalMap.FloatMap(fn) =>
        if lastSlotKind == SlotKind.Float then pushRef(fn(floatSlot))
        else pushRef(fn(pullAny().asInstanceOf[Float]))
      case schema.SchemaMapping.TotalMap.DoubleMap(fn) =>
        if lastSlotKind == SlotKind.Double then pushRef(fn(doubleSlot))
        else pushRef(fn(pullAny().asInstanceOf[Double]))
      case schema.SchemaMapping.TotalMap.AnyMap(fn) =>
        val fn0 = fn.asInstanceOf[RawSchema.InputMap[Any, Any]]
        pushRef(fn0(pullAny()))
      case schema.SchemaMapping.TotalMap.Empty =>
        val fn = mapping.resultMap
        if fn == null then () // preserve the live slot as-is
        else
          val fn0    = fn.asInstanceOf[RawSchema.ResultMap[Any, Any]]
          val value1 = fn0(pullAny()).ok
          pushRef(value1)
  }

  /** [[Result.eval.check]] with error decoration that allocates only on the error path */
  protected inline def checkOrRaise(inline r: Result[Unit, DecodeError])(
      inline decorate: DecodeError => DecodeError
  )(using scala.util.boundary.Label[Result.Err[DecodeError]]): Unit =
    r match
      case Result.Err(error) => raise(decorate(error))
      case _                 => ()
