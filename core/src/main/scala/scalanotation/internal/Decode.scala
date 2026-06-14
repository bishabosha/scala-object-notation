package scalanotation.internal

import scalanotation.BuilderSlots
import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.internal.BuilderSlotsPool.given
import scalanotation.internal.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

import scala.annotation.switch
import scala.compiletime.uninitialized

import TokenDecoder.describe

private[scalanotation] object ExprDecoder:
  def decodeExpr[A: Reader as reader](expr: Expr): Result[A, DecodeError] =
    ExprDecoder().decodeInto(reader, expr)

private[scalanotation] object NumericPromotions:
  private[scalanotation] def isExactFloat(value: Int): Boolean =
    value.toFloat.toInt == value

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
      mapping: RawSchema.SchemaMapping,
      r: Result[Unit, DecodeError]
  ): Result[Unit, DecodeError] =
    val fn = mapping.resultMap
    if fn == null || r.isErr then r
    else
      fn(pullAny()) match
        case Result.Ok(value)    => pushRef(value); r
        case err @ Result.Err(_) => err

  /** [[Result.eval.check]] with error decoration that allocates only on the error path */
  protected inline def checkOrRaise(inline r: Result[Unit, DecodeError])(
      inline decorate: DecodeError => DecodeError
  )(using scala.util.boundary.Label[Result.Err[DecodeError]]): Unit =
    r match
      case Result.Err(error) => raise(decorate(error))
      case _                 => ()

private[scalanotation] class ExprDecoder extends PushSlots:
  private def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private def isExactFloat(value: Int): Boolean =
    NumericPromotions.isExactFloat(value)

  private def describeExpr(expr: Expr): String =
    TokenDecoder.describe(expr)

  private def expectedType(schema: RawSchema, expr: Expr): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeExpr(expr))

  def decodeInto[A](reader: Reader[A], expr: Expr): Result[A, DecodeError] =
    Result:
      decodeBase(reader.schema, expr).check
      pullAny().asInstanceOf[A]

  private[scalanotation] def decodeRaw(
      schema: RawSchema,
      expr: Expr
  ): Result[Any, DecodeError] =
    Result:
      decodeBase(schema, expr).check
      pullAny()

  private def decodeBase(
      schema: RawSchema,
      expr: Expr
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapSlot(mapped.mapping, decodeBase(mapped.base, expr))
      case sc: RawSchema.NamedTuple =>
        decodeNamedTuple(sc, expr)
      case sc: RawSchema.Tuple =>
        decodeTuple(sc, expr)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        decodePartialNamedTuple(base, alreadySeenField, expr)
      case sc: RawSchema.Sum =>
        decodeSum(sc, expr)
      case sc: RawSchema.DiscriminatorSum =>
        decodeDiscriminatorSum(sc, expr)
      case sc: RawSchema.Vector =>
        decodeVector(sc, expr)
      case sc: RawSchema.Dict =>
        decodeDict(sc, expr)
      case sc: RawSchema.Option =>
        expr match
          case Expr.NullConstant =>
            pushRef(None)
            Result.done
          case other =>
            val r = decodeBase(sc.inner, other)
            if r.isOk then pushRef(Some(pullAny()))
            r
      case RawSchema.AnyExpr =>
        pushRef(expr)
        Result.done
      case RawSchema.String =>
        expr match
          case Expr.StringConstant(value) =>
            pushString(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.String, other))
      case RawSchema.Char =>
        expr match
          case Expr.CharConstant(value) =>
            pushChar(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Char, other))
      case RawSchema.Int =>
        expr match
          case Expr.IntConstant(value) =>
            pushInt(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Int, other))
      case RawSchema.Long =>
        expr match
          case Expr.LongConstant(value) =>
            pushLong(value)
            Result.done
          case Expr.IntConstant(value) =>
            pushLong(value.toLong)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Long, other))
      case RawSchema.Float =>
        expr match
          case Expr.FloatConstant(value) =>
            pushFloat(value)
            Result.done
          case Expr.IntConstant(value) if isExactFloat(value) =>
            pushFloat(value.toFloat)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Float, other))
      case RawSchema.Double =>
        expr match
          case Expr.DoubleConstant(value) =>
            pushDouble(value)
            Result.done
          case Expr.IntConstant(value) =>
            pushDouble(value.toDouble)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Double, other))
      case RawSchema.Boolean =>
        expr match
          case Expr.BooleanConstant(value) =>
            pushBoolean(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Boolean, other))
      case RawSchema.Null =>
        expr match
          case Expr.NullConstant =>
            pushRef(null)
            Result.done
          case other => Result.Err(expectedType(schema, other))

  private def decodeVector(
      schema: RawSchema.Vector,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.VectorExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read   = schema.read.nn
        var values = read.init()
        var index  = 0
        while index < elements.length do
          checkOrRaise(decodeBase(schema.element, elements(index)))(_.atPath(s"[$index]"))
          values = addSlot(read)(values)
          index += 1
        pushRef(read.finish(values))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeTuple(
      schema: RawSchema.Tuple,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.TupleExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read  = schema.read.nn
        val slots = schema.slots
        if elements.length != slots.length then
          raise(DecodeError.FieldCountMismatch(slots.length, elements.length))
        var state = read.init(slots.length)
        var index = 0
        while index < slots.length do
          checkOrRaise(decodeBase(slots(index), elements(index)))(_.atPath(s"[$index]"))
          state = addSlot(read)(state, index)
          index += 1
        pushRef(read.finish(state))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple,
      expr: Expr
  ): Result[Unit, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result.task:
      if schema.read == null then missingReadCapability(schema)
      val read = schema.read.nn
      schema.isValidNamedTuple(namesPool).check
      expr match
        case Expr.NamedTupleExpr(fieldExprs) =>
          val fields = schema.fields
          var state  = read.init(fields.length, slots = null)
          if schema.allowSkippedNullableFields then
            if fieldExprs.isEmpty && fields.nonEmpty then raise(DecodeError.UnitValueNotAllowed())
            var fieldExprIndex = 0
            var fieldIndex     = 0
            while fieldExprIndex < fieldExprs.length do
              val fieldExpr          = fieldExprs(fieldExprIndex)
              val fieldName          = fieldExpr.name
              val expectedBeforeSkip =
                if fieldIndex < fields.length then fields(fieldIndex) else null
              state = fillSkippedNullableFields(read)(fields, state, fieldIndex, fieldName)
              fieldIndex = pullSkipFillIndex()

              if fieldIndex >= fields.length then
                if expectedBeforeSkip == null then
                  raise(DecodeError.FieldCountMismatch(fields.length, fieldExprIndex + 1))
                else raise(DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, fieldName))

              val field = fields(fieldIndex)
              if fieldName != field.name then
                raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

              checkOrRaise(decodeBase(field.schema, fieldExpr.value))(
                _.atPath(s".${field.name}")
              )
              state = addSlot(read)(state, fieldIndex)
              fieldIndex += 1
              fieldExprIndex += 1

            state = fillSkippedNullableFields(read)(fields, state, fieldIndex, "")
            fieldIndex = pullSkipFillIndex()
            if fieldIndex != fields.length then
              raise(DecodeError.FieldCountMismatch(fields.length, fieldExprs.length))
          else
            if fieldExprs.length != fields.length then
              raise(DecodeError.FieldCountMismatch(fields.length, fieldExprs.length))
            var index = 0
            while index < fields.length do
              val field     = fields(index)
              val fieldExpr = fieldExprs(index)
              val fieldName = fieldExpr.name
              if fieldName != field.name then
                raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

              checkOrRaise(decodeBase(field.schema, fieldExpr.value))(
                _.atPath(s".${field.name}")
              )
              state = addSlot(read)(state, index)
              index += 1

          pushRef(read.finish(state))
        case other =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
  }

  private def decodeDict(
      schema: RawSchema.Dict,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        if schema.read == null then missingReadCapability(schema)
        val read  = schema.read.nn
        var index = 0
        var state = read.init()
        while index < fieldExprs.length do
          val fieldExpr = fieldExprs(index)
          val fieldName = fieldExpr.name
          checkOrRaise(decodeBase(schema.element, fieldExpr.value))(
            _.atPath(s".${fieldName}")
          )
          state = addSlot(read)(state, fieldName)
          index += 1

        pushRef(read.finish(state))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeSum(
      schema: RawSchema.Sum,
      expr: Expr
  ): Result[Unit, DecodeError] =
    Result.task:
      expr match
        case Expr.NamedTupleExpr(fieldExprs) =>
          if fieldExprs.length != 1 then raise(DecodeError.FieldCountMismatch(1, fieldExprs.length))
          val fieldExpr = fieldExprs(0)
          val caseName  = fieldExpr.name
          val value     = fieldExpr.value
          val sumCase   = RawSchema.findCase(schema.cases, caseName) match
            case null => raise(DecodeError.UnexpectedField(caseName).atPath(s".$caseName"))
            case c    => c
          checkOrRaise(decodeBase(sumCase.schema, value))(_.atPath(s".$caseName"))
        case other =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeDiscriminatorSum(
      schema: RawSchema.DiscriminatorSum,
      expr: Expr
  ): Result[Unit, DecodeError] =
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        Result.task:
          val discriminatorField = schema.discriminatorField
          if fieldExprs.isEmpty then raise(DecodeError.FieldCountMismatch(1, 0))

          val discriminatorExpr = fieldExprs(0)
          if discriminatorExpr.name != discriminatorField then
            raise(DecodeError.FieldOrderMismatch(discriminatorField, discriminatorExpr.name))

          checkOrRaise(decodeBase(RawSchema.String, discriminatorExpr.value))(
            _.atPath(s".$discriminatorField")
          )
          val caseName = pullStringStrict()
          val sumCase  = RawSchema.findCase(schema.cases, caseName) match
            case null =>
              raise(DecodeError.UnexpectedField(caseName).atPath(s".$discriminatorField"))
            case c => c
          decodeBase(sumCase.schema, expr).check
      case other =>
        Result.Err(expectedType(schema, other))

  private def decodePartialNamedTuple(
      schema: RawSchema,
      alreadySeenField: String,
      expr: Expr
  ): Result[Unit, DecodeError] =
    schema match
      case RawSchema.PartialNamedTuple(base, _) =>
        decodePartialNamedTuple(base, alreadySeenField, expr)
      case mapped: RawSchema.Mapped =>
        mapSlot(mapped.mapping, decodePartialNamedTuple(mapped.base, alreadySeenField, expr))
      case namedTuple: RawSchema.NamedTuple =>
        decodePartialNamedTuple(namedTuple, alreadySeenField, expr)
      case RawSchema.Null =>
        expr match
          case Expr.NamedTupleExpr(fieldExprs) =>
            Result.task:
              validatePartialNamedTupleStart(fieldExprs, alreadySeenField).check
              val payloadFieldCount = fieldExprs.length - 1
              if payloadFieldCount == 0 then pushRef(null)
              else raise(DecodeError.FieldCountMismatch(0, payloadFieldCount))
          case other =>
            Result.Err(DecodeError.ExpectedType(RawSchema.Null.describeSelf, describeExpr(other)))
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeExpr(expr)))

  private def validatePartialNamedTupleStart(
      fieldExprs: IndexedSeq[(name: String, value: Expr)],
      alreadySeenField: String
  ): Result[Unit, DecodeError] =
    Result.task:
      if fieldExprs.isEmpty then raise(DecodeError.FieldCountMismatch(1, 0))
      val actualName = fieldExprs(0).name
      if actualName != alreadySeenField then
        raise(DecodeError.FieldOrderMismatch(alreadySeenField, actualName))

  private def decodePartialNamedTuple(
      schema: RawSchema.NamedTuple,
      alreadySeenField: String,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    if schema.read == null then missingReadCapability(schema)
    val read = schema.read.nn
    schema.isValidNamedTuple(namesPool).check
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        validatePartialNamedTupleStart(fieldExprs, alreadySeenField).check
        val fields     = schema.fields
        var state      = read.init(fields.length, slots = null)
        var fieldIndex = 0
        val offset     = 1
        var index      = offset
        while index < fieldExprs.length do
          val parsedFieldIndex               = index - offset
          val fieldExpr                      = fieldExprs(index)
          val actualName                     = fieldExpr.name
          val validated: DecodeError | Field = {
            if schema.allowSkippedNullableFields then
              val expectedBeforeSkip =
                if fieldIndex < fields.length then fields(fieldIndex) else null
              state = fillSkippedNullableFields(read)(fields, state, fieldIndex, actualName)
              fieldIndex = pullSkipFillIndex()

              if fieldIndex >= fields.length then
                if expectedBeforeSkip == null then
                  DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                else DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
              else
                val expectedField = fields(fieldIndex)
                if actualName != expectedField.name then
                  DecodeError.FieldOrderMismatch(expectedField.name, actualName)
                else expectedField
            else if parsedFieldIndex >= fields.length then
              DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
            else
              val expectedField = fields(parsedFieldIndex)
              if actualName != expectedField.name then
                DecodeError.FieldOrderMismatch(expectedField.name, actualName)
              else expectedField
          }
          validated match
            case expectedField: Field =>
              checkOrRaise(decodeBase(expectedField.schema, fieldExpr.value))(
                _.atPath(s".${expectedField.name}")
              )
              state = addSlot(read)(state, fieldIndex)
              fieldIndex += 1
            case err: DecodeError => raise(err.atPath(s".$actualName"))
          index += 1

        if schema.allowSkippedNullableFields then
          state = fillSkippedNullableFields(read)(fields, state, fieldIndex, "")
          fieldIndex = pullSkipFillIndex()

        val payloadFieldCount = fieldExprs.length - offset
        val decodedFieldCount =
          if schema.allowSkippedNullableFields then fieldIndex else payloadFieldCount
        if decodedFieldCount != fields.length then
          raise(DecodeError.FieldCountMismatch(fields.length, payloadFieldCount))

        pushRef(read.finish(state))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

private[scalanotation] object TokenDecoder:

  /** The sole implementation of the opaque [[scalanotation.BatchContext]]: a wrapper over a pool of
    * decoder instances. The pooled contexts amortize decoder construction, which dominates the
    * fixed cost of small decodes; a reentrant decode (e.g. from a user-supplied schema mapping)
    * borrows a second instance instead of corrupting the active one.
    */
  private[scalanotation] enum PoolHolder:
    case RealPoolHolder(pool: Internal.Pool[TokenDecoder])
    case NoPoolHolder

  private def pooled(): TokenDecoder =
    TokenDecoder("", debug = false, slotsPooling = true, scanOnInit = false)

  private def oneShot(input: String, debug: Boolean): TokenDecoder =
    TokenDecoder(input, debug, slotsPooling = false, scanOnInit = true)

  private object PoolDecoderAlloc extends Internal.Alloc[TokenDecoder]:
    def alloc(): TokenDecoder            = TokenDecoder.pooled()
    def prepare(t: TokenDecoder): t.type = t // re-aimed by reset(input, debug) after borrowing

  private[scalanotation] val gcContext: PoolHolder =
    PoolHolder.NoPoolHolder // one-shot decodes allocate a new decoder for each call

  private[scalanotation] def localContext(): PoolHolder =
    given Internal.Alloc[TokenDecoder] = PoolDecoderAlloc
    PoolHolder.RealPoolHolder(Internal.LocalPool[TokenDecoder]())

  private[scalanotation] def sharedContext(capacityHint: Int): PoolHolder =
    given Internal.Alloc[TokenDecoder] = PoolDecoderAlloc
    PoolHolder.RealPoolHolder(Internal.SharedPool[TokenDecoder](capacityHint))

  private inline def withPooled[A](ctx: PoolHolder, input: String, debug: Boolean)(
      inline use: TokenDecoder => A
  ): A =
    def useDecoder(decoder: TokenDecoder): A = use(decoder)
    ctx match
      case PoolHolder.RealPoolHolder(pool) =>
        pool.withBorrowed { decoder =>
          useDecoder(decoder.reset(input, debug))
        }
      case PoolHolder.NoPoolHolder => useDecoder(TokenDecoder.oneShot(input, debug))

  private[scalanotation] def decode[T](
      input: String,
      debugTokens: Boolean,
      rootName: String,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeRoot(decoder, rootName, packageName))

  private[scalanotation] def decodeAnyRoot[T](
      input: String,
      debugTokens: Boolean,
      packageName: String,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[Expr.SourceFile[T], DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeAnyRoot(decoder, packageName))

  private[scalanotation] def decodeExpression[T](
      input: String,
      debugTokens: Boolean,
      decoder: Reader[T]
  )(using ctx: PoolHolder): Result[T, DecodeError] =
    catchingTokenErrors(input):
      withPooled(ctx, input, debugTokens)(_.decodeExpression(decoder))

  /** tokens are scanned lazily while decoding, so malformed input can surface anywhere in the
    * decode as a [[TokenizeException]] — converted to a [[DecodeError]] here.
    */
  private inline def catchingTokenErrors[A](input: String)(
      inline body: => Result[A, DecodeError]
  ): Result[A, DecodeError] =
    try body
    catch
      case e: TokenizeException =>
        Result.Err(DecodeError.TokenFormat(e.message).atToken(Tokenizer.spanAt(input, e.offset)))

  private[scalanotation] def isNullable(schema: RawSchema): Boolean =
    schema match
      case RawSchema.Option(_)       => true
      case RawSchema.Mapped(base, _) => isNullable(base)
      case _                         => false

  private[scalanotation] def describe(expr: Expr): String =
    expr match
      case Expr.VectorExpr(_)       => "Vector(...)"
      case Expr.TupleExpr(elements) =>
        RawSchema.describeTupleSlots(elements.length)
      case Expr.NamedTupleExpr(fieldExprs) =>
        if fieldExprs.isEmpty then "NamedTuple.Empty" else s"(${fieldExprs.head.name} = ...)"
      case Expr.StringConstant(value)  => s"""("$value": String)"""
      case Expr.CharConstant(value)    => s"('$value': Char)"
      case Expr.IntConstant(value)     => s"($value: Int)"
      case Expr.LongConstant(value)    => s"($value: Long)"
      case Expr.FloatConstant(value)   => s"($value: Float)"
      case Expr.DoubleConstant(value)  => s"($value: Double)"
      case Expr.BooleanConstant(value) => s"($value: Boolean)"
      case Expr.NullConstant           => "null"

/** Reusable buffer for named-tuple parse results: the closing token is recorded as an unboxed Int
  * offset; a Span is only materialized if an error is constructed.
  */
private final class NamedTupleParseResult() {
  var fieldCount: Int          = uninitialized
  var fieldName: String | Null = uninitialized
  var closingOffset: Int       = uninitialized

  def push(fieldCount: Int, fieldName: String | Null, closingOffset: Int): this.type =
    this.fieldCount = fieldCount
    this.fieldName = fieldName
    this.closingOffset = closingOffset
    this
}

private final class TokenDecoder private (
    input: String,
    debug: Boolean,
    private val slotsPooling: Boolean,
    scanOnInit: Boolean
) extends TokenStream(input, debug, cacheNamesOnInit = slotsPooling, scanOnInit) {
  def this(input: String, debug: Boolean) =
    this(input, debug, slotsPooling = true, scanOnInit = true)

  import scala.util.boundary.Label

  type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  /** Clears decode state and re-aims at a new input, for reuse via [[TokenDecoder.withPooled]]. */
  def reset(input: String, debug: Boolean): this.type =
    resetStream(input, debug)
    resetSlots()
    namedTupleParseResult.fieldName = null
    this

  private def expectedTypeAtCurrent(schema: RawSchema): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  // pooled builder slots for product-like schemas with a slots factory; borrow/release pairs
  // nest, so a reentrant decode inside a field value borrows a fresh instance. Lazy: a one-shot
  // (GC-pooled) decoder skips the slots path and must not pay for the pool either.
  private lazy val slotsPool = Internal.LocalPool[BuilderSlots]()

  private inline def withRead[T, S <: RawSchema, R](
      schema: S,
      inline r: S => R | Null
  )(inline f: R => T): T =
    val read = r(schema)
    if read == null then missingReadCapability(schema)
    else f(read.nn)

  private inline def withBorrowSlots[T](
      factory: scalanotation.TypedFactory.OfProduct[?] | Null
  )(inline f: (BuilderSlots | Null) => T): T =
    def useSlots(slots: BuilderSlots | Null): T =
      f(slots)
    if !slotsPooling || factory == null then useSlots(null)
    else slotsPool.withBorrowed(useSlots)

  def decodeRoot[T](
      schema: Reader[T],
      rootName: String,
      packageName: String
  ): Result[T, DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      expectVal().check
      expectIdentifier().check
      val declaredName = pullStringStrict()
      if declaredName != rootName then raise(DecodeError.UnexpectedRoot(declaredName))
      expectEquals().check
      decodeBase(schema.schema).check
      val value = pullAny()
      expectEof().check
      value.asInstanceOf[T]

  def decodeAnyRoot[T](
      schema: Reader[T],
      packageName: String
  ): Result[Expr.SourceFile[T], DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      expectVal().check
      expectIdentifier().check
      val declaredName = pullStringStrict()
      expectEquals().check
      decodeBase(schema.schema).check
      val value = pullAny().asInstanceOf[T]
      expectEof().check
      Expr.SourceFile(Map(declaredName -> value))

  def decodeExpression[T](schema: Reader[T]): Result[T, DecodeError] =
    Result:
      decodeBase(schema.schema).check
      val value = pullAny()
      expectEof().check
      value.asInstanceOf[T]

  private def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private def decodeBase(schema: RawSchema): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapSlot(mapped.mapping, decodeBase(mapped.base))
      case sc: RawSchema.NamedTuple =>
        decodeNamedTuple(sc)
      case sc: RawSchema.Tuple =>
        decodeTuple(sc)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        decodePartialNamedTuple(base, alreadySeenField)
      case sc: RawSchema.Sum =>
        decodeSum(sc)
      case sc: RawSchema.DiscriminatorSum =>
        decodeDiscriminatorSum(sc)
      case sc: RawSchema.Vector =>
        decodeVector(sc)
      case sc: RawSchema.Dict =>
        decodeDict(sc)
      case sc: RawSchema.Option =>
        decodeOption(sc)
      case RawSchema.AnyExpr =>
        decodeAnyExpr()
      case RawSchema.String =>
        decodeString()
      case RawSchema.Char =>
        decodeChar()
      case RawSchema.Int =>
        decodeInt()
      case RawSchema.Long =>
        decodeLong()
      case RawSchema.Float =>
        decodeFloat()
      case RawSchema.Double =>
        decodeDouble()
      case RawSchema.Boolean =>
        decodeBoolean()
      case RawSchema.Null =>
        decodeNull()

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple
  ): Result[Unit, DecodeError] = namesPool.withBorrowed { seenNames =>
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { slots =>
        Result.task {
          schema.isValidNamedTuple(namesPool).check
          val fields = schema.fields

          var state: read.State = read.init(fields.length, slots)
          var fieldIndex        = 0

          val allowEmpty =
            fields.isEmpty // FIXME: must be hoisted to allow inlining parseNamedTupleStructure!

          val parsed = parseNamedTupleStructure(schema, allowEmpty = allowEmpty) {
            (actualName, nameOffset, parsedFieldIndex) =>
              def actualFieldErr(err: DecodeError): DecodeError =
                err.atPath(s".${actualName}").atToken(spanAt(nameOffset))
              val validated: DecodeError | Field = {
                if seenNames.alreadySeen(actualName) then
                  actualFieldErr(DecodeError.DuplicateField(actualName))
                else if schema.allowSkippedNullableFields then
                  val expectedBeforeSkip =
                    if fieldIndex < fields.length then fields(fieldIndex) else null
                  state = fillSkippedNullableFields(read)(fields, state, fieldIndex, actualName)
                  fieldIndex = pullSkipFillIndex()

                  if fieldIndex >= fields.length then
                    if expectedBeforeSkip == null then
                      actualFieldErr(
                        DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                      )
                    else
                      actualFieldErr(
                        DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
                      )
                  else
                    val expectedField = fields(fieldIndex)
                    if actualName != expectedField.name then
                      actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                    else expectedField
                else if parsedFieldIndex >= fields.length then
                  actualFieldErr(
                    DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                  )
                else
                  val expectedField = fields(parsedFieldIndex)
                  if actualName != expectedField.name then
                    actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                  else expectedField
              }
              validated match
                case expectedField: Field =>
                  checkOrRaise(decodeBase(expectedField.schema))(actualFieldErr)
                  state = addSlot(read)(state, fieldIndex)
                  fieldIndex += 1
                case err: DecodeError => raise(err)
          }

          if schema.allowSkippedNullableFields && fields.nonEmpty && parsed.fieldCount == 0 then
            raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))

          if schema.allowSkippedNullableFields then
            state = fillSkippedNullableFields(read)(fields, state, fieldIndex, "")
            fieldIndex = pullSkipFillIndex()

          val decodedFieldCount =
            if schema.allowSkippedNullableFields then fieldIndex else parsed.fieldCount
          if decodedFieldCount != fields.length then
            def err =
              var err0 = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
              if parsed.fieldName != null then err0 = err0.atPath(s".${parsed.fieldName}")
              err0.atToken(spanAt(parsed.closingOffset))
            raise(err)

          pushRef(read.finish(state))
        }
      }
    }
  }

  private def decodeTuple(schema: RawSchema.Tuple): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { pooled =>
        Result.task {
          val slots             = schema.slots
          var state: read.State = read.initPooled(slots.length, pooled)
          currentKind() match
            case TokenKind.EmptyTupleId =>
              val emptyTupleOffset = currentOffset()
              advance()
              if slots.nonEmpty then
                raise(
                  DecodeError.FieldCountMismatch(slots.length, 0).atToken(spanAt(emptyTupleOffset))
                )
            case TokenKind.LParen =>
              decodeParenthesizedTuple(read)(schema, slots, state).check
              state = pullAny().asInstanceOf[read.State]
            case _ =>
              if slots.isEmpty then
                raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()))
              decodeTupleSlotValue(slots, index = 0, allowStringConcat = false).check
              state = addSlot(read)(state, 0)
              decodeTupleConsTail(read)(slots, state, startIndex = 1).check
              state = pullAny().asInstanceOf[read.State]
          pushRef(read.finish(state))
        }
      }
    }

  /** decodes the tuple tail, leaving the final builder state in the Any slot */
  private def decodeParenthesizedTuple(
      read: RawSchema.TupleRead
  )(
      schema: RawSchema.Tuple,
      slots: IArray[RawSchema],
      state: read.State
  ): Result[Unit, DecodeError] =
    Result.task:
      val separators   = parenthesizedTupleSeparators()
      val hasComma     = (separators & SeparatorComma) != 0
      val hasStarColon = (separators & SeparatorStarColon) != 0
      advanceTupleOpen(schema).check
      currentKind() match
        case TokenKind.RParen =>
          raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))
        case _ =>
          if slots.isEmpty then raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))
          decodeTupleSlotValue(slots, index = 0, hasComma || !hasStarColon).check
          val stateAfterFirst = addSlot(read)(state, 0)
          currentKind() match
            case TokenKind.Comma =>
              decodeTupleCommaTail(read)(slots, stateAfterFirst, startCount = 1).check
            case TokenKind.StarColon =>
              decodeTupleConsTail(read)(slots, stateAfterFirst, startIndex = 1).check
              val stateAfterTail = pullAny()
              currentKind() match
                case TokenKind.RParen =>
                  advanceTupleClose().check
                  pushRef(stateAfterTail)
                case _ =>
                  raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
            case TokenKind.RParen =>
              val rparenOffset = currentOffset()
              advanceTupleClose().check
              currentKind() match
                case TokenKind.StarColon =>
                  decodeTupleConsTail(read)(slots, stateAfterFirst, startIndex = 1).check
                case _ =>
                  raise(
                    DecodeError
                      .ExpectedType(schema.describeSelf, "(...)")
                      .atToken(spanAt(rparenOffset))
                  )
            case _ =>
              raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  /** decodes the comma-separated tuple tail, leaving the final builder state in the Any slot */
  private def decodeTupleCommaTail(
      read: RawSchema.TupleRead
  )(
      slots: IArray[RawSchema],
      state0: read.State,
      startCount: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      var state              = state0
      var count              = startCount
      var done               = false
      var closingOffset: Int = currentOffset()
      while !done do
        currentKind() match
          case TokenKind.Comma =>
            advanceTupleComma().check
            currentKind() match
              case TokenKind.RParen =>
                closingOffset = currentOffset()
                if count == 1 then
                  raise(DecodeError.FieldCountMismatch(2, 1).atToken(spanAt(closingOffset)))
                done = true
              case _ =>
                if count >= slots.length then
                  raise(
                    DecodeError
                      .FieldCountMismatch(slots.length, count + 1)
                      .atToken(currentSpan())
                  )
                decodeTupleSlotValue(slots, count, allowStringConcat = true).check
                state = addSlot(read)(state, count)
                count += 1
          case TokenKind.RParen =>
            closingOffset = currentOffset()
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      advanceTupleClose().check
      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(spanAt(closingOffset)))
      if count != slots.length then
        raise(DecodeError.FieldCountMismatch(slots.length, count).atToken(spanAt(closingOffset)))
      pushRef(state)

  /** decodes the `*:`-separated tuple tail, leaving the final builder state in the Any slot */
  private def decodeTupleConsTail(
      read: RawSchema.TupleRead
  )(
      slots: IArray[RawSchema],
      state0: read.State,
      startIndex: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      var state = state0
      var index = startIndex
      var done  = false
      while !done do
        advanceTupleConsSeparator().check
        currentKind() match
          case TokenKind.EmptyTupleId =>
            advanceTupleEmptyTail(slots.length, index).check
            done = true
          case _ =>
            if index >= slots.length then
              raise(
                DecodeError
                  .FieldCountMismatch(slots.length, index + 1)
                  .atToken(currentSpan())
              )
            decodeTupleSlotValue(slots, index, allowStringConcat = false).check
            state = addSlot(read)(state, index)
            index += 1
      pushRef(state)

  private def decodeTupleSlotValue(
      slots: IArray[RawSchema],
      index: Int,
      allowStringConcat: Boolean
  ): Result[Unit, DecodeError] =
    decodeTupleElement(slots(index), allowStringConcat) match
      case Result.Err(error) => Result.Err(error.atPath(s"[$index]"))
      case ok                => ok

  private def advanceTupleOpen(schema: RawSchema.Tuple): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )

  private def advanceTupleClose(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def advanceTupleComma(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.Comma then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def advanceTupleConsSeparator(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.StarColon then advance()
      else raise(DecodeError.ExpectedType("'*:'", describeCurrent()).atToken(currentSpan()))

  private def advanceTupleEmptyTail(
      expectedSlots: Int,
      actualSlots: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.EmptyTupleId then
        val emptyTupleOffset = currentOffset()
        advance()
        if actualSlots != expectedSlots then
          raise(
            DecodeError
              .FieldCountMismatch(expectedSlots, actualSlots)
              .atToken(spanAt(emptyTupleOffset))
          )
      else raise(DecodeError.ExpectedType("'EmptyTuple'", describeCurrent()).atToken(currentSpan()))

  private def decodeTupleElement(
      schema: RawSchema,
      allowStringConcat: Boolean
  ): Result[Unit, DecodeError] =
    schema match
      case RawSchema.Mapped(base, mapping) =>
        mapSlot(mapping, decodeTupleElement(base, allowStringConcat))
      case opt @ RawSchema.Option(inner) =>
        if currentKind() == TokenKind.NullKw then decodeOption(opt)
        else
          val r = decodeTupleElement(inner, allowStringConcat)
          if r.isOk then pushRef(Some(pullAny()))
          r
      case RawSchema.String if !allowStringConcat =>
        decodeStringAtom()
      case _ if currentKind() == TokenKind.LParen && !canDecodeFromLParen(schema) =>
        decodeGroupedTupleElement(schema)
      case _ =>
        decodeBase(schema)

  private def decodeGroupedTupleElement(schema: RawSchema): Result[Unit, DecodeError] =
    Result.task {
      if currentKind() == TokenKind.LParen then advance()
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      decodeTupleElement(schema, allowStringConcat = true).check
      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
    }

  private def canDecodeFromLParen(schema: RawSchema): Boolean =
    schema match
      case RawSchema.Mapped(base, _)      => canDecodeFromLParen(base)
      case RawSchema.Option(inner)        => canDecodeFromLParen(inner)
      case _: RawSchema.NamedTuple        => true
      case _: RawSchema.Tuple             => true
      case _: RawSchema.PartialNamedTuple => true
      case _: RawSchema.Sum               => true
      case _: RawSchema.DiscriminatorSum  => true
      case _: RawSchema.Dict              => true
      case RawSchema.AnyExpr              => true
      case _                              => false

  // bit flags of parenthesizedTupleSeparators — packed into an Int so no tuple is boxed per decode
  private final val SeparatorComma     = 1
  private final val SeparatorStarColon = 2

  /** Scans ahead (without buffering) from the current '(' to its matching ')' to discover which
    * separators the parenthesized tuple uses, returned as a bitmask of [[SeparatorComma]] and
    * [[SeparatorStarColon]]. Uses a scout scanner so the bounded token buffer of the stream is
    * preserved; no tokens are materialized.
    */
  private def parenthesizedTupleSeparators(): Int =
    var depth      = 0
    var sawOpen    = false
    var done       = false
    var separators = 0
    val scout      = scoutFromCurrent()
    while !done do
      scout.scanNext()
      scout.kind match
        case TokenKind.LParen =>
          depth += 1
          sawOpen = true
        case TokenKind.RParen if sawOpen =>
          depth -= 1
          if depth == 0 then done = true
        case TokenKind.Comma if depth == 1 =>
          separators |= SeparatorComma
        case TokenKind.StarColon if depth == 1 =>
          separators |= SeparatorStarColon
        case TokenKind.Eof =>
          done = true
        case _ => ()
    separators

  private def decodeSum(schema: RawSchema.Sum): Result[Unit, DecodeError] =
    Result.task {
      val parsed = parseNamedTupleStructure(schema, allowEmpty = false) {
        (actualName, nameOffset, fieldIndex) =>
          if fieldIndex >= 1 then
            raise(
              DecodeError
                .FieldCountMismatch(1, fieldIndex + 1)
                .atPath(s".${actualName}")
                .atToken(spanAt(nameOffset))
            )
          else
            val sumCase = RawSchema.findCase(schema.cases, actualName) match
              case null =>
                raise(
                  DecodeError
                    .UnexpectedField(actualName)
                    .atPath(s".${actualName}")
                    .atToken(spanAt(nameOffset))
                )
              case c => c
            checkOrRaise(decodeBase(sumCase.schema))(_.atPath(s".${actualName}"))
      }
      if parsed.fieldCount != 1 then
        var err = DecodeError.FieldCountMismatch(1, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(spanAt(parsed.closingOffset)))
      // the decoded case value remains in the live slot
    }

  private def decodeDiscriminatorSum(
      schema: RawSchema.DiscriminatorSum
  ): Result[Unit, DecodeError] =
    Result.task {
      val discriminatorField = schema.discriminatorField
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      val nameOffset = currentOffset()
      parseNamedFieldStart().check
      val actualName = pullStringStrict()
      if actualName != discriminatorField then
        raise(
          DecodeError
            .FieldOrderMismatch(discriminatorField, actualName)
            .atPath(s".$actualName")
            .atToken(spanAt(nameOffset))
        )

      checkOrRaise(decodeString())(_.atPath(s".$actualName"))
      val caseName = pullStringStrict()
      val sumCase  = RawSchema.findCase(schema.cases, caseName) match
        case null =>
          raise(
            DecodeError
              .UnexpectedField(caseName)
              .atPath(s".$actualName")
              .atToken(spanAt(nameOffset))
          )
        case c => c

      currentKind() match
        case TokenKind.Comma  => advance()
        case TokenKind.RParen =>
        case _                =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      decodeBase(sumCase.schema).check
    }

  private def decodePartialNamedTuple(
      schema: RawSchema,
      alreadySeenField: String
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapSlot(mapped.mapping, decodePartialNamedTuple(mapped.base, alreadySeenField))
      case namedTuple: RawSchema.NamedTuple =>
        decodePartialNamedTuple(namedTuple, alreadySeenField)
      case RawSchema.Null =>
        decodeEmptyPartialNamedTuple()
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeCurrent()))

  private def decodeEmptyPartialNamedTuple(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.RParen then
        advance()
        pushRef(null)
      else raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))

  private def decodePartialNamedTuple(
      schema: RawSchema.NamedTuple,
      alreadySeenField: String
  ): Result[Unit, DecodeError] = namesPool.withBorrowed { seenNames =>
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { slots =>
        Result.task {
          schema.isValidNamedTuple(namesPool).check
          seenNames.alreadySeen(alreadySeenField)
          val fields            = schema.fields
          var state: read.State = read.init(fields.length, slots)
          var fieldIndex        = 0

          val parsed = parsePartialNamedTupleStructure(schema) {
            (actualName, nameOffset, parsedFieldIndex) =>
              def actualFieldErr(err: DecodeError): DecodeError =
                err.atPath(s".${actualName}").atToken(spanAt(nameOffset))
              val validated: DecodeError | Field = {
                if seenNames.alreadySeen(actualName) then
                  actualFieldErr(DecodeError.DuplicateField(actualName))
                else if schema.allowSkippedNullableFields then
                  val expectedBeforeSkip =
                    if fieldIndex < fields.length then fields(fieldIndex) else null
                  state = fillSkippedNullableFields(read)(fields, state, fieldIndex, actualName)
                  fieldIndex = pullSkipFillIndex()
                  val fiLocal = fieldIndex
                  eval {
                    if fiLocal >= fields.length then
                      if expectedBeforeSkip == null then
                        actualFieldErr(
                          DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                        )
                      else
                        actualFieldErr(
                          DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
                        )
                    else
                      val expectedField = fields(fiLocal)
                      if actualName != expectedField.name then
                        actualFieldErr(
                          DecodeError.FieldOrderMismatch(expectedField.name, actualName)
                        )
                      else expectedField
                  }
                else if parsedFieldIndex >= fields.length then
                  eval {
                    actualFieldErr(
                      DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                    )
                  }
                else
                  eval {
                    val expectedField = fields(parsedFieldIndex)
                    if actualName != expectedField.name then
                      actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                    else expectedField
                  }
              }
              validated match
                case expectedField: Field =>
                  checkOrRaise(decodeBase(expectedField.schema))(actualFieldErr)
                  state = addSlot(read)(state, fieldIndex)
                  fieldIndex += 1
                case err: DecodeError => raise(err)
          }

          if schema.allowSkippedNullableFields then
            state = fillSkippedNullableFields(read)(fields, state, fieldIndex, "")
            fieldIndex = pullSkipFillIndex()

          val decodedFieldCount =
            if schema.allowSkippedNullableFields then fieldIndex else parsed.fieldCount
          if decodedFieldCount != fields.length then
            var err = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
            if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
            raise(err.atToken(spanAt(parsed.closingOffset)))

          pushRef(read.finish(state))
        }
      }
    }
  }

  private def decodeVector(schema: RawSchema.Vector): Result[Unit, DecodeError] =
    Result.task {
      if schema.read == null then missingReadCapability(schema)
      val read   = schema.read.nn
      var values = read.init()
      parseVectorStructure(schema) { indexInVector =>
        checkOrRaise(decodeBase(schema.element))(_.atPath(s"[$indexInVector]"))
        values = addSlot(read)(values)
      }
      pushRef(read.finish(values))
    }

  private def decodeDict(schema: RawSchema.Dict): Result[Unit, DecodeError] =
    namesPool.withBorrowed { seenNames =>
      Result.task {
        if schema.read == null then missingReadCapability(schema)
        val read   = schema.read.nn
        var state  = read.init()
        val parsed = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameOffset, _) =>
          if seenNames.alreadySeen(name) then
            raise(DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset)))
          checkOrRaise(decodeBase(schema.element))(_.atPath(s".${name}"))
          state = addSlot(read)(state, name)
        }
        val _ = parsed.closingOffset
        val _ = parsed.fieldName
        val _ = parsed.fieldCount
        pushRef(read.finish(state))
      }
    }

  private def decodeOption(schema: RawSchema.Option): Result[Unit, DecodeError] =
    Result.task {
      if currentKind() == TokenKind.NullKw then
        advance()
        pushRef(None)
      else
        decodeBase(schema.inner).check
        pushRef(Some(pullAny()))
    }

  private object exprVisitor:
    private val AnyNamedTupleSchema: RawSchema.NamedTuple =
      RawSchema.NamedTuple(IArray.empty[RawSchema.Field], read = null, write = null)
    private val AnyTupleSchema: RawSchema.Tuple =
      RawSchema.Tuple(IArray.empty[RawSchema], read = null, write = null)
    private val AnyVectorSchema: RawSchema.Vector =
      RawSchema.Vector(
        RawSchema.AnyExpr,
        read = null,
        write = null
      )
    private val EmptyTupleExpr: Expr =
      Expr.TupleExpr(Vector.empty)

    /** parses an expression, pushing the resulting [[Expr]] into the Any slot */
    def inferExpr(): Result[Unit, DecodeError] =
      onStringConcat()

    private def pulledExpr(): Expr = pullAny().asInstanceOf[Expr]

    private def onStringConcat(): Result[Unit, DecodeError] = Result.task {
      onTupleCons().check
      if currentKind() == TokenKind.Plus then
        val builder = pulledExpr() match
          case Expr.StringConstant(value) => new StringBuilder ++= value
          case other                      =>
            raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
        while currentKind() == TokenKind.Plus do
          advance()
          onTupleCons().check
          pulledExpr() match
            case Expr.StringConstant(value) => builder ++= value
            case other                      =>
              raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
        pushRef(Expr.StringConstant(builder.result()))
    }

    private def onTupleCons(): Result[Unit, DecodeError] = Result.task {
      onPrimary().check
      if currentKind() == TokenKind.StarColon then
        val head = pulledExpr()
        advance()
        onTupleCons().check
        pulledExpr() match
          case Expr.TupleExpr(elements) =>
            pushRef(Expr.TupleExpr(head +: elements))
          case other =>
            raise(DecodeError.ExpectedType("Tuple", describe(other)))
    }

    private def onPrimary(): Result[Unit, DecodeError] =
      currentKind() match
        case TokenKind.LParen                     => onParenthesized()
        case TokenKind.VectorId                   => onVector(AnyVectorSchema)
        case TokenKind.EmptyTupleId               => onEmptyTuple()
        case TokenKind.StringLit                  => onString()
        case TokenKind.CharLit                    => onChar()
        case TokenKind.IntLit                     => onInt()
        case TokenKind.LongLit                    => onLong()
        case TokenKind.FloatLit                   => onFloat()
        case TokenKind.DoubleLit                  => onDouble()
        case TokenKind.TrueKw | TokenKind.FalseKw => onBoolean()
        case TokenKind.NullKw                     => onNull()
        case TokenKind.Minus                      =>
          peekKind() match
            case TokenKind.IntLit    => onInt()
            case TokenKind.LongLit   => onLong()
            case TokenKind.FloatLit  => onFloat()
            case TokenKind.DoubleLit => onDouble()
            case _                   =>
              Result.Err(DecodeError.ExpectedType("Number", describePeek()).atToken(peekSpan()))
        case _ =>
          Result.Err(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

    def onParenthesized(): Result[Unit, DecodeError] = Result.task {
      if currentKind() == TokenKind.LParen then advance()
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      if peekKind() == TokenKind.Equals then onNamedTupleAfterOpen(AnyNamedTupleSchema).check
      else
        inferExpr().check
        currentKind() match
          case TokenKind.Comma =>
            onTupleAfterGroupedHead(pulledExpr()).check
          case TokenKind.RParen =>
            advance()
            // the grouped expression remains in the Any slot
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
    }

    def onTupleAfterGroupedHead(first: Expr): Result[Unit, DecodeError] = Result.task {
      val elements = IArray.newBuilder[Expr]
      elements += first
      var count = 1
      var done  = false
      while !done do
        currentKind() match
          case TokenKind.Comma =>
            advance()
            currentKind() match
              case TokenKind.RParen =>
                if count == 1 then
                  raise(DecodeError.FieldCountMismatch(2, 1).atToken(currentSpan()))
                done = true
              case _ =>
                inferExpr().check
                elements += pulledExpr()
                count += 1
          case TokenKind.RParen =>
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1))
      pushRef(Expr.TupleExpr(elements.result()))
    }

    def onNamedTuple(schema: RawSchema.NamedTuple): Result[Unit, DecodeError] = Result.task {
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )
      onNamedTupleAfterOpen(schema).check
    }

    def onNamedTupleAfterOpen(schema: RawSchema.NamedTuple): Result[Unit, DecodeError] =
      namesPool.withBorrowed { seenNames =>
        Result.task {
          val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
          val allowEmpty = false
          val parsed     =
            parseNamedTupleStructureAfterOpen(schema, allowEmpty) { (name, nameOffset, _) =>
              if seenNames.alreadySeen(name) then
                raise(
                  DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset))
                )
              checkOrRaise(inferExpr())(_.atPath(s".${name}"))
              fieldExprs += ((name, pulledExpr()))
            }
          val _ = parsed.closingOffset
          val _ = parsed.fieldName
          val _ = parsed.fieldCount
          pushRef(Expr.NamedTupleExpr(fieldExprs.result()))
        }
      }

    def onVector(schema: RawSchema.Vector): Result[Unit, DecodeError] = Result.task {
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure(schema) { _ =>
        inferExpr().check
        elements += pulledExpr()
      }
      pushRef(Expr.VectorExpr(elements.result()))
    }

    def onString(): Result[Unit, DecodeError] = Result.task:
      decodeString().check
      pushRef(Expr.StringConstant(pullStringStrict()))

    def onChar(): Result[Unit, DecodeError] = Result.task:
      decodeChar().check
      pushRef(Expr.CharConstant(pullCharStrict()))

    def onInt(): Result[Unit, DecodeError] = Result.task:
      decodeInt().check
      pushRef(Expr.IntConstant(pullIntStrict()))

    def onLong(): Result[Unit, DecodeError] = Result.task:
      decodeLong().check
      pushRef(Expr.LongConstant(pullLongStrict()))

    def onFloat(): Result[Unit, DecodeError] = Result.task:
      decodeFloat().check
      pushRef(Expr.FloatConstant(pullFloatStrict()))

    def onDouble(): Result[Unit, DecodeError] = Result.task:
      decodeDouble().check
      pushRef(Expr.DoubleConstant(pullDoubleStrict()))

    def onBoolean(): Result[Unit, DecodeError] = Result.task:
      decodeBoolean().check
      pushRef(Expr.BooleanConstant(pullBooleanStrict()))

    def onNull(): Result[Unit, DecodeError] = Result.task:
      decodeNull().check
      val _ = pullRefStrict().ensuring(_ == null, "Expected null in Any slot after decoding Null")
      pushRef(Expr.NullConstant)

    def onEmptyTuple(): Result[Unit, DecodeError] = Result.task:
      if currentKind() == TokenKind.EmptyTupleId then
        advance()
        pushRef(EmptyTupleExpr)
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

  /** parses an expression, pushing the resulting [[Expr]] into the Any slot */
  private[scalanotation] def decodeAnyExpr(): Result[Unit, DecodeError] =
    exprVisitor.inferExpr()

  // reusable (per-decoder) buffer for named-tuple parse results — no allocation in the happy path
  private val namedTupleParseResult = new NamedTupleParseResult()

  @deprecated("Kept for binary compatibility; will be removed in a future version", "0.3.6")
  private class NamedTupleParseResultBuf() {
    // retained only for binary compatibility — superseded by NamedTupleParseResult above
    var fieldCount: Int               = uninitialized
    var fieldName: String | Null      = uninitialized
    var closingSpan: DecodeError.Span = uninitialized
  }

  @deprecated("Kept for binary compatibility; will be removed in a future version", "0.3.6")
  private object NamedTupleParseResultBuf extends NamedTupleParseResultBuf() {
    def push(fieldCount: Int, fieldName: String | Null, closingSpan: DecodeError.Span): this.type =
      this.fieldCount = fieldCount
      this.fieldName = fieldName
      this.closingSpan = closingSpan
      this
  }

  private inline def eval[T](inline op: T): T =
    def exprToEval(): T = op
    exprToEval()

  /** parses `<name> =`, pushing the field name into [[stringSlot]] */
  private def parseNamedFieldStart(): Result[Unit, DecodeError] =
    Result.task:
      val actualName = currentKind() match
        case TokenKind.Identifier   => currentName()
        case TokenKind.VectorId     => "Vector"
        case TokenKind.EmptyTupleId => "EmptyTuple"
        case TokenKind.Plus         => "+"
        case TokenKind.Minus        => "-"
        case TokenKind.StarColon    => "*:"
        case _                      =>
          raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))
      advance()
      if currentKind() == TokenKind.Equals then
        advance()
        pushString(actualName)
      else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  private inline def parseNamedTupleStructure(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    if currentKind() == TokenKind.LParen then advance()
    else
      raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan()))

    // parseNamedTupleStructureAfterOpen(schema, allowEmpty)(consumeFieldValue)
    val parsed: NamedTupleParseResult =
      parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
        case parsed: NamedTupleParseResult => parsed
        case err: Result.Err[DecodeError]  =>
          scala.util.boundary.break(err) // TODO: replace with Result.breakErr

    if !allowEmpty && parsed.fieldCount == 0 then
      raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))
    parsed
  }

  private inline def parseNamedTupleStructureAfterOpen(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    val parsed: NamedTupleParseResult =
      parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
        case parsed: NamedTupleParseResult => parsed
        case err: Result.Err[DecodeError]  =>
          scala.util.boundary.break(err) // TODO: replace with Result.breakErr

    if !allowEmpty && parsed.fieldCount == 0 then
      raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))
    parsed
  }

  private inline def parsePartialNamedTupleStructure(
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = {
    parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
      case parsed: NamedTupleParseResult => parsed
      case err: Result.Err[DecodeError]  =>
        scala.util.boundary.break(err) // TODO: replace with Result.breakErr
  }

  private inline def parsePartialNamedTupleStructureInner(
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): NamedTupleParseResult | Result.Err[DecodeError] =
    // to share the logic without breaking the label optimisation, we need to cache the result and
    // then redispatch the break at the call-site. i.e. nested inline calls dont seem to compose
    // well enough to pass along the label. i would like to investigate why.
    {
      scala.util.boundary {
        import Internal.loop

        currentKind() match {
          case TokenKind.RParen =>
            val closingOffset = currentOffset()
            advance()
            namedTupleParseResult.push(0, null, closingOffset)
          case _ =>
            var fieldIndex: Int              = 0
            var lastFieldName: String | Null = null
            val closingOffset: Int           = loop {
              val nameOffset = currentOffset()
              parseNamedFieldStart().check
              val actualName = pullStringStrict()
              consumeFieldValue(actualName, nameOffset, fieldIndex)
              lastFieldName = actualName
              fieldIndex += 1

              currentKind() match
                case TokenKind.Comma =>
                  advance()
                  if currentKind() == TokenKind.RParen then loop.break(currentOffset())
                case TokenKind.RParen =>
                  loop.break(currentOffset())
                case _ =>
                  raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
            }
            advance()
            namedTupleParseResult.push(fieldIndex, lastFieldName, closingOffset)
        }
      }
    }

  private inline def parseVectorStructure(schema: RawSchema)(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    if currentKind() == TokenKind.VectorId && peekKind() == TokenKind.LParen then
      advance()
      advance()
    else
      raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan()))

    var indexInVector = 0

    if currentKind() == TokenKind.RParen then advance()
    else
      var done = false
      while !done do
        consumeElementValue(indexInVector)
        indexInVector += 1

        currentKind() match
          case TokenKind.Comma =>
            advance()
            if currentKind() == TokenKind.RParen then done = true
          case TokenKind.RParen => done = true
          case _                =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
  }

  /** decodes a string (with `+` concatenation), pushing the value into [[stringSlot]] */
  private[scalanotation] def decodeString(): Result[Unit, DecodeError] =
    Result.task {
      decodeStringAtom().check
      if currentKind() == TokenKind.Plus then
        val builder = StringBuilder() ++= pullStringStrict()
        while currentKind() == TokenKind.Plus do
          advance()
          decodeStringAtom().check
          builder ++= pullStringStrict()
        pushString(builder.result())
    }

  private def decodeStringAtom(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.StringLit then
      pushString(currentStringValue())
      advance()
    else raise(expectedTypeAtCurrent(RawSchema.String))

  private[scalanotation] def decodeChar(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.CharLit then
      pushChar(currentCharValue())
      advance()
    else raise(expectedTypeAtCurrent(RawSchema.Char))

  private[scalanotation] def decodeInt(): Result[Unit, DecodeError] = Result.task:
    decodeSigned[Int](
      literal = negative =>
        currentKind() match
          case TokenKind.IntLit => currentIntValue(negative)
          case _                => raise(expectedTypeAtCurrent(RawSchema.Int)),
      store = v => pushInt(v)
    )

  private[scalanotation] def decodeLong(): Result[Unit, DecodeError] = Result.task:
    decodeSigned[Long](
      literal = negative =>
        currentKind() match
          case TokenKind.LongLit => currentLongValue(negative)
          case TokenKind.IntLit  => currentIntValue(negative).toLong
          case _                 => raise(expectedTypeAtCurrent(RawSchema.Long)),
      store = v => pushLong(v)
    )

  private[scalanotation] def decodeFloat(): Result[Unit, DecodeError] = Result.task:
    decodeSigned[Float](
      literal = negative =>
        currentKind() match
          case TokenKind.FloatLit =>
            val magnitude = currentFloatValue()
            if negative then -magnitude else magnitude
          case TokenKind.IntLit =>
            val value = currentIntValue(negative)
            if NumericPromotions.isExactFloat(value) then value.toFloat
            else raise(expectedTypeAtCurrent(RawSchema.Float))
          case _ => raise(expectedTypeAtCurrent(RawSchema.Float)),
      store = v => pushFloat(v)
    )

  private[scalanotation] def decodeDouble(): Result[Unit, DecodeError] = Result.task:
    decodeSigned[Double](
      literal = negative =>
        currentKind() match
          case TokenKind.DoubleLit =>
            val magnitude = currentDoubleValue()
            if negative then -magnitude else magnitude
          case TokenKind.IntLit => currentIntValue(negative).toDouble
          case _                => raise(expectedTypeAtCurrent(RawSchema.Double)),
      store = v => pushDouble(v)
    )

  private[scalanotation] def decodeBoolean(): Result[Unit, DecodeError] =
    Result.task:
      currentKind() match
        case TokenKind.TrueKw =>
          advance()
          pushBoolean(true)
        case TokenKind.FalseKw =>
          advance()
          pushBoolean(false)
        case _ =>
          raise(expectedTypeAtCurrent(RawSchema.Boolean))

  private[scalanotation] def decodeNull(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.NullKw then
      advance()
      pushRef(null)
    else raise(expectedTypeAtCurrent(RawSchema.Null))

  // The sign is passed into `literal` rather than multiplied in afterwards, so integer literals
  // are range-checked with the sign known — `-2147483648` is valid although its magnitude
  // overflows a positive Int. Parameters are inline so the lambda literals beta-reduce away
  // instead of going through Function1, which is not specialized for Float and would box on
  // every float decode.
  private inline def decodeSigned[N](
      inline literal: Boolean => N,
      inline store: N => Unit
  ): Unit =
    val negative =
      if currentKind() == TokenKind.Minus then
        advance()
        true
      else false
    val value = literal(negative)
    advance()
    store(value)

  private def expectVal(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.ValKw then advance()
    else raise(DecodeError.ExpectedVal(describeCurrent()).atToken(currentSpan()))

  private def expectPackageStatement(packageName: String): Result[Unit, DecodeError] =
    Result.task:
      if packageName.nonEmpty then
        expectPackage().check
        expectQualifiedIdentifier().check
        val declaredName = pullStringStrict()
        if declaredName != packageName then raise(DecodeError.UnexpectedPackage(declaredName))
        acceptStatementSeparator()

  private def acceptStatementSeparator(): Unit =
    if currentKind() == TokenKind.Semicolon then advance()

  private def expectPackage(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.PackageKw then advance()
    else raise(DecodeError.ExpectedPackage(describeCurrent()).atToken(currentSpan()))

  /** parses a dotted identifier path, pushing it into [[stringSlot]] */
  private def expectQualifiedIdentifier(): Result[Unit, DecodeError] = Result.task:
    expectIdentifier().check
    if currentKind() == TokenKind.Dot then
      val builder = new StringBuilder(pullStringStrict())
      while currentKind() == TokenKind.Dot do
        advance()
        builder.append('.')
        expectIdentifier().check
        builder ++= pullStringStrict()
      pushString(builder.result())

  /** parses an identifier, pushing it into [[stringSlot]] */
  private def expectIdentifier(): Result[Unit, DecodeError] = Result.task:
    val name = currentKind() match
      case TokenKind.Identifier   => currentName()
      case TokenKind.VectorId     => "Vector"
      case TokenKind.EmptyTupleId => "EmptyTuple"
      case TokenKind.Plus         => "+"
      case TokenKind.Minus        => "-"
      case TokenKind.StarColon    => "*:"
      case _                      =>
        raise(DecodeError.ExpectedIdentifier(describeCurrent()).atToken(currentSpan()))
    advance()
    pushString(name)

  private def expectEquals(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.Equals then advance()
    else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  private def expectEof(): Result[Unit, DecodeError] = Result.task:
    if currentKind() != TokenKind.Eof then
      raise(DecodeError.ExpectedEof(describeCurrent()).atToken(currentSpan()))
}
