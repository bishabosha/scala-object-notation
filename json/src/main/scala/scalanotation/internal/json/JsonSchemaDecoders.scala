package scalanotation.internal.json

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.RouterSchema
import scalanotation.internal.Internal
import scalanotation.internal.Internal.breakErr
import scalanotation.internal.PublicInternal
import scalanotation.internal.SharedHelpers
import scalanotation.schema.RawSchema
import scalanotation.schema.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

import Internal.JumboNameSet.given

/** Schema-driven JSON decoding over the byte-level scanning kernel — the JSON counterpart of the
  * core SchemaDecoders, with the same single-path structure: one record loop serves ordered and
  * skippable schemas, field headers match plan-cached bytes without materializing names, values
  * decode-and-append in one plan dispatch through the typed slots, and every deviation drops to a
  * cold helper that re-reads the same bytes generically and owns error rendering.
  */
private[json] trait JsonSchemaDecoders extends JsonScanner, SharedHelpers:
  import JsonScanner.*

  protected final def decodeBase(schema: RawSchema[?]): Result[Unit, DecodeError] =
    schema match
      case schema: RawSchema.Atomic =>
        // primitive decodes never recurse into the input (Mapped/Ref chains are schema-bounded),
        // so they skip the nesting guard entirely
        decodePrimitiveBase(schema)
      case schema: RawSchema.Collection =>
        if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
        else
          val result = decodeCollectionBase(schema)
          exitNesting()
          result
      case _ =>
        if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
        else
          val result = decodeCompositeBase(schema)
          exitNesting()
          result

  private def decodePrimitiveBase(
      schema: RawSchema[?] & RawSchema.Atomic
  ): Result[Unit, DecodeError] =
    schema match
      case RawSchema.String =>
        decodeString()
      case RawSchema.Int =>
        decodeInt()
      case RawSchema.Double =>
        decodeDouble()
      case RawSchema.Long =>
        decodeLong()
      case RawSchema.Boolean =>
        decodeBoolean()
      case mapped: RawSchema.Mapped[?, ?] =>
        decodeMappedBase(mapped)
      case RawSchema.Null =>
        decodeNull()
      case RawSchema.Char =>
        decodeChar()
      case RawSchema.Float =>
        decodeFloat()
      case ref: RawSchema.Ref[?] =>
        decodeBase(ref.target())

  private def decodeMappedBase(schema: RawSchema.Mapped[?, ?]): Result[Unit, DecodeError] =
    Result.task {
      decodeBase(schema.base).check
      mapSlot(schema.mapping).check
    }

  private def decodeCompositeBase(schema: RawSchema[?]): Result[Unit, DecodeError] =
    schema match
      case sc: RawSchema.NamedTuple[?] =>
        decodeNamedTuple(sc)
      case sc: RawSchema.Tuple[?] =>
        decodeTuple(sc)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        decodePartialNamedTuple(base, alreadySeenField)
      case sc: RawSchema.Sum[?] =>
        decodeSum(sc)
      case sc: RawSchema.DiscriminatorSum[?] =>
        decodeDiscriminatorSum(sc)
      case router: RawSchema.Router[?] =>
        decodeRouter(router)
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeCurrent()))

  private def decodeCollectionBase(
      schema: RawSchema[?] & RawSchema.Collection
  ): Result[Unit, DecodeError] =
    schema match
      case sc: RawSchema.Vector[?, ?] =>
        decodeVector(sc)
      case sc: RawSchema.PairSeq[?, ?, ?] =>
        decodePairSeq(sc)
      case sc: RawSchema.Dict[?, ?] =>
        decodeDict(sc)
      case sc: RawSchema.Option[?] =>
        decodeOption(sc)
      case sc: RawSchema.TupleOf[?, ?] =>
        decodeTupleOf(sc)

  protected final def expectedTypeAtCurrent(schema: RawSchema[?]): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  private def expectedColonError(): DecodeError =
    DecodeError.Custom(s"Expected ':' but found ${describeCurrent()}").atToken(currentSpan())

  private def expectedObjectEndError(): DecodeError =
    DecodeError.Custom(s"Expected '}' but found ${describeCurrent()}").atToken(currentSpan())

  private def expectedArrayEndError(): DecodeError =
    DecodeError.Custom(s"Expected ']' but found ${describeCurrent()}").atToken(currentSpan())

  private def expectedFieldNameError(): DecodeError =
    DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan())

  // --- records ---

  protected final def decodeNamedTuple(
      schema: RawSchema.NamedTuple[?]
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { slots =>
        // the seen-field bitset engages lazily on the first out-of-order arrival — while every
        // field arrives in schema order, the decoded set is always the contiguous prefix
        decodeRecordFields(schema, read, slots, null, null, openBrace = true)
      }
    }

  // Per-depth seen-field sets: a nesting level runs at most one record decode at a time, so a
  // level's set is reusable directly — no pool borrow/release/clear per record.
  private var seenFieldSets = new Array[Internal.FieldIndexSet | Null](8)

  private def seenFieldSetForDepth(): Internal.FieldIndexSet =
    val depth = currentNestingDepth
    var sets  = seenFieldSets
    if depth >= sets.length then
      sets = java.util.Arrays.copyOf(sets, math.max(sets.length * 2, depth + 1))
      seenFieldSets = sets
    val existing = sets(depth)
    if existing != null then existing
    else
      val created = new Internal.FieldIndexSet
      sets(depth) = created
      created

  /** Decodes one JSON object as a named-tuple record with a single loop covering every field order.
    * The happy path matches the plan-cached header bytes (`"name":`) of the next expected field
    * (writer output arrives in schema order), decodes the value straight into the builder through
    * its plan code, and consumes the `,`/`}` separator — a primitive field materializes nothing but
    * its value, and no bitset is touched. Any deviation (whitespace before the colon, out-of-order
    * names, escapes, unknown fields) engages the seen-field bitset and drops to the cold resolver,
    * which re-reads the same bytes generically: JSON objects are unordered, so an arriving name
    * resolves against ALL fields, and unknown names are skipped (or rejected in strict mode).
    * Omitted fields fill from their fill values at the closing brace; a missing field without one
    * is an error.
    */
  private def decodeRecordFields(
      schema: RawSchema.NamedTuple[?],
      read: RawSchema.NamedTupleRead,
      slots: scalanotation.BuilderSlots | Null,
      seenFields: Internal.FieldIndexSet | Null,
      alreadySeenField: String | Null,
      openBrace: Boolean
  ): Result[Unit, DecodeError] =
    Result.task {
      val fields = schema.fields
      val plans  = JsonFieldPlans.of(schema)
      val fills  = plans.fills
      var seen   = seenFields
      if seen != null then
        seen.reset(fields.length)
        if alreadySeenField != null then
          val alreadySeenIndex = indexOfField(fields, alreadySeenField)
          if alreadySeenIndex >= 0 then seen.mark(alreadySeenIndex)
      schema.isValidNamedTuple(namesPool).check
      val kinds             = plans.kinds
      val headers           = plans.headerBytes
      var state: read.State = read.init(fields.length, slots)

      if openBrace && !tryReadPunct('{') then raise(expectedTypeAtCurrent(schema))

      var fieldIndex    = 0 // the next expected schema field (a hint, not a requirement)
      var closingOffset = 0L

      if tryReadPunct('}') then
        // `{}` provides no fields: the closing walk below fills or reports every field
        closingOffset = absoluteOffset(pos - 1)
      else
        var done = false
        while !done do
          // --- field name (+ ':'): fused header bytes against the expected field, else the
          // cold resolver ---
          var decodedIndex = -1
          var nameOffset   = 0L
          if fieldIndex < fields.length && expectFieldHeader(headers(fieldIndex)) then
            decodedIndex = fieldIndex
            nameOffset = absoluteOffset(pos - headers(fieldIndex).length)
            if seen != null then
              // engaged only after a deviation (or a pre-marked discriminator): the expected
              // field can then already be decoded
              if seen.contains(decodedIndex) then
                raise(makeDuplicateKnownFieldError(fields(decodedIndex).name, nameOffset))
              seen.mark(decodedIndex)
          else
            nameOffset = currentOffset()
            if seen == null then seen = engagePrefixSeenSet(fields.length, fieldIndex)
            resolveRecordFieldSlow(fields, plans, seen, alreadySeenField).check
            decodedIndex = pullControl()
            if decodedIndex >= 0 then seen.mark(decodedIndex)

          // --- value: decode and append in one plan dispatch (unknown fields were consumed
          // whole by the resolver) ---
          if decodedIndex >= 0 then
            decodeValueInto(read)(
              state,
              decodedIndex,
              kinds(decodedIndex),
              fields(decodedIndex).schema
            ) match
              case err: Result.Err[?] =>
                raise(
                  recordFieldValueError(
                    err.asInstanceOf[Result.Err[DecodeError]].error,
                    fields(decodedIndex),
                    nameOffset
                  )
                )
              case nextState =>
                state = nextState.asInstanceOf[read.State]
                fieldIndex = decodedIndex + 1

          // --- separator ---
          tryReadSeparator('}') match
            case SeparatorComma   => ()
            case SeparatorClosing =>
              closingOffset = absoluteOffset(pos - 1)
              done = true
            case _ =>
              raise(expectedObjectEndError())

      // --- omitted fields: fill the fillable ones, report the first missing required field ---
      if seen == null then
        // every decoded field was the expected one, so the unseen set is the trailing range
        var index = fieldIndex
        while index < fields.length do
          val fill = if fills != null then fills(index) else null
          if fill == null then raise(missingFieldError(fields(index).name, closingOffset))
          state = read.add(state, index, fill)
          index += 1
      else
        var index = 0
        while index < fields.length do
          if !seen.contains(index) then
            val fill = if fills != null then fills(index) else null
            if fill == null then raise(missingFieldError(fields(index).name, closingOffset))
            state = read.add(state, index, fill)
          index += 1
      pushRef(read.finish(state))
    }

  /** engages the per-depth seen-field set mid-record: fields decoded so far are exactly the
    * contiguous prefix
    */
  private def engagePrefixSeenSet(fieldCount: Int, prefix: Int): Internal.FieldIndexSet =
    val set = seenFieldSetForDepth()
    set.reset(fieldCount)
    var index = 0
    while index < prefix do
      set.mark(index)
      index += 1
    set

  private def missingFieldError(name: String, closingOffset: Long): DecodeError =
    DecodeError.MissingField(name).atPath(s".$name").atToken(spanAt(closingOffset))

  /** Resolves the current escape-free name slice to its plan entry: wide plans probe the name
    * dispatch table (hash of the slice bytes, linear probe, byte-compare verification), narrow
    * plans walk their few names directly. -1 = the name matches no entry.
    */
  private def dispatchSlice(plans: JsonFieldPlans): Int =
    val contents = plans.contentBytes
    val table    = plans.dispatch
    if table == null then
      var index  = plans.dispatchStart
      var result = -1
      while result < 0 && index < contents.length do
        val content = contents(index)
        if content != null && sliceEquals(content) then result = index
        else index += 1
      result
    else
      val mask    = table.length - 1
      var slot    = sliceHash() & mask
      var result  = -1
      var probing = true
      while probing do
        val candidate = table(slot)
        if candidate < 0 then probing = false
        else
          val content = contents(candidate)
          if content != null && sliceEquals(content) then
            result = candidate
            probing = false
          else slot = (slot + 1) & mask
      result

  /** [[resolveRecordFieldSlow]]'s control-slot value when an unknown field name (and its whole
    * value) was consumed in lenient mode
    */
  private inline val SkippedUnknownField = -1

  /** Cold path of the record loop's name step: resolves an arriving field name that was not the
    * exact expected header against ALL fields — JSON objects are unordered. Escape-free names
    * slice-compare against the plan-cached content bytes (nothing materializes on the match path);
    * escaped names compare decoded. On success the name and colon are consumed and the resolved
    * index is left in the control slot. A name matching no field is an
    * [[DecodeError.UnexpectedField]] in strict mode; otherwise its value is skipped whole and
    * [[SkippedUnknownField]] is left in the control slot.
    */
  private def resolveRecordFieldSlow(
      fields: IArray[Field],
      plans: JsonFieldPlans,
      seen: Internal.FieldIndexSet,
      alreadySeenField: String | Null
  ): Result[Unit, DecodeError] = Result.task {
    if !tryScanStringSlice() then raise(expectedFieldNameError())
    val nameOffset = sliceQuoteAbsolute()
    var resolved   = -1
    if !sliceEscaped then resolved = dispatchSlice(plans)
    else
      val name  = materializeSlice()
      var index = 0
      while resolved < 0 && index < fields.length do
        if name == plans.names(index) then resolved = index
        else index += 1

    if resolved >= 0 then
      if seen.contains(resolved) then
        raise(makeDuplicateKnownFieldError(fields(resolved).name, nameOffset))
      if !tryReadPunct(':') then raise(expectedColonError())
      pushControl(resolved)
    else
      // not a schema field: a re-sent pre-consumed field (the discriminator) is a duplicate;
      // anything else is unknown — an error in strict mode, skipped whole otherwise
      if alreadySeenField != null || plans.rejectUnknown then
        val actualName = materializeSlice()
        if actualName == alreadySeenField then
          raise(makeDuplicateKnownFieldError(alreadySeenField.nn, nameOffset))
        if plans.rejectUnknown then
          // ':' is consumed before the unknown field is reported, like the sum decoder
          if !tryReadPunct(':') then raise(expectedColonError())
          raise(
            DecodeError
              .UnexpectedField(actualName)
              .atPath(s".$actualName")
              .atToken(spanAt(nameOffset))
          )
      if !tryReadPunct(':') then raise(expectedColonError())
      skipValue().check
      pushControl(SkippedUnknownField)
  }

  private def makeDuplicateKnownFieldError(name: String, nameOffset: Long): DecodeError =
    DecodeError.DuplicateField(name).atPath(s".$name").atToken(spanAt(nameOffset))

  /** cold decoration of a field value's decode error */
  private def recordFieldValueError(
      error: DecodeError,
      field: Field,
      nameOffset: Long
  ): DecodeError =
    error.atPath(s".${field.name}").atToken(spanAt(nameOffset))

  /** Skips one JSON value of any shape — the lenient record loop discards an unknown field's value
    * without materializing anything. Malformed values report the decoders' errors.
    */
  private def skipValue(): Result[Unit, DecodeError] =
    Result.task:
      val next = peekByteOrEof()
      if next < 0 then
        raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      (next: @annotation.switch) match
        case '"' =>
          tryScanStringSlice()
          ()
        case 't' | 'f' =>
          tryReadBoolean()
          ()
        case 'n' =>
          tryReadNull()
          ()
        case '{' =>
          skipObject().check
        case '[' =>
          skipArray().check
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          tryScanNumber()
          ()
        case _ =>
          raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

  private def skipObject(): Result[Unit, DecodeError] =
    if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
    else
      val result = Result.task:
        pos += 1 // the '{' probed by skipValue
        if tryReadPunct('}') then ()
        else
          var done = false
          while !done do
            if !tryScanStringSlice() then raise(expectedFieldNameError())
            if !tryReadPunct(':') then raise(expectedColonError())
            skipValue().check
            tryReadSeparator('}') match
              case SeparatorComma   => ()
              case SeparatorClosing => done = true
              case _                => raise(expectedObjectEndError())
      exitNesting()
      result

  private def skipArray(): Result[Unit, DecodeError] =
    if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
    else
      val result = Result.task:
        pos += 1 // the '[' probed by skipValue
        if tryReadPunct(']') then ()
        else
          var done = false
          while !done do
            skipValue().check
            tryReadSeparator(']') match
              case SeparatorComma   => ()
              case SeparatorClosing => done = true
              case _                => raise(expectedArrayEndError())
      exitNesting()
      result

  private def indexOfField(fields: IArray[Field], name: String): Int =
    var index = 0
    while index < fields.length do
      if fields(index).name == name then return index
      index += 1
    -1

  /** Decodes a planned field value and appends it into the named-tuple builder state in a single
    * plan dispatch, returning the new state directly (the error otherwise) — the plan already names
    * the live typed slot, so neither a slot-kind switch nor a state round trip through the Any slot
    * runs on the happy path.
    */
  private def decodeValueInto(
      read: RawSchema.NamedTupleRead
  )(
      state: read.State,
      index: Int,
      plan: Byte,
      schema: RawSchema[?]
  ): read.State | Result.Err[DecodeError] = {
    inline def added(inline decoded: Result[Unit, DecodeError])(
        inline add: => read.State
    ): read.State | Result.Err[DecodeError] =
      decoded match
        case err: Result.Err[?] => err.asInstanceOf[Result.Err[DecodeError]]
        case _                  => add
    (plan: @scala.annotation.switch) match
      case RawSchema.FieldPlan.IntV =>
        added(decodeInt())(read.addInt(state, index, pullIntValue()))
      case RawSchema.FieldPlan.LongV =>
        added(decodeLong())(read.addLong(state, index, pullLongValue()))
      case RawSchema.FieldPlan.DoubleV =>
        added(decodeDouble())(read.addDouble(state, index, pullDoubleValue()))
      case RawSchema.FieldPlan.FloatV =>
        added(decodeFloat())(read.addFloat(state, index, pullFloatValue()))
      case RawSchema.FieldPlan.BooleanV =>
        added(decodeBoolean())(read.addBoolean(state, index, pullBooleanValue()))
      case RawSchema.FieldPlan.StringV =>
        added(decodeString())(read.addString(state, index, pullStringStrict()))
      case RawSchema.FieldPlan.CharV =>
        added(decodeChar())(read.addChar(state, index, pullCharValue()))
      case RawSchema.FieldPlan.RecordV =>
        added(decodeNestedRecord(schema))(addSlot(read)(state, index))
      case RawSchema.FieldPlan.VectorV =>
        added(decodeNestedVector(schema))(addSlot(read)(state, index))
      case RawSchema.FieldPlan.OptionV =>
        added(decodeNestedOption(schema))(addSlot(read)(state, index))
      case _ =>
        added(decodeBase(schema))(addSlot(read)(state, index))
  }

  /** [[decodeValueInto]] for array elements — same single dispatch onto the vector builder */
  private def decodeElementInto[Elem, Repr, A](
      read: Reader.VectorBuilder[Elem, Repr, A]
  )(
      values: Repr,
      plan: Byte,
      schema: RawSchema[?]
  ): Repr | Result.Err[DecodeError] = {
    inline def added(inline decoded: Result[Unit, DecodeError])(
        inline add: => Repr
    ): Repr | Result.Err[DecodeError] =
      decoded match
        case err: Result.Err[?] => err.asInstanceOf[Result.Err[DecodeError]]
        case _                  => add
    (plan: @scala.annotation.switch) match
      case RawSchema.FieldPlan.IntV =>
        added(decodeInt())(read.addInt(values, pullIntValue()))
      case RawSchema.FieldPlan.LongV =>
        added(decodeLong())(read.addLong(values, pullLongValue()))
      case RawSchema.FieldPlan.DoubleV =>
        added(decodeDouble())(read.addDouble(values, pullDoubleValue()))
      case RawSchema.FieldPlan.FloatV =>
        added(decodeFloat())(read.addFloat(values, pullFloatValue()))
      case RawSchema.FieldPlan.BooleanV =>
        added(decodeBoolean())(read.addBoolean(values, pullBooleanValue()))
      case RawSchema.FieldPlan.StringV =>
        added(decodeString())(read.addString(values, pullStringStrict()))
      case RawSchema.FieldPlan.CharV =>
        added(decodeChar())(read.addChar(values, pullCharValue()))
      case RawSchema.FieldPlan.RecordV =>
        added(decodeNestedRecord(schema))(addSlot(read)(values))
      case RawSchema.FieldPlan.VectorV =>
        added(decodeNestedVector(schema))(addSlot(read)(values))
      case RawSchema.FieldPlan.OptionV =>
        added(decodeNestedOption(schema))(addSlot(read)(values))
      case _ =>
        added(decodeBase(schema))(addSlot(read)(values))
  }

  // Straight dispatches for planned composite values — the same nesting guard and decoders
  // decodeBase reaches, minus its two schema matches per value.
  private def decodeNestedRecord(schema: RawSchema[?]): Result[Unit, DecodeError] =
    if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
    else
      val result = decodeNamedTuple(schema.asInstanceOf[RawSchema.NamedTuple[?]])
      exitNesting()
      result

  private def decodeNestedVector(schema: RawSchema[?]): Result[Unit, DecodeError] =
    if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
    else
      val result = decodeVector(schema.asInstanceOf[RawSchema.Vector[?, ?]])
      exitNesting()
      result

  private def decodeNestedOption(schema: RawSchema[?]): Result[Unit, DecodeError] =
    if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
    else
      val result = decodeOption(schema.asInstanceOf[RawSchema.Option[?]])
      exitNesting()
      result

  /** Dispatches a planned value into the live slot — semantically identical to [[decodeBase]]. */
  private def decodePlannedSlotValue(plan: Byte, schema: RawSchema[?]): Result[Unit, DecodeError] =
    (plan: @scala.annotation.switch) match
      case RawSchema.FieldPlan.IntV     => decodeInt()
      case RawSchema.FieldPlan.LongV    => decodeLong()
      case RawSchema.FieldPlan.DoubleV  => decodeDouble()
      case RawSchema.FieldPlan.FloatV   => decodeFloat()
      case RawSchema.FieldPlan.BooleanV => decodeBoolean()
      case RawSchema.FieldPlan.StringV  => decodeString()
      case RawSchema.FieldPlan.CharV    => decodeChar()
      case RawSchema.FieldPlan.RecordV  => decodeNestedRecord(schema)
      case RawSchema.FieldPlan.VectorV  => decodeNestedVector(schema)
      case RawSchema.FieldPlan.OptionV  => decodeNestedOption(schema)
      case _                            => decodeBase(schema)

  // --- sums ---

  /** Decodes one sum value `{"CaseName": <case value>}`: the case name slice-matches against the
    * plan-cached case names (no String materializes on the match path), and the case value
    * dispatches through its plan code.
    */
  protected final def decodeSum(schema: RawSchema.Sum[?]): Result[Unit, DecodeError] =
    Result.task:
      if !tryReadPunct('{') then raise(expectedTypeAtCurrent(schema))
      if tryReadPunct('}') then
        raise(DecodeError.FieldCountMismatch(1, 0).atToken(spanAt(absoluteOffset(pos - 1))))

      val cases     = schema.cases
      val plans     = JsonFieldPlans.of(schema)
      var caseIndex = -1
      if !tryScanStringSlice() then raise(expectedFieldNameError())
      val nameOffset = sliceQuoteAbsolute()
      if !sliceEscaped then caseIndex = dispatchSlice(plans)
      if caseIndex < 0 then
        // a non-plain or unknown name: compare decoded (only errors materialize otherwise)
        val name  = materializeSlice()
        var index = 0
        while caseIndex < 0 && index < cases.length do
          if name == cases(index).name then caseIndex = index
          else index += 1
        if caseIndex < 0 then
          // like the record loop, ':' is consumed before an unknown case is reported
          if !tryReadPunct(':') then raise(expectedColonError())
          raise(
            DecodeError.UnexpectedField(name).atPath(s".$name").atToken(spanAt(nameOffset))
          )

      if !tryReadPunct(':') then raise(expectedColonError())
      val sumCase = cases(caseIndex)
      checkOrRaise(decodePlannedSlotValue(plans.kinds(caseIndex), sumCase.schema))(
        _.atPath(s".${sumCase.name}")
      )
      finishSumObject().check
      // the decoded case value remains in the live slot

  private def finishSumObject(): Result[Unit, DecodeError] =
    Result.task:
      if tryReadPunct('}') then ()
      else if tryReadPunct(',') then
        val nameOffset = currentOffset()
        if !tryScanStringSlice() then raise(expectedFieldNameError())
        val actualName = materializeSlice()
        raise(
          DecodeError
            .FieldCountMismatch(1, 2)
            .atPath(s".$actualName")
            .atToken(spanAt(nameOffset))
        )
      else raise(expectedObjectEndError())

  protected final def decodeDiscriminatorSum(
      schema: RawSchema.DiscriminatorSum[?]
  ): Result[Unit, DecodeError] =
    Result.task {
      decodeDiscriminatorSumHeader(schema) match
        case err: Result.Err[?] =>
          breakErr(err.asInstanceOf[Result.Err[DecodeError]])
        case sumCase =>
          decodeBase(sumCase.asInstanceOf[RawSchema.SumCase].schema).check
    }

  private def decodeDiscriminatorSumHeader(
      schema: RawSchema.DiscriminatorSum[?]
  ): RawSchema.SumCase | Result.Err[DecodeError] =
    if !tryReadPunct('{') then return Result.Err(expectedTypeAtCurrent(schema))
    if tryReadPunct('}') then
      return Result.Err(
        DecodeError.MissingField(schema.discriminatorField).atToken(spanAt(absoluteOffset(pos - 1)))
      )

    val discriminatorField = schema.discriminatorField
    val plans              = JsonFieldPlans.of(schema)
    // the discriminator header reads like a record field: fused header bytes, with the decoded
    // compare (and the name materialized only for its error) covering escapes and mismatches
    if !expectFieldHeader(plans.headerBytes(0)) then
      val nameOffset = currentOffset()
      if !tryScanStringSlice() then return Result.Err(expectedFieldNameError())
      val actualName = materializeSlice()
      if actualName != discriminatorField then
        return Result.Err(
          DecodeError
            .FieldOrderMismatch(discriminatorField, actualName)
            .atPath(s".$actualName")
            .atToken(spanAt(nameOffset))
        )
      if !tryReadPunct(':') then return Result.Err(expectedColonError())

    // the case name arrives as a JSON string: an escape-free literal's content slice-compares
    // against the plan-cached case names, so no String materializes on the match path
    val headerOffset                      = currentOffset()
    var sumCase: RawSchema.SumCase | Null = null
    if !tryScanStringSlice() then
      return Result.Err(
        DecodeError
          .ExpectedType("String", describeCurrent())
          .atPath(s".$discriminatorField")
          .atToken(currentSpan())
      )
    if !sliceEscaped then
      // the dispatch table covers entries 1..n (entry 0 is the discriminator header)
      val entry = dispatchSlice(plans)
      if entry > 0 then sumCase = schema.cases(entry - 1)
    if sumCase == null then
      val caseName = materializeSlice()
      sumCase = RawSchema.findCase(schema, caseName)
      if sumCase == null then
        return Result.Err(
          DecodeError
            .UnexpectedField(caseName)
            .atPath(s".$discriminatorField")
            .atToken(spanAt(headerOffset))
        )

    // payload start: a comma leaves the cursor at the first payload field; the closing brace is
    // left for the partial record loop to consume
    val payloadStart = peekByteOrEof()
    if payloadStart == ',' then pos += 1
    else if payloadStart == '}' then ()
    else return Result.Err(expectedObjectEndError())
    sumCase.nn

  protected final def decodePartialNamedTuple(
      schema: RawSchema[?],
      alreadySeenField: String
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        Result.task {
          decodePartialNamedTuple(mapped.base, alreadySeenField).check
          mapSlot(mapped.mapping).check
        }
      case namedTuple: RawSchema.NamedTuple[?] =>
        withRead(namedTuple, _.read) { read =>
          withBorrowSlots(read.slotsFactory) { slots =>
            // the opening brace and the discriminator were consumed by the caller; the bitset is
            // always tracked so a re-sent discriminator classifies as a duplicate
            decodeRecordFields(
              namedTuple,
              read,
              slots,
              seenFieldSetForDepth(),
              alreadySeenField,
              openBrace = false
            )
          }
        }
      case RawSchema.Null =>
        decodeEmptyPartialNamedTuple()
      case RawSchema.Ref(_, target) =>
        decodePartialNamedTuple(target(), alreadySeenField)
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeCurrent()))

  private def decodeEmptyPartialNamedTuple(): Result[Unit, DecodeError] =
    Result.task:
      if tryReadPunct('}') then pushRef(null)
      else raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))

  // --- arrays ---

  protected final def decodeVector(schema: RawSchema.Vector[?, ?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeVectorWithRead(schema, read))

  private def decodeVectorWithRead[Elem, Repr, A](
      schema: RawSchema.Vector[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A]
  ): Result[Unit, DecodeError] =
    Result.task:
      if !tryReadPunct('[') then raise(expectedTypeAtCurrent(schema))
      var values = read.init()
      if tryReadPunct(']') then ()
      else
        var indexInVector = 0
        var done          = false
        val elementPlan   = RawSchema.valuePlanOf(schema.element)
        while !done do
          decodeElementInto(read)(values, elementPlan, schema.element) match
            case err: Result.Err[?] =>
              raise(err.asInstanceOf[Result.Err[DecodeError]].error.atPath(s"[$indexInVector]"))
            case nextValues =>
              values = nextValues.asInstanceOf[Repr]
          indexInVector += 1

          tryReadSeparator(']') match
            case SeparatorComma   => ()
            case SeparatorClosing => done = true
            case _                => raise(expectedArrayEndError())

      pushRef(read.finish(values))

  protected final def decodeTupleOf(schema: RawSchema.TupleOf[?, ?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeTupleOfWithRead(schema, read))

  private def decodeTupleOfWithRead[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A]
  ): Result[Unit, DecodeError] =
    Result.task:
      if !tryReadPunct('[') then raise(expectedTypeAtCurrent(schema))
      var state = read.init()
      if tryReadPunct(']') then ()
      else
        var index = 0
        var done  = false
        while !done do
          decodeBase(schema.element) match
            case Result.Err(error) => raise(error.atPath(s"[$index]"))
            case _                 => ()
          state = addSlot(read)(state)
          index += 1
          tryReadSeparator(']') match
            case SeparatorComma   => ()
            case SeparatorClosing => done = true
            case _                => raise(expectedArrayEndError())
      pushRef(read.finish(state))

  protected final def decodeTuple(schema: RawSchema.Tuple[?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeTupleWithRead(schema, read))

  private def decodeTupleWithRead[Repr, A](
      schema: RawSchema.Tuple[?],
      read: Reader.TupleBuilder[Repr, A]
  ): Result[Unit, DecodeError] =
    withBorrowSlots(read.slotsFactory) { pooled =>
      Result.task:
        val slots      = schema.slots
        val expected   = slots.length
        val openOffset = currentOffset()
        if !tryReadPunct('[') then raise(expectedTypeAtCurrent(schema))
        var state = read.initPooled(expected, pooled)
        if tryReadPunct(']') then
          if expected > 0 then
            raise(DecodeError.FieldCountMismatch(expected, 0).atToken(spanAt(openOffset)))
        else
          if expected == 0 then raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))
          var count = 0
          var done  = false
          while !done do
            if count >= expected then
              raise(DecodeError.FieldCountMismatch(expected, count + 1).atToken(currentSpan()))
            decodeBase(slots(count)) match
              case Result.Err(error) => raise(error.atPath(s"[$count]"))
              case _                 => ()
            state = addSlot(read)(state, count)
            count += 1
            tryReadSeparator(']') match
              case SeparatorComma   => ()
              case SeparatorClosing => done = true
              case _                => raise(expectedArrayEndError())
          if count != expected then
            raise(
              DecodeError
                .FieldCountMismatch(expected, count)
                .atToken(spanAt(absoluteOffset(pos - 1)))
            )
        pushRef(read.finish(state))
    }

  protected final def decodePairSeq(schema: RawSchema.PairSeq[?, ?, ?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodePairSeqWithRead(schema, read))

  private def decodePairSeqWithRead[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A]
  ): Result[Unit, DecodeError] =
    Result.task:
      if !tryReadPunct('[') then raise(expectedTypeAtCurrent(schema))
      var state = read.init()
      if tryReadPunct(']') then ()
      else
        var index = 0
        var done  = false
        while !done do
          decodePairSeqElement(schema, read, state, index) match
            case err: Result.Err[?] => breakErr(err.asInstanceOf[Result.Err[DecodeError]])
            case nextState          => state = nextState.asInstanceOf[Repr]
          index += 1
          tryReadSeparator(']') match
            case SeparatorComma   => ()
            case SeparatorClosing => done = true
            case _                => raise(expectedArrayEndError())
      pushRef(read.finish(state))

  private def decodePairSeqElement[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      state0: Repr,
      index: Int
  ): Repr | Result.Err[DecodeError] =
    if !tryReadPunct('[') then
      return Result.Err(
        DecodeError
          .ExpectedType(RawSchema.describeTupleSlots(2), describeCurrent())
          .atToken(currentSpan())
      )
    if tryReadPunct(']') then
      return Result.Err(
        DecodeError.FieldCountMismatch(2, 0).atToken(spanAt(absoluteOffset(pos - 1)))
      )

    decodeBase(schema.key) match
      case Result.Err(error) => return Result.Err(error.atPath(s"[$index][0]"))
      case _                 =>

    var state = addPairKeySlot(read)(state0)
    if !tryReadPunct(',') then
      return Result.Err(
        if tryReadPunct(']') then
          DecodeError.FieldCountMismatch(2, 1).atToken(spanAt(absoluteOffset(pos - 1)))
        else expectedArrayEndError()
      )

    decodeBase(schema.value) match
      case Result.Err(error) => return Result.Err(error.atPath(s"[$index][1]"))
      case _                 =>

    state = addPairValueSlot(read)(state)
    tryReadSeparator(']') match
      case SeparatorClosing => state
      case SeparatorComma   =>
        Result.Err(DecodeError.FieldCountMismatch(2, 3).atToken(currentSpan()))
      case _ => Result.Err(expectedArrayEndError())

  // --- dicts ---

  protected final def decodeDict(schema: RawSchema.Dict[?, ?]): Result[Unit, DecodeError] =
    namesPool.withBorrowed { seenNames =>
      withRead(schema, _.read)(read => decodeDictWithRead(schema, read, seenNames))
    }

  private def decodeDictWithRead[Elem, Repr, A](
      schema: RawSchema.Dict[?, ?],
      read: Reader.DictBuilder[Elem, Repr, A],
      seenNames: Internal.JumboNameSet
  ): Result[Unit, DecodeError] =
    Result.task:
      if !tryReadPunct('{') then raise(expectedTypeAtCurrent(schema))
      var state = read.init()
      if tryReadPunct('}') then ()
      else
        var done        = false
        val elementPlan = RawSchema.valuePlanOf(schema.element)
        while !done do
          if !tryScanStringSlice() then raise(expectedFieldNameError())
          val nameOffset = sliceQuoteAbsolute()
          val name       = materializeSlice()
          if !tryReadPunct(':') then raise(expectedColonError())
          if seenNames.alreadySeen(name) then
            raise(DecodeError.DuplicateField(name).atPath(s".$name").atToken(spanAt(nameOffset)))
          decodePlannedSlotValue(elementPlan, schema.element) match
            case Result.Err(error) => raise(error.atPath(s".$name"))
            case _                 => ()
          state = addSlot(read)(state, name)
          tryReadSeparator('}') match
            case SeparatorComma   => ()
            case SeparatorClosing => done = true
            case _                => raise(expectedObjectEndError())
      pushRef(read.finish(state))

  // --- options ---

  protected final def decodeOption(schema: RawSchema.Option[?]): Result[Unit, DecodeError] =
    Result.task {
      if tryReadNull() then pushRef(None)
      else
        decodeBase(schema.inner).check
        pushRef(Some(pullAny()))
    }

  // --- routers (dynamic values) ---

  protected final def decodeRouter(schema: RawSchema.Router[?]): Result[Unit, DecodeError] =
    Result.task:
      val next = peekByteOrEof()
      if next < 0 then
        raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      val router = schema.router
      (next: @annotation.switch) match
        case '{' =>
          decodeRouterCase(schema, router.recordIndex).check
        case '[' =>
          val index =
            if caseSupported(schema, router.vectorIndex) then router.vectorIndex
            else router.tupleIndex
          decodeRouterCase(schema, index).check
        case '"' =>
          val index =
            if caseSupported(schema, router.stringIndex) then router.stringIndex
            else router.charIndex
          decodeRouterCase(schema, index).check
        case 't' | 'f' =>
          decodeRouterCase(schema, router.booleanIndex).check
        case 'n' =>
          decodeRouterCase(schema, router.nullIndex).check
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          decodeRouterNumber(schema).check
        case _ =>
          raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

  private def caseSupported(schema: RawSchema.Router[?], index: RouterSchema.Index): Boolean =
    RawSchema.routerCase(schema, index) != null

  private def decodeRouterCase(
      schema: RawSchema.Router[?],
      index: RouterSchema.Index
  ): Result[Unit, DecodeError] =
    val routerCase = RawSchema.routerCase(schema, index)
    if routerCase == null then
      Result.Err(
        DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
      )
    else decodeBase(routerCase.schema)

  /** Routes a number: the literal scans exactly once, the router picks a case from its shape (raw
    * mode routes every number to the raw case), and the scanned value feeds whatever primitive base
    * that case has.
    */
  private def decodeRouterNumber(schema: RawSchema.Router[?]): Result[Unit, DecodeError] =
    Result.task:
      val numberOffset = absoluteOffset(pos)
      tryScanNumber() // start byte was probed by the caller; malformed numbers throw
      val router = schema.router
      val index  =
        schema.numberMode match
          case RouterSchema.NumberMode.Raw     => router.rawNumberIndex
          case RouterSchema.NumberMode.Bounded =>
            if !numIntegral then router.doubleIndex
            else if numberFitsInt then router.intIndex
            else if numDigits <= 19 then router.longIndex
            else router.doubleIndex
      val routerCase = RawSchema.routerCase(schema, index)
      if routerCase == null then
        raise(
          DecodeError
            .ExpectedType(schema.describeSelf, "a number")
            .atToken(spanAt(numberOffset))
        )
      checkOrRaise(decodeScannedNumberValue(routerCase.schema, numberOffset))(identity)

  private def numberFitsInt: Boolean =
    numIntegral && numDigits <= 10 &&
      numNegAcc >= (if numNeg then Int.MinValue.toLong else -Int.MaxValue.toLong)

  /** Feeds the already-scanned number into a case schema's primitive base, applying any mapped
    * chain on the way out — semantically the decode the case schema would have performed.
    */
  private def decodeScannedNumberValue(
      schema: RawSchema[?],
      numberOffset: Long
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        Result.task {
          decodeScannedNumberValue(mapped.base, numberOffset).check
          mapSlot(mapped.mapping).check
        }
      case RawSchema.Ref(_, target) =>
        decodeScannedNumberValue(target(), numberOffset)
      case RawSchema.Int =>
        Result.task {
          if !numberAsInt() then raise(scannedNumberTypeError(RawSchema.Int, numberOffset))
        }
      case RawSchema.Long =>
        Result.task {
          if !numberAsLong() then raise(scannedNumberTypeError(RawSchema.Long, numberOffset))
        }
      case RawSchema.Double =>
        Result.task { numberAsDouble() }
      case RawSchema.Float =>
        Result.task { numberAsFloat() }
      case RawSchema.String =>
        // a raw-number case stores the literal's exact text
        Result.task { pushString(rawNumberString()) }
      case other =>
        Result.Err(
          DecodeError.ExpectedType(other.describeSelf, "a number").atToken(spanAt(numberOffset))
        )

  private def scannedNumberTypeError(schema: RawSchema[?], numberOffset: Long): DecodeError =
    DecodeError
      .ExpectedType(schema.describeSelf, s"the number ${rawNumberString()}")
      .atToken(spanAt(numberOffset))

  // --- primitives ---

  protected final def decodeString(): Result[Unit, DecodeError] =
    Result.task {
      if tryScanStringSlice() then pushString(materializeSlice())
      else raise(expectedTypeAtCurrent(RawSchema.String))
    }

  protected final def decodeChar(): Result[Unit, DecodeError] = Result.task:
    if tryScanStringSlice() then
      val value = sliceAsChar()
      if value < 0 then
        raise(
          DecodeError
            .ExpectedType(RawSchema.Char.describeSelf, "a string")
            .atToken(spanAt(sliceQuoteAbsolute()))
        )
      pushChar(value.toChar)
    else raise(expectedTypeAtCurrent(RawSchema.Char))

  protected final def decodeInt(): Result[Unit, DecodeError] = Result.task:
    if tryScanNumber() then
      if !numberAsInt() then raise(scannedNumberTypeError(RawSchema.Int, numberStartAbsolute()))
    else raise(expectedTypeAtCurrent(RawSchema.Int))

  protected final def decodeLong(): Result[Unit, DecodeError] = Result.task:
    if tryScanNumber() then
      if !numberAsLong() then raise(scannedNumberTypeError(RawSchema.Long, numberStartAbsolute()))
    else raise(expectedTypeAtCurrent(RawSchema.Long))

  protected final def decodeDouble(): Result[Unit, DecodeError] = Result.task:
    if tryScanNumber() then numberAsDouble()
    else raise(expectedTypeAtCurrent(RawSchema.Double))

  protected final def decodeFloat(): Result[Unit, DecodeError] = Result.task:
    if tryScanNumber() then numberAsFloat()
    else raise(expectedTypeAtCurrent(RawSchema.Float))

  protected final def decodeBoolean(): Result[Unit, DecodeError] = Result.task:
    tryReadBoolean() match
      case BooleanTrue  => pushBoolean(true)
      case BooleanFalse => pushBoolean(false)
      case _            => raise(expectedTypeAtCurrent(RawSchema.Boolean))

  protected final def decodeNull(): Result[Unit, DecodeError] = Result.task:
    if tryReadNull() then pushRef(null)
    else raise(expectedTypeAtCurrent(RawSchema.Null))
