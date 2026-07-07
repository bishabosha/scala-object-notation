package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.RouterSchema
import scalanotation.schema.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise
import scalanotation.schema.RawSchema
import scalanotation.internal.Internal.breakErr

private[scalanotation] trait SchemaDecoders extends BaseDecoders:
  self: TokenStream =>

  private type StateDecodeResult[Repr] = Repr | Result.Err[DecodeError]

  protected final def decodeBase(schema: RawSchema[?]): Result[Unit, DecodeError] =
    decodeBase(schema, allowTopLevelArrow = true)

  private def decodeBase(
      schema: RawSchema[?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    schema match
      case schema: RawSchema.Atomic =>
        // primitive decodes never recurse into the input (Mapped/Ref chains are schema-bounded),
        // so they skip the nesting guard entirely
        decodePrimitiveBase(schema, allowTopLevelArrow)
      case schema: RawSchema.Collection =>
        if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
        else
          val result = decodeCollectionBase(schema, allowTopLevelArrow)
          exitNesting()
          result
      case _ =>
        if !enterNesting() then Result.Err(nestingLimitError().atToken(currentSpan()))
        else
          val result = decodeCompositeBase(schema, allowTopLevelArrow)
          exitNesting()
          result

  private def decodeMappedBase(
      schema: RawSchema.Mapped[?, ?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    Result.task {
      decodeBase(schema.base, allowTopLevelArrow).check
      mapSlot(schema.mapping).check
    }

  private def decodeCompositeBase(
      schema: RawSchema[?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    schema match
      case sc: RawSchema.NamedTuple[?] =>
        decodeNamedTuple(sc)
      case sc: RawSchema.Tuple[?] =>
        decodeTuple(sc, allowTopLevelArrow)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        decodePartialNamedTuple(base, alreadySeenField)
      case sc: RawSchema.Sum[?] =>
        decodeSum(sc)
      case sc: RawSchema.DiscriminatorSum[?] =>
        decodeDiscriminatorSum(sc)
      case router: RawSchema.Router[?] =>
        decodeRouter(router, allowTopLevelArrow)
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeCurrent()))

  private def decodeCollectionBase(
      schema: RawSchema[?] & RawSchema.Collection,
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    schema match
      case sc: RawSchema.Vector[?, ?] =>
        decodeVector(sc)
      case sc: RawSchema.PairSeq[?, ?, ?] =>
        decodePairSeq(sc)
      case sc: RawSchema.Dict[?, ?] =>
        decodeDict(sc)
      case sc: RawSchema.Option[?] =>
        decodeOption(sc, allowTopLevelArrow)
      case sc: RawSchema.TupleOf[?, ?] =>
        decodeTupleOf(sc, allowTopLevelArrow)

  private def decodePrimitiveBase(
      schema: RawSchema[?] & RawSchema.Atomic,
      allowTopLevelArrow: Boolean
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
        decodeMappedBase(mapped, allowTopLevelArrow)
      case RawSchema.Null =>
        decodeNull()
      case RawSchema.Char =>
        decodeChar()
      case RawSchema.Float =>
        decodeFloat()
      case ref: RawSchema.Ref[?] =>
        decodeBase(ref.target(), allowTopLevelArrow)

  protected final def decodeRouter(
      schema: RawSchema.Router[?]
  ): Result[Unit, DecodeError] =
    decodeRouter(schema, allowTopLevelArrow = true)

  protected final def decodeRouter(
      schema: RawSchema.Router[?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    Result.task:
      val construct = currentRouterConstruct(schema.numberMode)
      if construct == null then
        raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      else decodeRouterCase(schema, construct, allowTopLevelArrow).check

  private def currentRouterConstruct(
      numberMode: RouterSchema.NumberMode
  ): RouterSchema.RouterConstruct | Null =
    import RouterSchema.RouterConstruct

    currentKind() match
      case TokenKind.LParen =>
        if parenStartsRecord() then RouterConstruct.Record
        else RouterConstruct.Tuple
      case TokenKind.EmptyTupleId                          => RouterConstruct.Tuple
      case TokenKind.TupleId                               => RouterConstruct.Tuple
      case TokenKind.VectorId                              => RouterConstruct.Vector
      case TokenKind.LBracket if collectionLiteralsEnabled =>
        RouterConstruct.Vector
      case TokenKind.StringLit => RouterConstruct.String
      case TokenKind.CharLit   => RouterConstruct.Char
      case TokenKind.IntLit    =>
        numberConstruct(RouterConstruct.Int, numberMode)
      case TokenKind.LongLit =>
        numberConstruct(RouterConstruct.Long, numberMode)
      case TokenKind.FloatLit =>
        numberConstruct(RouterConstruct.Float, numberMode)
      case TokenKind.DoubleLit =>
        numberConstruct(RouterConstruct.Double, numberMode)
      case TokenKind.TrueKw | TokenKind.FalseKw =>
        RouterConstruct.Boolean
      case TokenKind.NullKw =>
        RouterConstruct.Null
      case TokenKind.Minus =>
        peekKind() match
          case TokenKind.IntLit =>
            numberConstruct(RouterConstruct.Int, numberMode)
          case TokenKind.LongLit =>
            numberConstruct(RouterConstruct.Long, numberMode)
          case TokenKind.FloatLit =>
            numberConstruct(RouterConstruct.Float, numberMode)
          case TokenKind.DoubleLit =>
            numberConstruct(RouterConstruct.Double, numberMode)
          case _ =>
            RouterConstruct.RawNumber
      case _ => null

  private def decodeRouterCase(
      schema: RawSchema.Router[?],
      construct: RouterSchema.RouterConstruct,
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    Result.task:
      val routerCase = RawSchema.routerCase(schema, schema.router.indexFor(construct))
      if routerCase == null then
        raise(
          DecodeError
            .ExpectedType(schema.describeSelf, describeCurrent())
            .atToken(currentSpan())
        )
      decodeBase(routerCase.schema).check
      if allowTopLevelArrow && collectionLiteralsEnabled && currentKind() == TokenKind.Arrow then
        // Router cases usually materialize reference values already; accept primitive boxing here
        // rather than carrying a typed slot snapshot for the arrow-tuple fallback.
        val firstValue = pullAny()
        decodeRouterArrowTupleCase(schema, firstValue).check

  private def decodeRouterArrowTupleCase(
      schema: RawSchema.Router[?],
      firstValue: Any
  ): Result[Unit, DecodeError] =
    Result.task:
      val tupleCase        = RawSchema.routerCase(schema, schema.router.tupleIndex)
      var cachedFirstValue = firstValue
      while currentKind() == TokenKind.Arrow do
        val arrowOffset = currentOffset()
        advance()
        if tupleCase == null then raise(missingRouterTupleCaseError(schema, arrowOffset))
        finishRouterTupleCase(
          tupleCase.schema,
          cachedFirstValue,
          null,
          decodeSecondFromInput = true,
          arrowOffset = arrowOffset
        ).check
        cachedFirstValue = pullAny()

  private def numberConstruct(
      bounded: RouterSchema.RouterConstruct,
      numberMode: RouterSchema.NumberMode
  ): RouterSchema.RouterConstruct =
    numberMode match
      case RouterSchema.NumberMode.Bounded => bounded
      case RouterSchema.NumberMode.Raw     => RouterSchema.RouterConstruct.RawNumber

  protected final def decodeTuple(
      schema: RawSchema.Tuple[?]
  ): Result[Unit, DecodeError] =
    decodeTuple(schema, allowTopLevelArrow = true)

  protected final def decodeTuple(
      schema: RawSchema.Tuple[?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeTupleWithRead(schema, read, allowTopLevelArrow))

  private def decodeTupleWithRead[Repr, A](
      schema: RawSchema.Tuple[?],
      read: Reader.TupleBuilder[Repr, A],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    withBorrowSlots(read.slotsFactory) { pooled =>
      Result.task {
        if allowTopLevelArrow && collectionLiteralsEnabled && !currentStartsTupleLike then
          decodeArrowFixedTuple(schema, read, pooled).check
        else decodeFixedTupleLike(schema, read, pooled).check
      }
    }

  private def decodeFixedTupleLike[Repr, A](
      schema: RawSchema.Tuple[?],
      read: Reader.TupleBuilder[Repr, A],
      pooled: scalanotation.BuilderSlots | Null
  ): Result[Unit, DecodeError] =
    Result.task:
      val slots    = schema.slots
      val expected = slots.length
      val state0   = read.initPooled(expected, pooled)
      currentKind() match
        case TokenKind.EmptyTupleId =>
          val emptyTupleOffset = currentOffset()
          advance()
          if expected > 0 then
            raise(DecodeError.FieldCountMismatch(expected, 0).atToken(spanAt(emptyTupleOffset)))
          pushRef(read.finish(state0))
        case TokenKind.TupleId =>
          decodeFixedSingletonTuple(read, slots, state0, expected).check
        case TokenKind.LParen =>
          decodeFixedParenTuple(schema, read, slots, state0, expected).check
        case _ =>
          raise(
            DecodeError
              .ExpectedType(schema.describeSelf, describeCurrent())
              .atToken(currentSpan())
          )

  private def decodeFixedSingletonTuple[Repr, A](
      read: Reader.TupleBuilder[Repr, A],
      slots: IArray[RawSchema[?]],
      state0: Repr,
      expected: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      val tupleOffset = currentOffset()
      advance()
      if currentKind() == TokenKind.LParen then advance()
      else raise(expectedTupleApplyError())
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.FieldCountMismatch(1, 0).atToken(currentSpan()))
      if expected == 0 then raise(DecodeError.FieldCountMismatch(0, 1).atToken(spanAt(tupleOffset)))

      val slotError = decodeFixedTupleSlot(slots, 0)
      if slotError != null then raise(slotError)

      currentKind() match
        case TokenKind.RParen =>
          advance()
          if expected > 1 then
            raise(DecodeError.FieldCountMismatch(expected, 1).atToken(spanAt(tupleOffset)))
          val state = addSlot(read)(state0, 0)
          pushRef(read.finish(state))
        case TokenKind.Comma =>
          raise(DecodeError.FieldCountMismatch(1, 2).atToken(currentSpan()))
        case _ =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def expectedTupleApplyError(): DecodeError =
    DecodeError.ExpectedType("Tuple(...)", describeCurrent()).atToken(currentSpan())

  private def decodeFixedParenTuple[Repr, A](
      schema: RawSchema.Tuple[?],
      read: Reader.TupleBuilder[Repr, A],
      slots: IArray[RawSchema[?]],
      state0: Repr,
      expected: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      val openOffset = currentOffset()
      advance()
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))
      if expected == 0 then raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))

      val slotError = decodeFixedTupleSlot(slots, 0)
      if slotError != null then raise(slotError)

      currentKind() match
        case TokenKind.Comma =>
          val state = addSlot(read)(state0, 0)
          decodeFixedTupleCommaTail(read, slots, state, expected).check
        case TokenKind.RParen =>
          raise(
            DecodeError
              .ExpectedType(schema.describeSelf, "(...)")
              .atToken(spanAt(openOffset))
          )
        case _ =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def decodeFixedTupleCommaTail[Repr, A](
      read: Reader.TupleBuilder[Repr, A],
      slots: IArray[RawSchema[?]],
      state0: Repr,
      expected: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      var state                         = state0
      var count                         = 1
      var closingSpan: DecodeError.Span = currentSpan()
      var done                          = false
      while !done do
        currentKind() match
          case TokenKind.Comma =>
            advance()
            currentKind() match
              case TokenKind.RParen =>
                closingSpan = currentSpan()
                done = true
              case _ =>
                if expected > 0 && count >= expected then
                  raise(DecodeError.FieldCountMismatch(expected, count + 1).atToken(currentSpan()))
                val slotError = decodeFixedTupleSlot(slots, count)
                if slotError != null then raise(slotError)
                state = addSlot(read)(state, count)
                count += 1
          case TokenKind.RParen =>
            closingSpan = currentSpan()
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
      advance()
      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(closingSpan))
      if expected > 0 && count != expected then
        raise(DecodeError.FieldCountMismatch(expected, count).atToken(closingSpan))
      pushRef(read.finish(state))

  private def decodeFixedTupleSlot(
      slots: IArray[RawSchema[?]],
      index: Int
  ): DecodeError | Null =
    decodeBase(slots(index)) match
      case Result.Err(error) => error.atPath(s"[$index]")
      case _                 => null

  private def currentStartsTupleLike: Boolean =
    currentKind() match
      case TokenKind.LParen | TokenKind.EmptyTupleId | TokenKind.TupleId => true
      case _                                                             => false

  private def decodeArrowFixedTuple[Repr, A](
      schema: RawSchema.Tuple[?],
      read: Reader.TupleBuilder[Repr, A],
      pooled: scalanotation.BuilderSlots | Null
  ): Result[Unit, DecodeError] =
    Result.task:
      val slots = schema.slots
      if slots.length != 2 then
        raise(DecodeError.FieldCountMismatch(slots.length, 2).atToken(currentSpan()))

      var state = read.initPooled(slots.length, pooled)
      checkOrRaise(decodeBase(slots(0), allowTopLevelArrow = true))(_.atPath("[0]"))
      state = addSlot(read)(state, 0)
      if currentKind() == TokenKind.Arrow then advance()
      else raise(expectedArrowError())
      checkOrRaise(decodeBase(slots(1), allowTopLevelArrow = false))(_.atPath("[1]"))
      state = addSlot(read)(state, 1)
      pushRef(read.finish(state))

  protected final def decodeTupleSlotValue(
      slots: IArray[RawSchema[?]],
      index: Int
  ): Result[Unit, DecodeError] =
    decodeBase(slots(index)) match
      case Result.Err(error) => Result.Err(error.atPath(s"[$index]"))
      case ok                => ok

  private def parenStartsRecord(): Boolean =
    isFieldNameStart(peekKind()) && peekSecondKind() == TokenKind.Equals

  private inline val FieldNameStartMask =
    (1 << TokenKind.Identifier) |
      (1 << TokenKind.VectorId) |
      (1 << TokenKind.EmptyTupleId) |
      (1 << TokenKind.TupleId) |
      (1 << TokenKind.Plus) |
      (1 << TokenKind.Minus)

  private inline def isFieldNameStart(kind: Int): Boolean =
    ((FieldNameStartMask >>> kind) & 1) != 0

  protected final def decodeNamedTuple(
      schema: RawSchema.NamedTuple[?]
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { slots =>
        // the seen-field bitset only matters when fields can be skipped (out-of-schema-order
        // duplicates); in ordered mode the decoded set is always the contiguous prefix
        if schema.allowSkippedNullableFields then
          fieldIndexSetPool.withBorrowed { seenFields =>
            decodeRecordFields(schema, read, slots, seenFields)
          }
        else decodeRecordFields(schema, read, slots, null)
      }
    }

  /** Decodes one named-tuple record with a single loop covering both ordered and
    * skipped-nullable-field schemas. The happy path reads structure at the char level — a
    * plain-identifier name slice compared against the expected field name, `=`, the value's own
    * token, and the `,`/`)` separator — so a primitive field materializes exactly one token (its
    * value). Any deviation (pending tokens, non-plain names, mismatches, skips) drops to the cold
    * helpers, which fall back to the token path over the same characters and preserve the generic
    * loop's semantics and errors. The loop body stays small so the JIT can inline the char-level
    * readers into it.
    */
  private def decodeRecordFields(
      schema: RawSchema.NamedTuple[?],
      read: RawSchema.NamedTupleRead,
      slots: scalanotation.BuilderSlots | Null,
      seenFields: Internal.FieldIndexSet | Null
  ): Result[Unit, DecodeError] =
    Result.task {
      val fields    = schema.fields
      val allowSkip = schema.allowSkippedNullableFields
      if seenFields != null then seenFields.reset(fields.length)
      schema.isValidNamedTuple(namesPool).check
      val fieldPlans        = schema.fieldPlans
      val plans             = fieldPlans.kinds
      val nameChars         = fieldPlans.nameChars
      var state: read.State = read.init(fields.length, slots)

      if !tryReadPunctChar('(') then consumeRecordLParenToken(schema).check

      var fieldIndex                   = 0 // the next expected schema field
      var decodedCount                 = 0 // fields actually present in the input
      var lastFieldName: String | Null = null
      var closingOffset                = 0

      if tryConsumeRParen() then
        closingOffset = consumedRParenOffset
        if fields.nonEmpty then
          raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(closingOffset)))
      else
        var done        = false
        var headerProbe = probeRecordHeader(fields, plans, nameChars, fieldIndex)
        while !done do
          // --- field name (+ '='): fused char-level header against the expected field (read by
          // the previous separator scan after the first field), else the cold resolver ---
          var decodedIndex = -1
          var nameOffset   = 0
          if headerProbe == Tokenizer.HeaderMatched then
            decodedIndex = fieldIndex // name and '=' both consumed
            nameOffset = sliceNameOffset()
          else
            if headerProbe == Tokenizer.HeaderNameSlice then
              nameOffset = sliceNameOffset()
              resolveRecordFieldSlow(
                fields,
                fieldPlans,
                seenFields,
                allowSkip,
                fieldIndex,
                true
              ).check
              decodedIndex = pullControl()
            else
              nameOffset = currentOffset()
              resolveRecordFieldSlow(
                fields,
                fieldPlans,
                seenFields,
                allowSkip,
                fieldIndex,
                false
              ).check
              decodedIndex = pullControl()

            if decodedIndex > fieldIndex then
              // None-fill the skipped range — the resolver validated that every schema in it is a
              // skippable nullable
              while fieldIndex < decodedIndex do
                state = read.add(state, fieldIndex, None)
                fieldIndex += 1

            // --- '=' ---
            if !tryReadEqualsChar() then consumeEqualsToken().check

          // --- value: decode and append in one plan dispatch ---
          val expectedField = fields(decodedIndex)
          decodeValueInto(read)(
            state,
            decodedIndex,
            plans(decodedIndex),
            expectedField.schema
          ) match
            case err: Result.Err[DecodeError] =>
              raise(recordFieldValueError(err.error, expectedField, nameOffset))
            case next =>
              state = next.asInstanceOf[read.State]
              if seenFields != null then seenFields.mark(decodedIndex)
              fieldIndex = decodedIndex + 1
              decodedCount += 1
              lastFieldName = expectedField.name

          // --- separator ---
          tryReadSeparatorChar(')') match
            case Tokenizer.SeparatorComma =>
              headerProbe = probeRecordHeader(fields, plans, nameChars, fieldIndex)
            case Tokenizer.SeparatorClosing | Tokenizer.SeparatorTrailingClosing =>
              closingOffset = charScanOffset()
              done = true
            case _ =>
              if { recordSeparatorToken().check; pullControl() == RecordClosed } then
                closingOffset = consumedRParenOffset
                done = true
              else headerProbe = probeRecordHeader(fields, plans, nameChars, fieldIndex)

      if allowSkip then
        state = fillTrailingSkippedNullableFields(read)(fields, state, fieldIndex)
        fieldIndex = pullSkipFillIndex()

      val decodedFieldCount = if allowSkip then fieldIndex else decodedCount
      if decodedFieldCount != fields.length then
        raise(
          fieldCountMismatchAtClosing(
            fields,
            namedTupleParseResult.push(decodedCount, lastFieldName, closingOffset),
            lastFieldName
          )
        )
      else pushRef(read.finish(state))
    }

  /** probes the next field name at the char level — the record loop's entry and token-fallback
    * header read: 1 = expected header consumed, 0 = a name slice consumed, -1 = token path
    */
  private def probeRecordHeader(
      fields: IArray[Field],
      plans: Array[scala.Byte],
      nameChars: Array[Array[scala.Char]],
      atField: Int
  ): Int =
    if atField < fields.length && plans(atField) != RawSchema.FieldPlan.TokenName then
      expectFieldHeader(nameChars(atField))
    else if tryReadNameSlice() then Tokenizer.HeaderNameSlice
    else Tokenizer.HeaderNone

  /** Cold path of the record loop's name step: resolves an arriving field name that was not the
    * exact expected header. The name is first normalized to a generic token — a consumed char-level
    * slice rewinds and rescans, so classification and errors are the token path's — and one
    * resolution algorithm serves ordered and skippable schemas alike (ordered mode is an empty skip
    * walk). On success the name is consumed and the resolved index is left in the control slot; the
    * caller None-fills up to it (the walk only passes skippable nullables).
    */
  private def resolveRecordFieldSlow(
      fields: IArray[Field],
      fieldPlans: RawSchema.FieldPlans,
      seenFields: Internal.FieldIndexSet | Null,
      allowSkip: Boolean,
      fieldIndexStart: Int,
      sliceRead: Boolean
  ): Result[Unit, DecodeError] = Result.task {
    if sliceRead then rescanNameSliceAsToken()
    val nameOffset = currentOffset()
    if !isFieldNameStart(currentKind()) then
      raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))

    val nullable           = fieldPlans.nullable
    var index              = fieldIndexStart
    val expectedBeforeSkip = if index < fields.length then fields(index) else null
    while allowSkip && index < fields.length
      && !currentFieldNameMatches(fields(index).name)
      && nullable(index)
    do index += 1

    if index >= fields.length then
      duplicateDecodedFieldError(fields, seenFields, index, nameOffset).check
      if expectedBeforeSkip == null then
        raise(currentFieldCountMismatchError(nameOffset, fields.length, index + 1))
      else raise(currentFieldOrderMismatchError(nameOffset, expectedBeforeSkip.name))
    else if !currentFieldNameMatches(fields(index).name) then
      duplicateDecodedFieldError(fields, seenFields, index, nameOffset).check
      raise(currentFieldOrderMismatchError(nameOffset, fields(index).name))
    if seenFields != null && seenFields.contains(index) then
      raise(makeDuplicateKnownFieldError(fields(index).name, nameOffset))
    advance() // consume the field-name token
    pushControl(index)
  }

  /** cold `(` fallback for the record entry: a pending token or a non-record shape */
  private def consumeRecordLParenToken(schema: RawSchema[?]): Result[Unit, DecodeError] =
    Result.task {
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )
    }

  /** cold `=` fallback: a pending token or a non-`=` shape */
  private def consumeEqualsToken(): Result[Unit, DecodeError] = Result.task {
    if currentKind() == TokenKind.Equals then advance()
    else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))
  }

  /** [[recordSeparatorToken]]'s control-slot value when the record closed */
  private inline val RecordClosed = 1

  /** [[recordSeparatorToken]]'s control-slot value when a comma was consumed — a field follows */
  private inline val RecordContinues = 0

  /** Cold separator fallback on the token path: leaves [[RecordContinues]] or [[RecordClosed]] in
    * the control slot (the closing offset in [[consumedRParenOffset]]).
    */
  private def recordSeparatorToken(): Result[Unit, DecodeError] = Result.task {
    currentKind() match
      case TokenKind.Comma =>
        advance()
        if currentKind() == TokenKind.RParen then
          consumedRParenOffset = currentOffset()
          advance()
          pushControl(RecordClosed)
        else pushControl(RecordContinues)
      case TokenKind.RParen =>
        consumedRParenOffset = currentOffset()
        advance()
        pushControl(RecordClosed)
      case _ => raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
  }

  /** cold decoration of a field value's decode error */
  private def recordFieldValueError(
      error: DecodeError,
      field: Field,
      nameOffset: Int
  ): DecodeError =
    error.atPath(s".${field.name}").atToken(spanAt(nameOffset))

  /** Decodes a planned field value and appends it into the named-tuple builder state in a single
    * plan dispatch: the plan already names the live typed slot, so the [[addSlot]] kind switch and
    * the [[PushSlots]] round trip collapse into a direct typed `add`. Returns the error instead of
    * the new state when the value fails to decode.
    */
  private def decodeValueInto(
      read: RawSchema.NamedTupleRead
  )(
      state: read.State,
      index: Int,
      plan: Byte,
      schema: RawSchema[?]
  ): read.State | Result.Err[DecodeError] =
    inline def added[E](inline decoded: Result[Unit, E])(inline add: => read.State) =
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
      val result = decodeOption(schema.asInstanceOf[RawSchema.Option[?]], allowTopLevelArrow = true)
      exitNesting()
      result

  /** [[decodeValueInto]] for vector elements — same single dispatch onto the vector builder */
  private def decodeElementInto[Elem, Repr, A](
      read: Reader.VectorBuilder[Elem, Repr, A]
  )(
      values: Repr,
      plan: Byte,
      schema: RawSchema[?]
  ): Repr | Result.Err[DecodeError] =
    inline def added[E](inline decoded: Result[Unit, E])(inline add: => Repr) =
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

  /** offset of the `)` consumed by the most recent successful [[tryConsumeRParen]] */
  private var consumedRParenOffset = 0

  /** Consumes a closing `)` from either stream state — char-level between tokens, or the pending
    * current token. False consumes nothing.
    */
  private def tryConsumeRParen(): Boolean =
    if betweenTokens then
      if tryReadPunctChar(')') then
        consumedRParenOffset = charScanOffset()
        true
      else false
    else if currentKind() == TokenKind.RParen then
      consumedRParenOffset = currentOffset()
      advance()
      true
    else false

  private def fillTrailingSkippedNullableFields(read: RawSchema.NamedTupleRead)(
      fields: IArray[Field],
      state: read.State,
      fieldIndex: Int
  ): read.State =
    fillSkippedNullableFields(read)(fields, state, fieldIndex, "")

  private def makeDuplicateKnownFieldError(
      name: String,
      nameOffset: Int
  ): DecodeError =
    DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset))

  /** [[duplicateKnownFieldError]] for the record loop: without a bitset (ordered mode) the decoded
    * set is exactly the contiguous prefix `fields(0 until seenCount)`.
    */
  private def duplicateDecodedFieldError(
      fields: IArray[Field],
      seenFields: Internal.FieldIndexSet | Null,
      seenCount: Int,
      nameOffset: Int
  ): Result[Unit, DecodeError] =
    if seenFields != null then duplicateKnownFieldError(fields, seenFields, nameOffset, null)
    else
      Result.task {
        var index = 0
        val limit = math.min(seenCount, fields.length)
        while index < limit do
          val name = fields(index).name
          if currentFieldNameMatches(name) then
            raise(makeDuplicateKnownFieldError(name, nameOffset))
          index += 1
      }

  private def duplicateKnownFieldError(
      fields: IArray[Field],
      seenFields: Internal.FieldIndexSet,
      nameOffset: Int,
      alreadySeenField: String | Null
  ): Result[Unit, DecodeError] = Result.task {
    if alreadySeenField != null && currentFieldNameMatches(alreadySeenField) then
      raise(makeDuplicateKnownFieldError(alreadySeenField, nameOffset))
    else
      var index = seenFields.nextMarked(0)
      while index >= 0 do
        val name = fields(index).name
        if currentFieldNameMatches(name) then raise(makeDuplicateKnownFieldError(name, nameOffset))
        index = seenFields.nextMarked(index + 1)
  }

  private def currentFieldCountMismatchError(
      nameOffset: Int,
      expected: Int,
      actual: Int
  ): DecodeError =
    val actualName = currentFieldName()
    DecodeError
      .FieldCountMismatch(expected, actual)
      .atPath(s".${actualName}")
      .atToken(spanAt(nameOffset))

  private def currentFieldOrderMismatchError(nameOffset: Int, expected: String): DecodeError =
    val actualName = currentFieldName()
    DecodeError
      .FieldOrderMismatch(expected, actualName)
      .atPath(s".${actualName}")
      .atToken(spanAt(nameOffset))

  private def fieldCountMismatchAtClosing(
      fields: IArray[Field],
      parsed: NamedTupleParseResult,
      lastFieldName: String | Null
  ): DecodeError =
    var err = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
    if lastFieldName != null then err = err.atPath(s".${lastFieldName}")
    err.atToken(spanAt(parsed.closingOffset))

  protected final def decodeSum(schema: RawSchema.Sum[?]): Result[Unit, DecodeError] =
    Result.task:
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
      decodeSumField(schema, actualName, nameOffset, fieldIndex = 0).check
      finishSumTuple().check
      // the decoded case value remains in the live slot

  private def decodeSumField(
      schema: RawSchema.Sum[?],
      actualName: String,
      nameOffset: Int,
      fieldIndex: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      if fieldIndex >= 1 then
        raise(
          DecodeError
            .FieldCountMismatch(1, fieldIndex + 1)
            .atPath(s".${actualName}")
            .atToken(spanAt(nameOffset))
        )
      else
        val sumCase = RawSchema.findCase(schema, actualName) match
          case null =>
            raise(
              DecodeError
                .UnexpectedField(actualName)
                .atPath(s".${actualName}")
                .atToken(spanAt(nameOffset))
            )
          case c => c
        checkOrRaise(decodeBase(sumCase.schema))(_.atPath(s".${actualName}"))

  private def finishSumTuple(): Result[Unit, DecodeError] =
    Result.task:
      currentKind() match
        case TokenKind.RParen =>
          advance()
        case TokenKind.Comma =>
          advance()
          if currentKind() == TokenKind.RParen then advance()
          else
            val nameOffset = currentOffset()
            parseNamedFieldStart().check
            val actualName = pullStringStrict()
            raise(
              DecodeError
                .FieldCountMismatch(1, 2)
                .atPath(s".${actualName}")
                .atToken(spanAt(nameOffset))
            )
        case _ =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

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
    val startError = consumeDiscriminatorSumStart(schema)
    if startError != null then return Result.Err(startError)

    val nameOffset = currentOffset()
    parseNamedFieldStart() match
      case Result.Err(error) => return Result.Err(error)
      case _                 =>

    val actualName         = pullStringStrict()
    val discriminatorField = schema.discriminatorField
    if actualName != discriminatorField then
      return Result.Err(discriminatorFieldOrderError(discriminatorField, actualName, nameOffset))

    decodeString() match
      case Result.Err(error) => return Result.Err(error.atPath(s".$actualName"))
      case _                 =>

    val caseName = pullStringStrict()
    val sumCase  = RawSchema.findCase(schema, caseName)
    if sumCase == null then
      return Result.Err(unexpectedDiscriminatorCaseError(caseName, actualName, nameOffset))

    val endError = consumeDiscriminatorPayloadStart()
    if endError != null then Result.Err(endError)
    else sumCase

  private def consumeDiscriminatorSumStart(
      schema: RawSchema.DiscriminatorSum[?]
  ): DecodeError | Null =
    if currentKind() == TokenKind.LParen then
      advance()
      if currentKind() == TokenKind.RParen then
        DecodeError.UnitValueNotAllowed().atToken(currentSpan())
      else null
    else DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  private def discriminatorFieldOrderError(
      expected: String,
      actualName: String,
      nameOffset: Int
  ): DecodeError =
    DecodeError
      .FieldOrderMismatch(expected, actualName)
      .atPath(s".$actualName")
      .atToken(spanAt(nameOffset))

  private def findDiscriminatorSumCase(
      schema: RawSchema.DiscriminatorSum[?],
      caseName: String
  ): RawSchema.SumCase | Null =
    RawSchema.findCase(schema, caseName)

  private def unexpectedDiscriminatorCaseError(
      caseName: String,
      discriminatorName: String,
      nameOffset: Int
  ): DecodeError =
    DecodeError
      .UnexpectedField(caseName)
      .atPath(s".$discriminatorName")
      .atToken(spanAt(nameOffset))

  private def consumeDiscriminatorPayloadStart(): DecodeError | Null =
    currentKind() match
      case TokenKind.Comma =>
        advance()
        null
      case TokenKind.RParen =>
        null
      case _ =>
        DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan())

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
        decodePartialNamedTuple(namedTuple, alreadySeenField)
      case RawSchema.Null =>
        decodeEmptyPartialNamedTuple()
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeCurrent()))

  protected final def decodeEmptyPartialNamedTuple(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.RParen then
        advance()
        pushRef(null)
      else raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))

  protected final def decodePartialNamedTuple(
      schema: RawSchema.NamedTuple[?],
      alreadySeenField: String
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { slots =>
        fieldIndexSetPool.withBorrowed { seenFields =>
          decodePartialNamedTupleWithResources(schema, alreadySeenField, read, slots, seenFields)
        }
      }
    }

  private def decodePartialNamedTupleWithResources(
      schema: RawSchema.NamedTuple[?],
      alreadySeenField: String,
      read: RawSchema.NamedTupleRead,
      slots: scalanotation.BuilderSlots | Null,
      seenFields: Internal.FieldIndexSet
  ): Result[Unit, DecodeError] =
    Result.task {
      val fields = schema.fields
      seenFields.reset(fields.length)
      val alreadySeenIndex = indexOfField(fields, alreadySeenField)
      if alreadySeenIndex >= 0 then seenFields.mark(alreadySeenIndex)

      schema.isValidNamedTuple(namesPool).check
      var state: read.State            = read.init(fields.length, slots)
      var fieldIndex                   = 0
      var lastFieldName: String | Null = null

      val parsedResult = parsePartialKnownNamedTupleStructure(schema) {
        (nameOffset, parsedFieldIndex) =>
          Result.task {
            val decodedIndex = {
              if schema.allowSkippedNullableFields then
                val expectedBeforeSkip =
                  if fieldIndex < fields.length then fields(fieldIndex) else null
                while fieldIndex < fields.length
                  && !currentFieldNameMatches(fields(fieldIndex).name)
                  && TokenDecoder.isNullable(fields(fieldIndex).schema)
                do
                  state = read.add(state, fieldIndex, None)
                  fieldIndex += 1

                if fieldIndex >= fields.length then
                  duplicateKnownFieldError(
                    fields,
                    seenFields,
                    nameOffset,
                    alreadySeenField
                  ).check
                  if expectedBeforeSkip == null then
                    raise(
                      currentFieldCountMismatchError(
                        nameOffset,
                        fields.length,
                        parsedFieldIndex + 1
                      )
                    )
                  else
                    raise(
                      currentFieldOrderMismatchError(
                        nameOffset,
                        expectedBeforeSkip.name
                      )
                    )
                else
                  val expectedField = fields(fieldIndex)
                  if currentFieldNameMatches(expectedField.name) then fieldIndex
                  else
                    duplicateKnownFieldError(
                      fields,
                      seenFields,
                      nameOffset,
                      alreadySeenField
                    ).check
                    raise(currentFieldOrderMismatchError(nameOffset, expectedField.name))
              else if parsedFieldIndex >= fields.length then
                duplicateKnownFieldError(
                  fields,
                  seenFields,
                  nameOffset,
                  alreadySeenField
                ).check
                raise(
                  currentFieldCountMismatchError(
                    nameOffset,
                    fields.length,
                    parsedFieldIndex + 1
                  )
                )
              else
                val expectedField = fields(parsedFieldIndex)
                if currentFieldNameMatches(expectedField.name) then parsedFieldIndex
                else
                  duplicateKnownFieldError(
                    fields,
                    seenFields,
                    nameOffset,
                    alreadySeenField
                  ).check
                  raise(
                    currentFieldOrderMismatchError(nameOffset, expectedField.name)
                  )
            }
            if seenFields.contains(decodedIndex) then
              raise(makeDuplicateKnownFieldError(fields(decodedIndex).name, nameOffset))
            else
              val expectedField = fields(decodedIndex)
              parseNamedFieldStartNoPush().check
              decodeBase(expectedField.schema)
                .mapErr(error =>
                  error
                    .atPath(s".${expectedField.name}")
                    .atToken(spanAt(nameOffset))
                )
                .check
              state = addSlot(read)(state, decodedIndex)
              seenFields.mark(decodedIndex)
              fieldIndex = decodedIndex + 1
              lastFieldName = expectedField.name
          }
      }

      parsedResult match
        case err: Result.Err[DecodeError]  => breakErr(err)
        case parsed: NamedTupleParseResult =>
          if schema.allowSkippedNullableFields then
            state = fillTrailingSkippedNullableFields(read)(fields, state, fieldIndex)
            fieldIndex = pullSkipFillIndex()

          val decodedFieldCount =
            if schema.allowSkippedNullableFields then fieldIndex else parsed.fieldCount
          if decodedFieldCount != fields.length then
            raise(fieldCountMismatchAtClosing(fields, parsed, lastFieldName))
          else pushRef(read.finish(state))
    }

  private def indexOfField(fields: IArray[Field], name: String): Int =
    var index = 0
    while index < fields.length do
      if fields(index).name == name then return index
      index += 1
    -1

  protected final def decodeVector(schema: RawSchema.Vector[?, ?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeVectorWithRead(schema, read))

  private def decodeVectorWithRead[Elem, Repr, A](
      schema: RawSchema.Vector[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A]
  ): Result[Unit, DecodeError] =
    Result.task:
      val closingKind = currentVectorClosingKind()
      if closingKind < 0 then raise(expectedVectorStartError(schema))

      advanceVectorStart(closingKind)

      var values      = read.init()
      val closingChar = if closingKind == TokenKind.RParen then ')' else ']'
      if tryConsumeClosing(closingKind, closingChar) then ()
      else
        var indexInVector = 0
        var done          = false
        val elementPlan   = RawSchema.valuePlanOf(schema.element)
        while !done do
          decodeElementInto(read)(values, elementPlan, schema.element) match
            case err: Result.Err[DecodeError] =>
              raise(err.error.atPath(s"[$indexInVector]"))
            case next =>
              values = next.asInstanceOf[Repr]
          indexInVector += 1

          tryReadSeparatorChar(closingChar) match
            case Tokenizer.SeparatorComma => () // the next element follows
            case Tokenizer.SeparatorClosing | Tokenizer.SeparatorTrailingClosing =>
              done = true // closing consumed (possibly via a trailing comma)
            case _ =>
              // token fallback: a pending token (e.g. after a string value) or another shape
              currentKind() match
                case TokenKind.Comma =>
                  advance()
                  if tryConsumeClosing(closingKind, closingChar) then done = true
                case kind if kind == closingKind =>
                  advance()
                  done = true
                case _ =>
                  raise(expectedVectorClosingError(closingKind))

      pushRef(read.finish(values))

  /** Consumes the collection-closing token from either stream state — char-level between tokens, or
    * the pending current token. False consumes nothing.
    */
  private def tryConsumeClosing(closingKind: Int, closingChar: Char): Boolean =
    if betweenTokens then tryReadPunctChar(closingChar)
    else if currentKind() == closingKind then
      advance()
      true
    else false

  private def currentVectorClosingKind(): Int =
    if currentKind() == TokenKind.VectorId && peekKind() == TokenKind.LParen then TokenKind.RParen
    else if collectionLiteralsEnabled && currentKind() == TokenKind.LBracket then TokenKind.RBracket
    else -1

  private def advanceVectorStart(closingKind: Int): Unit =
    if closingKind == TokenKind.RParen then
      advance()
      advance()
    else advance()

  private def vectorElementSeparator(closingKind: Int): Int =
    currentKind() match
      case TokenKind.Comma =>
        advance()
        if currentKind() == closingKind then 1 else 0
      case kind if kind == closingKind =>
        1
      case _ =>
        -1

  private def expectedVectorStartError(schema: RawSchema[?]): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  private def expectedVectorClosingError(closingKind: Int): DecodeError =
    if closingKind == TokenKind.RParen then
      DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan())
    else DecodeError.Custom(s"Expected ']' but found ${describeCurrent()}").atToken(currentSpan())

  protected final def decodeTupleOf(
      schema: RawSchema.TupleOf[?, ?]
  ): Result[Unit, DecodeError] =
    decodeTupleOf(schema, allowTopLevelArrow = true)

  protected final def decodeTupleOf(
      schema: RawSchema.TupleOf[?, ?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeTupleOfWithRead(schema, read, allowTopLevelArrow))

  private def decodeTupleOfWithRead[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    Result.task:
      if allowTopLevelArrow && collectionLiteralsEnabled && !currentStartsTupleLike then
        decodeArrowTupleOf(schema, read).check
      else decodeTupleOfLike(schema, read).check

  private def decodeTupleOfLike[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A]
  ): Result[Unit, DecodeError] =
    Result.task:
      val state0 = read.init()
      currentKind() match
        case TokenKind.EmptyTupleId =>
          advance()
          pushRef(read.finish(state0))
        case TokenKind.TupleId =>
          decodeTupleOfSingleton(schema, read, state0).check
        case TokenKind.LParen =>
          decodeTupleOfParen(schema, read, state0).check
        case _ =>
          raise(
            DecodeError
              .ExpectedType(schema.describeSelf, describeCurrent())
              .atToken(currentSpan())
          )

  private def decodeTupleOfSingleton[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A],
      state0: Repr
  ): Result[Unit, DecodeError] =
    Result.task:
      advance()
      if currentKind() == TokenKind.LParen then advance()
      else raise(expectedTupleApplyError())
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.FieldCountMismatch(1, 0).atToken(currentSpan()))

      val slotError = decodeTupleOfElement(schema, 0)
      if slotError != null then raise(slotError)

      currentKind() match
        case TokenKind.RParen =>
          advance()
          val state = addSlot(read)(state0)
          pushRef(read.finish(state))
        case TokenKind.Comma =>
          raise(DecodeError.FieldCountMismatch(1, 2).atToken(currentSpan()))
        case _ =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def decodeTupleOfParen[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A],
      state0: Repr
  ): Result[Unit, DecodeError] =
    Result.task:
      val openOffset = currentOffset()
      advance()
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      val slotError = decodeTupleOfElement(schema, 0)
      if slotError != null then raise(slotError)

      currentKind() match
        case TokenKind.Comma =>
          val state = addSlot(read)(state0)
          decodeTupleOfCommaTail(schema, read, state).check
        case TokenKind.RParen =>
          raise(
            DecodeError
              .ExpectedType(schema.describeSelf, "(...)")
              .atToken(spanAt(openOffset))
          )
        case _ =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def decodeTupleOfCommaTail[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A],
      state0: Repr
  ): Result[Unit, DecodeError] =
    Result.task:
      var state                         = state0
      var count                         = 1
      var closingSpan: DecodeError.Span = currentSpan()
      var done                          = false
      while !done do
        currentKind() match
          case TokenKind.Comma =>
            advance()
            currentKind() match
              case TokenKind.RParen =>
                closingSpan = currentSpan()
                done = true
              case _ =>
                val slotError = decodeTupleOfElement(schema, count)
                if slotError != null then raise(slotError)
                state = addSlot(read)(state)
                count += 1
          case TokenKind.RParen =>
            closingSpan = currentSpan()
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
      advance()
      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(closingSpan))
      pushRef(read.finish(state))

  private def decodeTupleOfElement(
      schema: RawSchema.TupleOf[?, ?],
      index: Int
  ): DecodeError | Null =
    decodeBase(schema.element) match
      case Result.Err(error) => error.atPath(s"[$index]")
      case _                 => null

  private def decodeArrowTupleOf[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A]
  ): Result[Unit, DecodeError] =
    Result.task:
      var state = read.init()
      checkOrRaise(decodeBase(schema.element, allowTopLevelArrow = false))(_.atPath("[0]"))
      state = addSlot(read)(state)
      if currentKind() == TokenKind.Arrow then advance()
      else raise(expectedArrowError())
      checkOrRaise(decodeBase(schema.element, allowTopLevelArrow = false))(_.atPath("[1]"))
      state = addSlot(read)(state)
      pushRef(read.finish(state))

  protected final def decodePairSeq(schema: RawSchema.PairSeq[?, ?, ?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodePairSeqWithRead(schema, read))

  private def decodePairSeqWithRead[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A]
  ): Result[Unit, DecodeError] =
    Result.task:
      var state           = read.init()
      val allowArrowPairs = collectionLiteralsEnabled
      val closingKind     = currentVectorClosingKind()
      if closingKind < 0 then raise(expectedVectorStartError(schema))

      advanceVectorStart(closingKind)

      if currentKind() == closingKind then advance()
      else
        var index = 0
        var done  = false
        while !done do
          decodePairSeqElement(schema, read, state, index, allowArrowPairs) match
            case err: Result.Err[?] => breakErr(err.asInstanceOf[Result.Err[DecodeError]])
            case nextState          => state = nextState.asInstanceOf[Repr]
          index += 1

          vectorElementSeparator(closingKind) match
            case 0 => ()
            case 1 => done = true
            case _ => raise(expectedVectorClosingError(closingKind))

        if currentKind() == closingKind then advance()
      pushRef(read.finish(state))

  private def decodePairSeqElement[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      state: Repr,
      index: Int,
      allowArrowPairs: Boolean
  ): StateDecodeResult[Repr] =
    if currentKind() == TokenKind.LParen then decodeTuplePairSeqElement(schema, read, state, index)
    else if allowArrowPairs then decodeArrowPairSeqElement(schema, read, state, index)
    else Result.Err(expectedPairSeqElementError())

  private def decodeTuplePairSeqElement[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      state0: Repr,
      index: Int
  ): StateDecodeResult[Repr] =
    val tupleOffset = currentOffset()
    advance()

    if currentKind() == TokenKind.RParen then
      return Result.Err(DecodeError.FieldCountMismatch(2, 0).atToken(currentSpan()))

    decodeBase(schema.key) match
      case Result.Err(error) => return Result.Err(error.atPath(s"[$index][0]"))
      case _                 =>

    var state          = addPairKeySlot(read)(state0)
    val separatorError = consumePairSeqTupleSeparator(tupleOffset)
    if separatorError != null then return Result.Err(separatorError)

    if currentKind() == TokenKind.RParen then
      return Result.Err(DecodeError.FieldCountMismatch(2, 1).atToken(currentSpan()))

    decodeBase(schema.value) match
      case Result.Err(error) => return Result.Err(error.atPath(s"[$index][1]"))
      case _                 =>

    state = addPairValueSlot(read)(state)
    val endError = consumePairSeqTupleEnd()
    if endError != null then Result.Err(endError)
    else state

  private def consumePairSeqTupleSeparator(tupleOffset: Int): DecodeError | Null =
    currentKind() match
      case TokenKind.Comma =>
        advance()
        null
      case TokenKind.RParen =>
        DecodeError.FieldCountMismatch(2, 1).atToken(spanAt(tupleOffset))
      case _ =>
        DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan())

  private def consumePairSeqTupleEnd(): DecodeError | Null =
    currentKind() match
      case TokenKind.RParen =>
        advance()
        null
      case TokenKind.Comma =>
        advance()
        currentKind() match
          case TokenKind.RParen =>
            advance()
            null
          case _ =>
            DecodeError.FieldCountMismatch(2, 3).atToken(currentSpan())
      case _ =>
        DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan())

  private def decodeArrowPairSeqElement[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      state: Repr,
      index: Int
  ): StateDecodeResult[Repr] =
    if pairSeqKeyUsesRouterArrowChain(schema.key, schema.value) then
      decodeRouterKeyArrowPair(schema, read, state, index)
    else decodeSimpleArrowPairSeqElement(schema, read, state, index)

  private def decodeSimpleArrowPairSeqElement[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      state0: Repr,
      index: Int
  ): StateDecodeResult[Repr] =
    val allowKeyArrow = !schemaIsRouter(schema.key)
    decodeBase(schema.key, allowTopLevelArrow = allowKeyArrow) match
      case Result.Err(error) => return Result.Err(error.atPath(s"[$index][0]"))
      case _                 =>

    var state = addPairKeySlot(read)(state0)
    if currentKind() == TokenKind.Arrow then advance()
    else return Result.Err(expectedArrowError())

    decodeBase(schema.value) match
      case Result.Err(error) => Result.Err(error.atPath(s"[$index][1]"))
      case _                 =>
        state = addPairValueSlot(read)(state)
        state

  private def expectedPairSeqElementError(): DecodeError =
    DecodeError
      .ExpectedType(RawSchema.describeTupleSlots(2), describeCurrent())
      .atToken(currentSpan())

  private def decodeRouterKeyArrowPair[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      state0: Repr,
      index: Int
  ): StateDecodeResult[Repr] =
    val keyError = decodeRouterKeyArrowPairKey(schema, index)
    if keyError != null then return Result.Err(keyError)

    var left  = pullAny()
    var state = state0
    var done  = false
    while !done do
      val arrowOffset = currentOffset()
      advance()
      val valueError = decodeRouterKeyArrowPairValue(schema, index)
      if valueError != null then return Result.Err(valueError)

      val right = pullAny()
      if currentKind() == TokenKind.Arrow then
        val tupleError = finishRouterKeyArrowPairKey(schema, left, right, arrowOffset, index)
        if tupleError != null then return Result.Err(tupleError)
        left = pullAny()
      else
        pushRef(left)
        state = addPairKeySlot(read)(state)
        pushRef(right)
        state = addPairValueSlot(read)(state)
        done = true
    state

  private def decodeRouterKeyArrowPairKey(
      schema: RawSchema.PairSeq[?, ?, ?],
      index: Int
  ): DecodeError | Null =
    decodeBase(schema.key, allowTopLevelArrow = false) match
      case Result.Err(error) => error.atPath(s"[$index][0]")
      case _                 =>
        if currentKind() == TokenKind.Arrow then null
        else expectedArrowError()

  private def decodeRouterKeyArrowPairValue(
      schema: RawSchema.PairSeq[?, ?, ?],
      index: Int
  ): DecodeError | Null =
    decodeBase(schema.value, allowTopLevelArrow = false) match
      case Result.Err(error) => error.atPath(s"[$index][1]")
      case _                 => null

  private def finishRouterKeyArrowPairKey(
      schema: RawSchema.PairSeq[?, ?, ?],
      left: Any,
      right: Any,
      arrowOffset: Int,
      index: Int
  ): DecodeError | Null =
    finishRouterTupleCase(schema.key, left, right, false, arrowOffset) match
      case Result.Ok(())     => null
      case Result.Err(error) => error.atPath(s"[$index][0]")

  private def finishRouterTupleCase(
      schema: RawSchema[?],
      firstValue: Any,
      secondValue: Any,
      decodeSecondFromInput: Boolean,
      arrowOffset: Int
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        finishMappedRouterTupleCase(
          mapped,
          firstValue,
          secondValue,
          decodeSecondFromInput,
          arrowOffset
        )
      case ref: RawSchema.Ref[?] =>
        finishRouterTupleCase(
          ref.target(),
          firstValue,
          secondValue,
          decodeSecondFromInput,
          arrowOffset
        )
      case router: RawSchema.Router[?] =>
        val tupleCase = RawSchema.routerCase(router, router.router.tupleIndex)
        if tupleCase == null then Result.Err(missingRouterTupleCaseError(router, arrowOffset))
        else
          finishRouterTupleCase(
            tupleCase.schema,
            firstValue,
            secondValue,
            decodeSecondFromInput,
            arrowOffset
          )
      case tupleOf: RawSchema.TupleOf[?, ?] =>
        finishRouterTupleOf(tupleOf, firstValue, secondValue, decodeSecondFromInput)
      case tuple: RawSchema.Tuple[?] =>
        finishRouterFixedTuple(tuple, firstValue, secondValue, decodeSecondFromInput, arrowOffset)
      case other =>
        Result.Err(expectedRouterTupleSchemaError(other, arrowOffset))

  private def finishMappedRouterTupleCase(
      schema: RawSchema.Mapped[?, ?],
      firstValue: Any,
      secondValue: Any,
      decodeSecondFromInput: Boolean,
      arrowOffset: Int
  ): Result[Unit, DecodeError] =
    Result.task {
      finishRouterTupleCase(
        schema.base,
        firstValue,
        secondValue,
        decodeSecondFromInput,
        arrowOffset
      ).check
      mapSlot(schema.mapping).check
    }

  private def missingRouterTupleCaseError(
      schema: RawSchema[?],
      arrowOffset: Int
  ): DecodeError =
    DecodeError
      .ExpectedType(schema.describeSelf, "'->'")
      .atToken(spanAt(arrowOffset))

  private def expectedRouterTupleSchemaError(
      schema: RawSchema[?],
      arrowOffset: Int
  ): DecodeError =
    DecodeError
      .ExpectedType(RawSchema.describeTupleSlots(2), schema.describeSelf)
      .atToken(spanAt(arrowOffset))

  private def finishRouterTupleOf[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      firstValue: Any,
      secondValue: Any,
      decodeSecondFromInput: Boolean
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      Result.task:
        var state = read.init()
        pushRef(firstValue)
        state = addSlot(read)(state)
        if decodeSecondFromInput then
          checkOrRaise(decodeBase(schema.element, allowTopLevelArrow = false))(_.atPath("[1]"))
        else pushRef(secondValue)
        state = addSlot(read)(state)
        pushRef(read.finish(state))
    }

  private def finishRouterFixedTuple[Repr, A](
      schema: RawSchema.Tuple[?],
      firstValue: Any,
      secondValue: Any,
      decodeSecondFromInput: Boolean,
      arrowOffset: Int
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { pooled =>
        Result.task:
          val slots = schema.slots
          if slots.length != 2 then
            raise(DecodeError.FieldCountMismatch(slots.length, 2).atToken(spanAt(arrowOffset)))
          var state = read.initPooled(slots.length, pooled)
          pushRef(firstValue)
          state = addSlot(read)(state, 0)
          if decodeSecondFromInput then
            checkOrRaise(decodeBase(slots(1), allowTopLevelArrow = false))(_.atPath("[1]"))
          else pushRef(secondValue)
          state = addSlot(read)(state, 1)
          pushRef(read.finish(state))
      }
    }

  private def pairSeqKeyUsesRouterArrowChain(
      keySchema: RawSchema[?],
      valueSchema: RawSchema[?]
  ): Boolean =
    val key   = resolveRef(keySchema)
    val value = resolveRef(valueSchema)
    (key.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) &&
    key.isInstanceOf[RawSchema.Router[?]]

  private def schemaIsRouter(schema: RawSchema[?]): Boolean =
    resolveRef(schema).isInstanceOf[RawSchema.Router[?]]

  private def resolveRef(schema: RawSchema[?]): RawSchema[?] =
    schema match
      case RawSchema.Ref(_, target) =>
        resolveRef(target())
      case _ =>
        schema

  protected final def decodeDict(schema: RawSchema.Dict[?, ?]): Result[Unit, DecodeError] =
    namesPool.withBorrowed { seenNames =>
      withRead(schema, _.read)(read => decodeDictWithRead(schema, read, seenNames))
    }

  private def decodeDictWithRead[Elem, Repr, A, SeenNames](
      schema: RawSchema.Dict[?, ?],
      read: Reader.DictBuilder[Elem, Repr, A],
      seenNames: SeenNames
  )(using PublicInternal.NameSet[SeenNames]): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )

      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      var state = read.init()
      var done  = false
      while !done do
        val nameOffset = currentOffset()
        parseNamedFieldStart().check
        val name       = pullStringStrict()
        val fieldError = decodeDictFieldValue(schema, seenNames, name, nameOffset)
        if fieldError != null then raise(fieldError)
        state = addSlot(read)(state, name)

        currentKind() match
          case TokenKind.Comma =>
            advance()
            if currentKind() == TokenKind.RParen then done = true
          case TokenKind.RParen =>
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      advance()
      pushRef(read.finish(state))

  private def decodeDictFieldValue[SeenNames](
      schema: RawSchema.Dict[?, ?],
      seenNames: SeenNames,
      name: String,
      nameOffset: Int
  )(using PublicInternal.NameSet[SeenNames]): DecodeError | Null =
    if seenNames.alreadySeen(name) then
      DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset))
    else
      decodeBase(schema.element) match
        case Result.Err(error) => error.atPath(s".${name}")
        case _                 => null

  protected final def decodeOption(
      schema: RawSchema.Option[?]
  ): Result[Unit, DecodeError] =
    decodeOption(schema, allowTopLevelArrow = true)

  protected final def decodeOption(
      schema: RawSchema.Option[?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    Result.task {
      if currentKind() == TokenKind.NullKw then
        advance()
        pushRef(None)
      else
        decodeBase(schema.inner, allowTopLevelArrow).check
        pushRef(Some(pullAny()))
    }

    /** decodes a string (with `+` concatenation), pushing the value into [[stringSlot]] */
  protected final def decodeString(): Result[Unit, DecodeError] =
    Result.task {
      if tryScanStringDirect() then pushString(scannedStringValue())
      else if currentKind() == TokenKind.StringLit then
        pushString(currentStringValue())
        advance()
      else raise(expectedTypeAtCurrent(RawSchema.String))
      // '+' concatenation is probed at the char level so the common single-literal case leaves the
      // stream between tokens instead of scanning whatever follows the string
      if (if betweenTokens then probePlusChar() else currentKind() == TokenKind.Plus) then
        val builder = StringBuilder() ++= pullStringStrict()
        while currentKind() == TokenKind.Plus do
          advance()
          decodeStringAtom().check
          builder ++= pullStringStrict()
        pushString(builder.result())
    }

  protected final def decodeStringAtom(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.StringLit then
      pushString(currentStringValue())
      advance()
    else raise(expectedTypeAtCurrent(RawSchema.String))

  protected final def decodeChar(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.CharLit then
      pushChar(currentCharValue())
      advance()
    else raise(expectedTypeAtCurrent(RawSchema.Char))

  protected final def decodeInt(): Result[Unit, DecodeError] = Result.task:
    val signedScan = tryScanSignedNumberDirect()
    if signedScan != Tokenizer.NumberNone then
      val negative = signedScan == Tokenizer.NumberNegated
      if scannedKind() == TokenKind.IntLit then pushInt(currentIntValue(negative))
      else
        adoptScannedToken()
        raise(expectedTypeAtCurrent(RawSchema.Int))
    else
      decodeSigned[Int](
        literal = negative =>
          currentKind() match
            case TokenKind.IntLit => currentIntValue(negative)
            case _                => raise(expectedTypeAtCurrent(RawSchema.Int)),
        store = v => pushInt(v)
      )

  protected final def decodeLong(): Result[Unit, DecodeError] = Result.task:
    val signedScan = tryScanSignedNumberDirect()
    if signedScan != Tokenizer.NumberNone then
      val negative = signedScan == Tokenizer.NumberNegated
      scannedKind() match
        case TokenKind.LongLit => pushLong(currentLongValue(negative))
        case TokenKind.IntLit  => pushLong(currentIntValue(negative).toLong)
        case _                 =>
          adoptScannedToken()
          raise(expectedTypeAtCurrent(RawSchema.Long))
    else
      decodeSigned[Long](
        literal = negative =>
          currentKind() match
            case TokenKind.LongLit => currentLongValue(negative)
            case TokenKind.IntLit  => currentIntValue(negative).toLong
            case _                 => raise(expectedTypeAtCurrent(RawSchema.Long)),
        store = v => pushLong(v)
      )

  protected final def decodeFloat(): Result[Unit, DecodeError] = Result.task:
    val signedScan = tryScanSignedNumberDirect()
    if signedScan != Tokenizer.NumberNone then
      val negative = signedScan == Tokenizer.NumberNegated
      scannedKind() match
        case TokenKind.FloatLit =>
          val magnitude = currentFloatValue()
          pushFloat(if negative then -magnitude else magnitude)
        case TokenKind.IntLit =>
          val value = currentIntValue(negative)
          if NumericPromotions.isExactFloat(value) then pushFloat(value.toFloat)
          else
            adoptScannedToken()
            raise(expectedTypeAtCurrent(RawSchema.Float))
        case _ =>
          adoptScannedToken()
          raise(expectedTypeAtCurrent(RawSchema.Float))
    else
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

  protected final def decodeDouble(): Result[Unit, DecodeError] = Result.task:
    val signedScan = tryScanSignedNumberDirect()
    if signedScan != Tokenizer.NumberNone then
      val negative = signedScan == Tokenizer.NumberNegated
      scannedKind() match
        case TokenKind.DoubleLit =>
          val magnitude = currentDoubleValue()
          pushDouble(if negative then -magnitude else magnitude)
        case TokenKind.IntLit => pushDouble(currentIntValue(negative).toDouble)
        case _                =>
          adoptScannedToken()
          raise(expectedTypeAtCurrent(RawSchema.Double))
    else
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

  protected final def decodeBoolean(): Result[Unit, DecodeError] =
    Result.task:
      tryReadBooleanChar() match
        case Tokenizer.BooleanTrue  => pushBoolean(true)
        case Tokenizer.BooleanFalse => pushBoolean(false)
        case _                      =>
          currentKind() match
            case TokenKind.TrueKw =>
              advance()
              pushBoolean(true)
            case TokenKind.FalseKw =>
              advance()
              pushBoolean(false)
            case _ =>
              raise(expectedTypeAtCurrent(RawSchema.Boolean))

  protected final def decodeNull(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.NullKw then
      advance()
      pushRef(null)
    else raise(expectedTypeAtCurrent(RawSchema.Null))
