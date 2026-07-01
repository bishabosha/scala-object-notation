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
        decodePrimitiveBase(schema, allowTopLevelArrow)
      case schema: RawSchema.Collection =>
        decodeCollectionBase(schema, allowTopLevelArrow)
      case _ =>
        decodeCompositeBase(schema, allowTopLevelArrow)

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
        fieldIndexSetPool.withBorrowed { seenFields =>
          decodeNamedTupleWithResources(schema, read, slots, seenFields)
        }
      }
    }

  private def decodeNamedTupleWithResources(
      schema: RawSchema.NamedTuple[?],
      read: RawSchema.NamedTupleRead,
      slots: scalanotation.BuilderSlots | Null,
      seenFields: Internal.FieldIndexSet
  ): Result[Unit, DecodeError] =
    Result.task {
      val fields = schema.fields
      seenFields.reset(fields.length)
      schema.isValidNamedTuple(namesPool).check
      var state: read.State            = read.init(fields.length, slots)
      var fieldIndex                   = 0
      var lastFieldName: String | Null = null

      val parsedResult =
        parseKnownNamedTupleStructure(schema, allowEmpty = fields.isEmpty) {
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
                    duplicateKnownFieldError(fields, seenFields, nameOffset, null).check
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
                      duplicateKnownFieldError(fields, seenFields, nameOffset, null).check
                      raise(
                        currentFieldOrderMismatchError(nameOffset, expectedField.name)
                      )
                else if parsedFieldIndex >= fields.length then
                  duplicateKnownFieldError(fields, seenFields, nameOffset, null).check
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
                    duplicateKnownFieldError(fields, seenFields, nameOffset, null).check
                    raise(currentFieldOrderMismatchError(nameOffset, expectedField.name))
              }

              if seenFields.contains(decodedIndex) then
                raise(makeDuplicateKnownFieldError(fields(decodedIndex).name, nameOffset))
              else
                val expectedField = fields(decodedIndex)
                parseNamedFieldStartNoPush() match
                  case err: Result.Err[DecodeError] => breakErr(err)
                  case _                            =>
                    decodeBase(expectedField.schema) match
                      case Result.Err(error) =>
                        raise(
                          error
                            .atPath(s".${expectedField.name}")
                            .atToken(spanAt(nameOffset))
                        )
                      case _ =>
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

      var values = read.init()
      if currentKind() == closingKind then advance()
      else
        var indexInVector = 0
        var done          = false
        while !done do
          checkOrRaise(decodeBase(schema.element))(_.atPath(s"[$indexInVector]"))
          values = addSlot(read)(values)
          indexInVector += 1

          vectorElementSeparator(closingKind) match
            case 0 => ()
            case 1 => done = true
            case _ => raise(expectedVectorClosingError(closingKind))

        if currentKind() == closingKind then advance()

      pushRef(read.finish(values))

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
      decodeStringAtom().check
      if currentKind() == TokenKind.Plus then
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
    decodeSigned[Int](
      literal = negative =>
        currentKind() match
          case TokenKind.IntLit => currentIntValue(negative)
          case _                => raise(expectedTypeAtCurrent(RawSchema.Int)),
      store = v => pushInt(v)
    )

  protected final def decodeLong(): Result[Unit, DecodeError] = Result.task:
    decodeSigned[Long](
      literal = negative =>
        currentKind() match
          case TokenKind.LongLit => currentLongValue(negative)
          case TokenKind.IntLit  => currentIntValue(negative).toLong
          case _                 => raise(expectedTypeAtCurrent(RawSchema.Long)),
      store = v => pushLong(v)
    )

  protected final def decodeFloat(): Result[Unit, DecodeError] = Result.task:
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
