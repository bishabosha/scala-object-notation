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

  protected final def decodeBase(schema: RawSchema[?]): Result[Unit, DecodeError] =
    decodeBase(schema, allowTopLevelArrow = true)

  private def decodeBase(
      schema: RawSchema[?],
      allowTopLevelArrow: Boolean
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        Result.task {
          decodeBase(mapped.base, allowTopLevelArrow).check
          mapSlot(mapped.mapping).check
        }
      case RawSchema.Ref(_, target) =>
        decodeBase(target(), allowTopLevelArrow)
      case router: RawSchema.Router[?] =>
        decodeRouter(router, allowTopLevelArrow)
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
      case sc: RawSchema.Vector[?, ?] =>
        decodeVector(sc)
      case sc: RawSchema.TupleOf[?, ?] =>
        decodeTupleOf(sc, allowTopLevelArrow)
      case sc: RawSchema.PairSeq[?, ?, ?] =>
        decodePairSeq(sc)
      case sc: RawSchema.Dict[?, ?] =>
        decodeDict(sc)
      case sc: RawSchema.Option[?] =>
        decodeOption(sc, allowTopLevelArrow)
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
      case _ => RouterConstruct.RawNumber

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
        if tupleCase == null then
          raise(
            DecodeError
              .ExpectedType(schema.describeSelf, "'->'")
              .atToken(spanAt(arrowOffset))
          )
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
        else
          val slots         = schema.slots
          val state0        = read.initPooled(slots.length, pooled)
          val expectedSlots = slots.length
          val state1        = parseTupleLike(
            schema,
            state0,
            expectedSlots
          )(
            index => decodeTupleSlotValue(slots, index),
            (state, index) => addSlot(read)(state, index)
          )
          pushRef(read.finish(state1))
      }
    }

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
        }
      }
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
      }
      if parsed.fieldCount != 1 then
        var err = DecodeError.FieldCountMismatch(1, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(spanAt(parsed.closingOffset)))
      // the decoded case value remains in the live slot
    }

  protected final def decodeDiscriminatorSum(
      schema: RawSchema.DiscriminatorSum[?]
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
      val sumCase  = RawSchema.findCase(schema, caseName) match
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
                    raise(
                      makeDuplicateKnownFieldError(fields(decodedIndex).name, nameOffset)
                    )
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
        }
      }
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
    Result.task {
      var values = read.init()
      parseVectorStructure(schema) { indexInVector =>
        checkOrRaise(decodeBase(schema.element))(_.atPath(s"[$indexInVector]"))
        values = addSlot(read)(values)
      }
      pushRef(read.finish(values))
    }

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
      else
        val state0 = read.init()
        val state1 = parseTupleLike(
          schema,
          state0,
          expectedSlots = VariableTupleSlots
        )(
          _ => decodeBase(schema.element),
          (state, _) => addSlot(read)(state)
        )
        pushRef(read.finish(state1))

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
      parseVectorStructure(schema) { index =>
        if currentKind() == TokenKind.LParen then
          val tupleOffset = currentOffset()
          advance()

          if currentKind() == TokenKind.RParen then
            raise(DecodeError.FieldCountMismatch(2, 0).atToken(currentSpan()))

          checkOrRaise(decodeBase(schema.key))(_.atPath(s"[$index][0]"))
          state = addPairKeySlot(read)(state)

          currentKind() match
            case TokenKind.Comma =>
              advance()
            case TokenKind.RParen =>
              raise(DecodeError.FieldCountMismatch(2, 1).atToken(spanAt(tupleOffset)))
            case _ =>
              raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

          if currentKind() == TokenKind.RParen then
            raise(DecodeError.FieldCountMismatch(2, 1).atToken(currentSpan()))

          checkOrRaise(decodeBase(schema.value))(_.atPath(s"[$index][1]"))
          state = addPairValueSlot(read)(state)

          currentKind() match
            case TokenKind.RParen =>
              advance()
            case TokenKind.Comma =>
              advance()
              currentKind() match
                case TokenKind.RParen =>
                  advance()
                case _ =>
                  raise(DecodeError.FieldCountMismatch(2, 3).atToken(currentSpan()))
            case _ =>
              raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
        else if allowArrowPairs then
          if pairSeqKeyUsesRouterArrowChain(schema.key, schema.value) then
            decodeRouterKeyArrowPair(schema, read, state, index) match
              case Result.Ok(nextState) => state = nextState
              case Result.Err(error)    => raise(error)
          else
            val allowKeyArrow = !schemaIsRouter(schema.key)
            checkOrRaise(decodeBase(schema.key, allowTopLevelArrow = allowKeyArrow))(
              _.atPath(s"[$index][0]")
            )
            state = addPairKeySlot(read)(state)
            if currentKind() == TokenKind.Arrow then advance()
            else raise(expectedArrowError())
            checkOrRaise(decodeBase(schema.value))(_.atPath(s"[$index][1]"))
            state = addPairValueSlot(read)(state)
        else
          raise(
            DecodeError
              .ExpectedType(RawSchema.describeTupleSlots(2), describeCurrent())
              .atToken(currentSpan())
          )
      }
      pushRef(read.finish(state))

  private def decodeRouterKeyArrowPair[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A],
      state0: Repr,
      index: Int
  ): Result[Repr, DecodeError] =
    Result:
      checkOrRaise(decodeBase(schema.key, allowTopLevelArrow = false))(_.atPath(s"[$index][0]"))
      var left = pullAny()
      if currentKind() != TokenKind.Arrow then raise(expectedArrowError())

      var state = state0
      var done  = false
      while !done do
        val arrowOffset = currentOffset()
        advance()
        checkOrRaise(decodeBase(schema.value, allowTopLevelArrow = false))(
          _.atPath(s"[$index][1]")
        )
        val right = pullAny()
        if currentKind() == TokenKind.Arrow then
          finishRouterTupleCase(schema.key, left, right, false, arrowOffset) match
            case Result.Ok(())     => left = pullAny()
            case Result.Err(error) => raise(error.atPath(s"[$index][0]"))
        else
          pushRef(left)
          state = addPairKeySlot(read)(state)
          pushRef(right)
          state = addPairValueSlot(read)(state)
          done = true
      state

  private def finishRouterTupleCase(
      schema: RawSchema[?],
      firstValue: Any,
      secondValue: Any,
      decodeSecondFromInput: Boolean,
      arrowOffset: Int
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        Result.task {
          finishRouterTupleCase(
            mapped.base,
            firstValue,
            secondValue,
            decodeSecondFromInput,
            arrowOffset
          ).check
          mapSlot(mapped.mapping).check
        }
      case RawSchema.Ref(_, target) =>
        finishRouterTupleCase(
          target(),
          firstValue,
          secondValue,
          decodeSecondFromInput,
          arrowOffset
        )
      case router: RawSchema.Router[?] =>
        val tupleCase = RawSchema.routerCase(router, router.router.tupleIndex)
        if tupleCase == null then
          Result.Err(
            DecodeError
              .ExpectedType(router.describeSelf, "'->'")
              .atToken(spanAt(arrowOffset))
          )
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
        Result.Err(
          DecodeError
            .ExpectedType(RawSchema.describeTupleSlots(2), other.describeSelf)
            .atToken(spanAt(arrowOffset))
        )

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
    Result.task {
      var state  = read.init()
      val parsed = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameOffset, _) =>
        if seenNames.alreadySeen(name) then
          raise(
            DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset))
          )
        checkOrRaise(decodeBase(schema.element))(_.atPath(s".${name}"))
        state = addSlot(read)(state, name)
      }
      val _ = parsed.closingOffset
      val _ = parsed.fieldName
      val _ = parsed.fieldCount
      pushRef(read.finish(state))
    }

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
