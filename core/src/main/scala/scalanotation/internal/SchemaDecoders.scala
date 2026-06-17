package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.internal.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

private[scalanotation] trait SchemaDecoders extends BaseDecoders:
  self: TokenStream =>

  protected final def decodeBase(schema: RawSchema): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        Result.task {
          decodeBase(mapped.base).check
          mapSlot(mapped.mapping).check
        }
      case RawSchema.Ref(_, target) =>
        decodeBase(target())
      case router: RawSchema.Router =>
        decodeRouter(router)
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
      case sc: RawSchema.TupleOf =>
        decodeTupleOf(sc)
      case sc: RawSchema.PairSeq =>
        decodePairSeq(sc)
      case sc: RawSchema.Dict =>
        decodeDict(sc)
      case sc: RawSchema.Option =>
        decodeOption(sc)
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

  protected final def decodeRouter(schema: RawSchema.Router): Result[Unit, DecodeError] =
    Result.task:
      val construct = currentRouterConstruct(schema.numberMode)
      if construct == null then
        raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      else decodeRouterCase(schema, construct).check

  private def currentRouterConstruct(
      numberMode: RawSchema.RouterNumberMode
  ): RawSchema.RouterConstruct | Null =
    currentKind() match
      case TokenKind.LParen =>
        if parenStartsRecord() then RawSchema.RouterConstruct.Record
        else RawSchema.RouterConstruct.Tuple
      case TokenKind.EmptyTupleId => RawSchema.RouterConstruct.Tuple
      case TokenKind.TupleId      => RawSchema.RouterConstruct.Tuple
      case TokenKind.VectorId     => RawSchema.RouterConstruct.Vector
      case TokenKind.StringLit    => RawSchema.RouterConstruct.String
      case TokenKind.CharLit      => RawSchema.RouterConstruct.Char
      case TokenKind.IntLit       =>
        numberConstruct(RawSchema.RouterConstruct.Int, numberMode)
      case TokenKind.LongLit =>
        numberConstruct(RawSchema.RouterConstruct.Long, numberMode)
      case TokenKind.FloatLit =>
        numberConstruct(RawSchema.RouterConstruct.Float, numberMode)
      case TokenKind.DoubleLit =>
        numberConstruct(RawSchema.RouterConstruct.Double, numberMode)
      case TokenKind.TrueKw | TokenKind.FalseKw =>
        RawSchema.RouterConstruct.Boolean
      case TokenKind.NullKw =>
        RawSchema.RouterConstruct.Null
      case TokenKind.Minus =>
        peekKind() match
          case TokenKind.IntLit =>
            numberConstruct(RawSchema.RouterConstruct.Int, numberMode)
          case TokenKind.LongLit =>
            numberConstruct(RawSchema.RouterConstruct.Long, numberMode)
          case TokenKind.FloatLit =>
            numberConstruct(RawSchema.RouterConstruct.Float, numberMode)
          case TokenKind.DoubleLit =>
            numberConstruct(RawSchema.RouterConstruct.Double, numberMode)
          case _ =>
            RawSchema.RouterConstruct.RawNumber
      case _ => RawSchema.RouterConstruct.RawNumber

  private def decodeRouterCase(
      schema: RawSchema.Router,
      construct: RawSchema.RouterConstruct
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      Result.task:
        val index = read.route(construct)
        if index < 0 || index >= schema.cases.length then
          raise(
            DecodeError
              .ExpectedType(schema.describeSelf, describeCurrent())
              .atToken(currentSpan())
          )
        decodeBase(schema.cases(index).schema).check
    }

  private def numberConstruct(
      bounded: RawSchema.RouterConstruct,
      numberMode: RawSchema.RouterNumberMode
  ): RawSchema.RouterConstruct =
    numberMode match
      case RawSchema.RouterNumberMode.Bounded => bounded
      case RawSchema.RouterNumberMode.Raw     => RawSchema.RouterConstruct.RawNumber

  protected final def decodeTuple(schema: RawSchema.Tuple): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      withBorrowSlots(read.slotsFactory) { pooled =>
        Result.task {
          val slots              = schema.slots
          val state0: read.State = read.initPooled(slots.length, pooled)
          val expectedSlots      = slots.length
          val state1             = parseTupleLike(
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
    }

  protected final def decodeTupleSlotValue(
      slots: IArray[RawSchema],
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

  protected final def decodeSum(schema: RawSchema.Sum): Result[Unit, DecodeError] =
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

  protected final def decodeDiscriminatorSum(
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

  protected final def decodePartialNamedTuple(
      schema: RawSchema,
      alreadySeenField: String
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        Result.task {
          decodePartialNamedTuple(mapped.base, alreadySeenField).check
          mapSlot(mapped.mapping).check
        }
      case namedTuple: RawSchema.NamedTuple =>
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

  protected final def decodeVector(schema: RawSchema.Vector): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      Result.task {
        var values = read.init()
        parseVectorStructure(schema) { indexInVector =>
          checkOrRaise(decodeBase(schema.element))(_.atPath(s"[$indexInVector]"))
          values = addSlot(read)(values)
        }
        pushRef(read.finish(values))
      }
    }

  protected final def decodeTupleOf(schema: RawSchema.TupleOf): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      Result.task:
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
    }

  protected final def decodePairSeq(schema: RawSchema.PairSeq): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      Result.task:
        var state = read.init()
        parseVectorStructure(schema) { index =>
          val tupleOffset = currentOffset()
          if currentKind() == TokenKind.LParen then advance()
          else
            raise(
              DecodeError
                .ExpectedType(RawSchema.describeTupleSlots(2), describeCurrent())
                .atToken(currentSpan())
            )

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
        }
        pushRef(read.finish(state))
    }

  protected final def decodeDict(schema: RawSchema.Dict): Result[Unit, DecodeError] =
    namesPool.withBorrowed { seenNames =>
      withRead(schema, _.read) { read =>
        Result.task {
          var state  = read.init()
          val parsed = parseNamedTupleStructure(schema, allowEmpty = false) {
            (name, nameOffset, _) =>
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
      }
    }

  protected final def decodeOption(schema: RawSchema.Option): Result[Unit, DecodeError] =
    Result.task {
      if currentKind() == TokenKind.NullKw then
        advance()
        pushRef(None)
      else
        decodeBase(schema.inner).check
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
