package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.RouterSchema
import scalanotation.schema.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise
import scalanotation.schema.RawSchema

private[scalanotation] trait SchemaDecoders extends BaseDecoders:
  self: TokenStream =>

  protected final def decodeBase(schema: RawSchema[?]): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        Result.task {
          decodeBase(mapped.base).check
          mapSlot(mapped.mapping).check
        }
      case RawSchema.Ref(_, target) =>
        decodeBase(target())
      case router: RawSchema.Router[?] =>
        decodeRouter(router)
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
      case sc: RawSchema.Vector[?, ?] =>
        decodeVector(sc)
      case sc: RawSchema.TupleOf[?, ?] =>
        decodeTupleOf(sc)
      case sc: RawSchema.PairSeq[?, ?, ?] =>
        decodePairSeq(sc)
      case sc: RawSchema.Dict[?, ?] =>
        decodeDict(sc)
      case sc: RawSchema.Option[?] =>
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

  protected final def decodeRouter(schema: RawSchema.Router[?]): Result[Unit, DecodeError] =
    Result.task:
      val construct = currentRouterConstruct(schema.numberMode)
      if construct == null then
        raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      else decodeRouterCase(schema, construct).check

  private def currentRouterConstruct(
      numberMode: RouterSchema.NumberMode
  ): RouterSchema.RouterConstruct | Null =
    import RouterSchema.RouterConstruct

    currentKind() match
      case TokenKind.LParen =>
        if parenStartsRecord() then RouterConstruct.Record
        else RouterConstruct.Tuple
      case TokenKind.EmptyTupleId => RouterConstruct.Tuple
      case TokenKind.TupleId      => RouterConstruct.Tuple
      case TokenKind.VectorId     => RouterConstruct.Vector
      case TokenKind.StringLit    => RouterConstruct.String
      case TokenKind.CharLit      => RouterConstruct.Char
      case TokenKind.IntLit       =>
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
      construct: RouterSchema.RouterConstruct
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

  private def numberConstruct(
      bounded: RouterSchema.RouterConstruct,
      numberMode: RouterSchema.NumberMode
  ): RouterSchema.RouterConstruct =
    numberMode match
      case RouterSchema.NumberMode.Bounded => bounded
      case RouterSchema.NumberMode.Raw     => RouterSchema.RouterConstruct.RawNumber

  protected final def decodeTuple(schema: RawSchema.Tuple[?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeTupleWithRead(schema, read))

  private def decodeTupleWithRead[Repr, A](
      schema: RawSchema.Tuple[?],
      read: Reader.TupleBuilder[Repr, A]
  ): Result[Unit, DecodeError] =
    withBorrowSlots(read.slotsFactory) { pooled =>
      Result.task {
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
        val fields          = schema.fields
        val jumboSeenFields =
          if fields.length > SeenFieldMaskLimit then fieldSetPool.borrow().reset(fields.length)
          else null
        try
          Result.task {
            schema.isValidNamedTuple(namesPool).check

            var state: read.State            = read.init(fields.length, slots)
            var fieldIndex                   = 0
            var seenFieldMask                = 0L
            var lastFieldName: String | Null = null

            def actualNameAlreadySeen(): Boolean =
              val actualIndex = currentFieldIndexOf(fields)
              actualIndex >= 0 && fieldIndexAlreadySeen(
                jumboSeenFields,
                seenFieldMask,
                actualIndex
              )

            def markFieldSeen(index: Int): Unit =
              val seenFields = jumboSeenFields
              if seenFields != null then seenFields.markSeen(index)
              else seenFieldMask |= 1L << index

            val parsed =
              parseNamedTupleStructureByCurrentName(schema, allowEmpty = fields.isEmpty) {
                (nameOffset, parsedFieldIndex) =>
                  if !isFieldNameStart(currentKind()) then
                    raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))
                  def actualFieldErr(err: String => DecodeError): DecodeError =
                    val actualName = currentFieldName()
                    err(actualName).atPath(s".${actualName}").atToken(spanAt(nameOffset))
                  val validated: DecodeError | Field = {
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
                        if actualNameAlreadySeen() then
                          actualFieldErr(DecodeError.DuplicateField(_))
                        else if expectedBeforeSkip == null then
                          actualFieldErr(_ =>
                            DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                          )
                        else
                          actualFieldErr(actualName =>
                            DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
                          )
                      else
                        val expectedField = fields(fieldIndex)
                        if !currentFieldNameMatches(expectedField.name) then
                          if actualNameAlreadySeen() then
                            actualFieldErr(DecodeError.DuplicateField(_))
                          else
                            actualFieldErr(actualName =>
                              DecodeError.FieldOrderMismatch(expectedField.name, actualName)
                            )
                        else expectedField
                    else if parsedFieldIndex >= fields.length then
                      if actualNameAlreadySeen() then actualFieldErr(DecodeError.DuplicateField(_))
                      else
                        actualFieldErr(_ =>
                          DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                        )
                    else
                      val expectedField = fields(parsedFieldIndex)
                      if !currentFieldNameMatches(expectedField.name) then
                        if actualNameAlreadySeen() then
                          actualFieldErr(DecodeError.DuplicateField(_))
                        else
                          actualFieldErr(actualName =>
                            DecodeError.FieldOrderMismatch(expectedField.name, actualName)
                          )
                      else expectedField
                  }
                  validated match
                    case expectedField: Field =>
                      val decodedIndex =
                        if schema.allowSkippedNullableFields then fieldIndex else parsedFieldIndex
                      parseNamedFieldStartNoPush().check
                      checkOrRaise(decodeBase(expectedField.schema))(
                        _.atPath(s".${expectedField.name}").atToken(spanAt(nameOffset))
                      )
                      state = addSlot(read)(state, decodedIndex)
                      markFieldSeen(decodedIndex)
                      fieldIndex = decodedIndex + 1
                      lastFieldName = expectedField.name
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
                if lastFieldName != null then err0 = err0.atPath(s".${lastFieldName}")
                err0.atToken(spanAt(parsed.closingOffset))
              raise(err)

            pushRef(read.finish(state))
          }
        finally if jumboSeenFields != null then fieldSetPool.release(jumboSeenFields)
      }
    }

  private inline val SeenFieldMaskLimit = java.lang.Long.SIZE

  private def currentFieldIndexOf(fields: IArray[Field]): Int =
    var index = 0
    while index < fields.length do
      if currentFieldNameMatches(fields(index).name) then return index
      index += 1
    -1

  private def fieldIndexAlreadySeen(
      jumboSeenFields: Internal.JumboFieldSet | Null,
      seenFieldMask: Long,
      index: Int
  ): Boolean =
    if jumboSeenFields != null then jumboSeenFields.isSeen(index)
    else (seenFieldMask & (1L << index)) != 0L

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
        val fields          = schema.fields
        val jumboSeenFields =
          if fields.length > SeenFieldMaskLimit then fieldSetPool.borrow().reset(fields.length)
          else null
        try
          Result.task {
            schema.isValidNamedTuple(namesPool).check
            var state: read.State            = read.init(fields.length, slots)
            var fieldIndex                   = 0
            var seenFieldMask                = 0L
            var lastFieldName: String | Null = null

            def actualNameAlreadySeen(): Boolean =
              val actualIndex = currentFieldIndexOf(fields)
              actualIndex >= 0 && fieldIndexAlreadySeen(
                jumboSeenFields,
                seenFieldMask,
                actualIndex
              )

            def markFieldSeen(index: Int): Unit =
              val seenFields = jumboSeenFields
              if seenFields != null then seenFields.markSeen(index)
              else seenFieldMask |= 1L << index

            val parsed = parsePartialNamedTupleStructureByCurrentName(schema) {
              (nameOffset, parsedFieldIndex) =>
                if !isFieldNameStart(currentKind()) then
                  raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))
                def actualFieldErr(err: String => DecodeError): DecodeError =
                  val actualName = currentFieldName()
                  err(actualName).atPath(s".${actualName}").atToken(spanAt(nameOffset))
                val validated: DecodeError | Field = {
                  if currentFieldNameMatches(alreadySeenField) then
                    actualFieldErr(DecodeError.DuplicateField(_))
                  else if schema.allowSkippedNullableFields then
                    val expectedBeforeSkip =
                      if fieldIndex < fields.length then fields(fieldIndex) else null
                    while fieldIndex < fields.length
                      && !currentFieldNameMatches(fields(fieldIndex).name)
                      && TokenDecoder.isNullable(fields(fieldIndex).schema)
                    do
                      state = read.add(state, fieldIndex, None)
                      fieldIndex += 1
                    if fieldIndex >= fields.length then
                      if actualNameAlreadySeen() then actualFieldErr(DecodeError.DuplicateField(_))
                      else if expectedBeforeSkip == null then
                        actualFieldErr(_ =>
                          DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                        )
                      else
                        actualFieldErr(actualName =>
                          DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
                        )
                    else
                      val expectedField = fields(fieldIndex)
                      if !currentFieldNameMatches(expectedField.name) then
                        if actualNameAlreadySeen() then
                          actualFieldErr(DecodeError.DuplicateField(_))
                        else
                          actualFieldErr(actualName =>
                            DecodeError.FieldOrderMismatch(expectedField.name, actualName)
                          )
                      else expectedField
                  else if parsedFieldIndex >= fields.length then
                    if actualNameAlreadySeen() then actualFieldErr(DecodeError.DuplicateField(_))
                    else
                      actualFieldErr(_ =>
                        DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                      )
                  else
                    val expectedField = fields(parsedFieldIndex)
                    if !currentFieldNameMatches(expectedField.name) then
                      if actualNameAlreadySeen() then actualFieldErr(DecodeError.DuplicateField(_))
                      else
                        actualFieldErr(actualName =>
                          DecodeError.FieldOrderMismatch(expectedField.name, actualName)
                        )
                    else expectedField
                }
                validated match
                  case expectedField: Field =>
                    val decodedIndex =
                      if schema.allowSkippedNullableFields then fieldIndex else parsedFieldIndex
                    parseNamedFieldStartNoPush().check
                    checkOrRaise(decodeBase(expectedField.schema))(
                      _.atPath(s".${expectedField.name}").atToken(spanAt(nameOffset))
                    )
                    state = addSlot(read)(state, decodedIndex)
                    markFieldSeen(decodedIndex)
                    fieldIndex = decodedIndex + 1
                    lastFieldName = expectedField.name
                  case err: DecodeError => raise(err)
            }

            if schema.allowSkippedNullableFields then
              state = fillSkippedNullableFields(read)(fields, state, fieldIndex, "")
              fieldIndex = pullSkipFillIndex()

            val decodedFieldCount =
              if schema.allowSkippedNullableFields then fieldIndex else parsed.fieldCount
            if decodedFieldCount != fields.length then
              var err = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
              if lastFieldName != null then err = err.atPath(s".${lastFieldName}")
              raise(err.atToken(spanAt(parsed.closingOffset)))

            pushRef(read.finish(state))
          }
        finally if jumboSeenFields != null then fieldSetPool.release(jumboSeenFields)
      }
    }

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

  protected final def decodeTupleOf(schema: RawSchema.TupleOf[?, ?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodeTupleOfWithRead(schema, read))

  private def decodeTupleOfWithRead[Elem, Repr, A](
      schema: RawSchema.TupleOf[?, ?],
      read: Reader.VectorBuilder[Elem, Repr, A]
  ): Result[Unit, DecodeError] =
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

  protected final def decodePairSeq(schema: RawSchema.PairSeq[?, ?, ?]): Result[Unit, DecodeError] =
    withRead(schema, _.read)(read => decodePairSeqWithRead(schema, read))

  private def decodePairSeqWithRead[Key, Elem, Repr, A](
      schema: RawSchema.PairSeq[?, ?, ?],
      read: Reader.PairSeqBuilder[Key, Elem, Repr, A]
  ): Result[Unit, DecodeError] =
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

  protected final def decodeOption(schema: RawSchema.Option[?]): Result[Unit, DecodeError] =
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
