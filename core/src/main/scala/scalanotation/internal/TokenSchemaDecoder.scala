package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.internal.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

private[scalanotation] trait TokenSchemaDecoder extends TokenTupleDecoder:
  self: TokenStream =>

  private var routerTupleConsEnabled: Boolean = true

  protected final def decodeBase(schema: RawSchema): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapSlot(mapped.mapping, decodeBase(mapped.base))
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
      case sc: RawSchema.Dict =>
        decodeDict(sc)
      case sc: RawSchema.Option =>
        decodeOption(sc)
      case RawSchema.AnyExpr =>
        decodeBase(RawSchema.ExprRouterSchema)
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

  override protected final def decodeBaseInTupleElement(
      schema: RawSchema
  ): Result[Unit, DecodeError] =
    val old = routerTupleConsEnabled
    routerTupleConsEnabled = false
    try decodeBase(schema)
    finally routerTupleConsEnabled = old

  protected final def decodeRouter(schema: RawSchema.Router): Result[Unit, DecodeError] =
    Result.task:
      if schema.read == null then missingReadCapability(schema)
      if currentKind() == TokenKind.LParen then
        if parenStartsRecord() then
          val index = schema.read.nn.route(RawSchema.RouterConstruct.Record)
          if index < 0 || index >= schema.cases.length then
            if schema eq RawSchema.ExprRouterSchema then
              raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
            else
              raise(
                DecodeError
                  .ExpectedType(schema.describeSelf, describeCurrent())
                  .atToken(currentSpan())
              )
          decodeBase(schema.cases(index).schema).check
        else decodeParenthesizedRouterExpression(schema).check
      else
        val construct = currentRouterConstruct(schema.numberMode)
        if construct == null then
          raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
        else
          val index = schema.read.nn.route(construct)
          if index < 0 || index >= schema.cases.length then
            if schema eq RawSchema.ExprRouterSchema then
              raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
            else
              raise(
                DecodeError
                  .ExpectedType(schema.describeSelf, describeCurrent())
                  .atToken(currentSpan())
              )
          decodeBase(schema.cases(index).schema).check
      decodeRouterTupleConsTail(schema).check

  private def currentRouterConstruct(
      numberMode: RawSchema.RouterNumberMode
  ): RawSchema.RouterConstruct | Null =
    currentKind() match
      case TokenKind.LParen =>
        if parenStartsRecord() then RawSchema.RouterConstruct.Record
        else null
      case TokenKind.EmptyTupleId => RawSchema.RouterConstruct.Tuple
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

  private def decodeParenthesizedRouterExpression(
      schema: RawSchema.Router
  ): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.LParen then advance()
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      def decodeRouterWithCons(): Result[Unit, DecodeError] = {
        val old = routerTupleConsEnabled
        routerTupleConsEnabled = true
        try decodeRouter(schema)
        finally routerTupleConsEnabled = old
      }

      decodeRouterWithCons().check
      currentKind() match
        case TokenKind.Comma =>
          val tupleIndex = schema.read.nn.route(RawSchema.RouterConstruct.Tuple)
          if tupleIndex < 0 || tupleIndex >= schema.cases.length then
            raise(
              DecodeError
                .ExpectedType(schema.describeSelf, describeCurrent())
                .atToken(currentSpan())
            )
          schema.cases(tupleIndex).schema match
            case tupleOf: RawSchema.TupleOf =>
              if tupleOf.read == null then missingReadCapability(tupleOf)
              val read         = tupleOf.read.nn
              val state0       = addSlot(read)(read.init())
              def parseOuter() = Result.task {
                pushRef(
                  parseTupleCommaTailLike(
                    state0,
                    startCount = 1,
                    expectedSlots = VariableTupleSlots
                  )(
                    _ => decodeBase(tupleOf.element),
                    (state, _) => addSlot(read)(state)
                  )
                )
              }
              parseOuter().check
              val state1 = pullAny().asInstanceOf[read.State]
              var state2 = state1
              if routerTupleConsEnabled && currentKind() == TokenKind.StarColon then
                val inner = read.finish(state1)
                pushRef(inner)
                val outer = addSlot(read)(read.init())

                def parseInner() = Result.task {
                  pushRef(
                    parseTupleConsTailLike(
                      outer,
                      startIndex = 1,
                      expectedSlots = VariableTupleSlots
                    )(
                      _ => decodeBaseInTupleElement(tupleOf.element),
                      (state, _) => addSlot(read)(state),
                      peekKind() != TokenKind.StarColon
                    )
                  )
                }

                parseInner().check
                state2 = pullAny().asInstanceOf[read.State]
              pushRef(read.finish(state2))
            case other =>
              raise(
                DecodeError
                  .ExpectedType(other.describeSelf, describeCurrent())
                  .atToken(currentSpan())
              )
        case TokenKind.RParen =>
          advance()
        case _ =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def decodeRouterTupleConsTail(
      schema: RawSchema.Router
  ): Result[Unit, DecodeError] =
    if !routerTupleConsEnabled || currentKind() != TokenKind.StarColon then Result.done
    else
      Result.task:
        val tupleIndex = schema.read.nn.route(RawSchema.RouterConstruct.Tuple)
        if tupleIndex < 0 || tupleIndex >= schema.cases.length then
          raise(
            DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
          )
        schema.cases(tupleIndex).schema match
          case tupleOf: RawSchema.TupleOf =>
            if tupleOf.read == null then missingReadCapability(tupleOf)
            val read   = tupleOf.read.nn
            val state0 = addSlot(read)(read.init())
            val state1 = parseTupleConsTailLike(
              state0,
              startIndex = 1,
              expectedSlots = VariableTupleSlots
            )(
              _ => decodeBaseInTupleElement(tupleOf.element),
              (state, _) => addSlot(read)(state),
              peekKind() != TokenKind.StarColon
            )
            pushRef(read.finish(state1))
          case other =>
            raise(
              DecodeError.ExpectedType(other.describeSelf, describeCurrent()).atToken(currentSpan())
            )

  private def numberConstruct(
      bounded: RawSchema.RouterConstruct,
      numberMode: RawSchema.RouterNumberMode
  ): RawSchema.RouterConstruct =
    numberMode match
      case RawSchema.RouterNumberMode.Bounded => bounded
      case RawSchema.RouterNumberMode.Raw     => RawSchema.RouterConstruct.RawNumber

  private def parenStartsRecord(): Boolean =
    val scout = scoutFromCurrent()
    scout.scanNext()
    scout.scanNext()
    scout.kind match
      case TokenKind.Identifier | TokenKind.VectorId | TokenKind.EmptyTupleId | TokenKind.Plus |
          TokenKind.Minus | TokenKind.StarColon =>
        scout.scanNext()
        scout.kind == TokenKind.Equals
      case _ => false

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
        mapSlot(mapped.mapping, decodePartialNamedTuple(mapped.base, alreadySeenField))
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

  protected final def decodeTupleOf(schema: RawSchema.TupleOf): Result[Unit, DecodeError] =
    withRead(schema, _.read) { read =>
      Result.task:
        var state = read.init()
        currentKind() match
          case TokenKind.EmptyTupleId =>
            advance()
          case TokenKind.LParen
              if !parenStartsRecord()
                && (parenthesizedTupleSeparators() & SeparatorComma) != 0 =>
            if currentKind() == TokenKind.LParen then advance()
            else
              raise(
                DecodeError
                  .ExpectedType(schema.describeSelf, describeCurrent())
                  .atToken(currentSpan())
              )
            if currentKind() == TokenKind.RParen then
              raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))
            decodeBase(schema.element).check
            val stateAfterFirst = addSlot(read)(state)
            state = parseTupleCommaTailLike(
              stateAfterFirst,
              startCount = 1,
              expectedSlots = VariableTupleSlots
            )(
              _ => decodeBase(schema.element),
              (state, _) => addSlot(read)(state)
            )
            if routerTupleConsEnabled && currentKind() == TokenKind.StarColon then
              val inner = read.finish(state)
              val outer = read.init()
              pushRef(inner)
              val outer1 = addSlot(read)(outer)
              state = parseTupleConsTailLike(
                outer1,
                startIndex = 1,
                expectedSlots = VariableTupleSlots
              )(
                _ => decodeBaseInTupleElement(schema.element),
                (state, _) => addSlot(read)(state),
                peekKind() != TokenKind.StarColon
              )
          case _ =>
            decodeBaseInTupleElement(schema.element).check
            val stateAfterFirst = addSlot(read)(state)
            state = parseTupleConsTailLike(
              stateAfterFirst,
              startIndex = 1,
              expectedSlots = VariableTupleSlots
            )(
              _ => decodeBaseInTupleElement(schema.element),
              (state, _) => addSlot(read)(state),
              peekKind() != TokenKind.StarColon
            )
        pushRef(read.finish(state))
    }

  protected final def decodeDict(schema: RawSchema.Dict): Result[Unit, DecodeError] =
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

  protected final def decodeOption(schema: RawSchema.Option): Result[Unit, DecodeError] =
    Result.task {
      if currentKind() == TokenKind.NullKw then
        advance()
        pushRef(None)
      else
        decodeBase(schema.inner).check
        pushRef(Some(pullAny()))
    }
