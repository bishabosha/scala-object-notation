package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

private[scalanotation] trait TokenTupleDecoder extends TokenExpressionParser:
  self: TokenStream =>

  protected def decodeBase(schema: RawSchema): Result[Unit, DecodeError]
  protected def decodeOption(schema: RawSchema.Option): Result[Unit, DecodeError]

  protected final def decodeTuple(schema: RawSchema.Tuple): Result[Unit, DecodeError] =
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
  protected final def decodeParenthesizedTuple(
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
  protected final def decodeTupleCommaTail(
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
  protected final def decodeTupleConsTail(
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

  protected final def decodeTupleSlotValue(
      slots: IArray[RawSchema],
      index: Int,
      allowStringConcat: Boolean
  ): Result[Unit, DecodeError] =
    decodeTupleElement(slots(index), allowStringConcat) match
      case Result.Err(error) => Result.Err(error.atPath(s"[$index]"))
      case ok                => ok

  protected final def advanceTupleOpen(schema: RawSchema.Tuple): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )

  protected final def advanceTupleClose(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  protected final def advanceTupleComma(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.Comma then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  protected final def advanceTupleConsSeparator(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.StarColon then advance()
      else raise(DecodeError.ExpectedType("'*:'", describeCurrent()).atToken(currentSpan()))

  protected final def advanceTupleEmptyTail(
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

  protected final def decodeTupleElement(
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

  protected final def decodeGroupedTupleElement(schema: RawSchema): Result[Unit, DecodeError] =
    Result.task {
      if currentKind() == TokenKind.LParen then advance()
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      decodeTupleElement(schema, allowStringConcat = true).check
      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
    }

  protected final def canDecodeFromLParen(schema: RawSchema): Boolean =
    schema match
      case RawSchema.Mapped(base, _)      => canDecodeFromLParen(base)
      case RawSchema.Option(inner)        => canDecodeFromLParen(inner)
      case _: RawSchema.NamedTuple        => true
      case _: RawSchema.Tuple             => true
      case _: RawSchema.PartialNamedTuple => true
      case _: RawSchema.Sum               => true
      case _: RawSchema.DiscriminatorSum  => true
      case _: RawSchema.Dict              => true
      case _: RawSchema.TupleOf           => true
      case _: RawSchema.Router            => true
      case RawSchema.Ref(_, target)       => canDecodeFromLParen(target())
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
  protected final def parenthesizedTupleSeparators(): Int =
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
