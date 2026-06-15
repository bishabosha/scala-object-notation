package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

private[scalanotation] trait TokenTupleDecoder
    extends TokenDecoderParsing: // TokenExpressionParser:
  self: TokenStream =>

  protected def decodeBase(schema: RawSchema): Result[Unit, DecodeError]
  protected def decodeOption(schema: RawSchema.Option): Result[Unit, DecodeError]
  protected def decodeBaseInTupleElement(schema: RawSchema): Result[Unit, DecodeError] =
    decodeBase(schema)

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
              val stateAfterFirst = addSlot(read)(state, 0)
              val expectedSlots   = slots.length
              state = parseTupleConsTailLike(stateAfterFirst, startIndex = 1, expectedSlots)(
                index => decodeTupleSlotValue(slots, index, allowStringConcat = false),
                (state, index) => addSlot(read)(state, index),
                emptyTupleTerminates = true
              )
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
              def afterComma() = Result.task {
                // FIXME: must lift out expectedSlots to avoid boxing closure
                val expectedSlots = slots.length
                pushRef(
                  parseTupleCommaTailLike(
                    stateAfterFirst,
                    startCount = 1,
                    expectedSlots
                  )(
                    index => decodeTupleSlotValue(slots, index, allowStringConcat = true),
                    (state, index) => addSlot(read)(state, index)
                  )
                )
              }

              afterComma().check
            case TokenKind.StarColon =>
              def stateAfterTail() = Result.task {
                // FIXME: must lift out expectedSlots to avoid boxing closure
                val expectedSlots = slots.length
                pushRef(
                  parseTupleConsTailLike(
                    stateAfterFirst,
                    startIndex = 1,
                    expectedSlots
                  )(
                    index => decodeTupleSlotValue(slots, index, allowStringConcat = false),
                    (state, index) => addSlot(read)(state, index),
                    emptyTupleTerminates = true
                  )
                )
              }
              currentKind() match
                case TokenKind.RParen =>
                  advanceTupleClose().check
                  stateAfterTail().check
                case _ =>
                  raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
            case TokenKind.RParen =>
              val rparenOffset = currentOffset()
              advanceTupleClose().check
              currentKind() match
                case TokenKind.StarColon =>
                  def stateAfterTail() = Result.task {
                    // FIXME: must lift out expectedSlots to avoid boxing closure
                    val expectedSlots       = slots.length
                    val stateAfterStarColon = parseTupleConsTailLike(
                      stateAfterFirst,
                      startIndex = 1,
                      expectedSlots
                    )(
                      index => decodeTupleSlotValue(slots, index, allowStringConcat = false),
                      (state, index) => addSlot(read)(state, index),
                      emptyTupleTerminates = true
                    )
                    pushRef(stateAfterStarColon)
                  }
                  stateAfterTail().check
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
      // FIXME: must lift out expectedSlots to avoid boxing closure
      val expectedSlots = slots.length
      pushRef(
        parseTupleCommaTailLike(state0, startCount, expectedSlots)(
          index => decodeTupleSlotValue(slots, index, allowStringConcat = true),
          (state, index) => addSlot(read)(state, index)
        )
      )

  /** decodes the `*:`-separated tuple tail, leaving the final builder state in the Any slot */
  protected final def decodeTupleConsTail(
      read: RawSchema.TupleRead
  )(
      slots: IArray[RawSchema],
      state0: read.State,
      startIndex: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      // FIXME: must lift out expectedSlots to avoid boxing closure
      val expectedSlots = slots.length
      pushRef(
        parseTupleConsTailLike(state0, startIndex, expectedSlots)(
          index => decodeTupleSlotValue(slots, index, allowStringConcat = false),
          (state, index) => addSlot(read)(state, index),
          emptyTupleTerminates = true
        )
      )

  protected final val VariableTupleSlots = -1

  protected inline def hasFixedTupleSlots(inline expectedSlots: Int): Boolean =
    expectedSlots >= 0

  protected final def tupleCurrentKind(): Int =
    currentKind()

  protected final def tupleCurrentSpan(): DecodeError.Span =
    currentSpan()

  protected final def tupleDescribeCurrent(): String =
    describeCurrent()

  protected final def tupleAdvance(): Unit =
    advance()

  protected inline def parseTupleCommaTailLike[State](
      state0: State,
      startCount: Int,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State
  ): Resulting[State, DecodeError] =
    var state                         = state0
    var count                         = startCount
    var done                          = false
    var closingSpan: DecodeError.Span = tupleCurrentSpan()
    while !done do
      tupleCurrentKind() match
        case TokenKind.Comma =>
          tupleAdvance()
          tupleCurrentKind() match
            case TokenKind.RParen =>
              closingSpan = tupleCurrentSpan()
              if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(closingSpan))
              done = true
            case _ =>
              if hasFixedTupleSlots(expectedSlots) && count >= expectedSlots then
                raise(
                  DecodeError
                    .FieldCountMismatch(expectedSlots, count + 1)
                    .atToken(tupleCurrentSpan())
                )
              decodeElement(count).check
              state = addElement(state, count)
              count += 1
        case TokenKind.RParen =>
          closingSpan = tupleCurrentSpan()
          done = true
        case _ =>
          raise(DecodeError.ExpectedRParen(tupleDescribeCurrent()).atToken(tupleCurrentSpan()))

    if tupleCurrentKind() == TokenKind.RParen then tupleAdvance()
    else raise(DecodeError.ExpectedRParen(tupleDescribeCurrent()).atToken(tupleCurrentSpan()))
    if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(closingSpan))
    if hasFixedTupleSlots(expectedSlots) && count != expectedSlots then
      raise(DecodeError.FieldCountMismatch(expectedSlots, count).atToken(closingSpan))
    state

  protected inline def parseTupleConsTailLike[State](
      state0: State,
      startIndex: Int,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State,
      inline emptyTupleTerminates: Boolean
  ): Resulting[State, DecodeError] =
    var state = state0
    var index = startIndex
    var done  = false
    while !done do
      if tupleCurrentKind() == TokenKind.StarColon then tupleAdvance()
      else
        raise(DecodeError.ExpectedType("'*:'", tupleDescribeCurrent()).atToken(tupleCurrentSpan()))
      tupleCurrentKind() match
        case TokenKind.EmptyTupleId if emptyTupleTerminates =>
          val emptyTupleSpan = tupleCurrentSpan()
          tupleAdvance()
          if hasFixedTupleSlots(expectedSlots) && index != expectedSlots then
            raise(
              DecodeError
                .FieldCountMismatch(expectedSlots, index)
                .atToken(emptyTupleSpan)
            )
          done = true
        case _ =>
          if hasFixedTupleSlots(expectedSlots) && index >= expectedSlots then
            raise(
              DecodeError
                .FieldCountMismatch(expectedSlots, index + 1)
                .atToken(tupleCurrentSpan())
            )
          decodeElement(index).check
          state = addElement(state, index)
          index += 1
    state

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
        decodeBaseInTupleElement(schema)

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
  protected final val SeparatorComma               = 1
  protected final val SeparatorStarColon           = 2
  protected final val SeparatorFollowedByStarColon = 4

  /** Scans ahead (without buffering) from the current '(' to its matching ')' to discover which
    * separators the parenthesized tuple uses, plus whether the balanced group is immediately
    * followed by `*:`, returned as a bitmask. Uses a scout scanner so the bounded token buffer of
    * the stream is preserved; no tokens are materialized.
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
    if sawOpen && scout.kind == TokenKind.RParen then
      scout.scanNext()
      if scout.kind == TokenKind.StarColon then separators |= SeparatorFollowedByStarColon
    separators
