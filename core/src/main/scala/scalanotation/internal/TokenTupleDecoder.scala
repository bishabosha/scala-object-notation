package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise
import scalanotation.internal.Internal.loop

private[scalanotation] trait TokenTupleDecoder
    extends TokenDecoderParsing: // TokenExpressionParser:
  self: TokenStream =>

  protected def decodeBase(schema: RawSchema): Result[Unit, DecodeError]

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

  protected final val VariableTupleSlots = -1

  protected final def tupleCurrentKind(): Int =
    currentKind()

  protected final def tupleCurrentOffset(): Int =
    currentOffset()

  protected final def tupleCurrentSpan(): DecodeError.Span =
    currentSpan()

  protected final def tupleSpanAt(offset: Int): DecodeError.Span =
    spanAt(offset)

  protected final def tupleDescribeCurrent(): String =
    describeCurrent()

  protected final def tupleAdvance(): Unit =
    advance()

  protected inline def parseTupleLike[State](
      schema: RawSchema,
      state0: State,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State
  ): Resulting[State, DecodeError] =
    tupleCurrentKind() match
      case TokenKind.EmptyTupleId =>
        val emptyTupleOffset = tupleCurrentOffset()
        tupleAdvance()
        if expectedSlots > 0 then
          raise(
            DecodeError.FieldCountMismatch(expectedSlots, 0).atToken(tupleSpanAt(emptyTupleOffset))
          )
        state0
      case TokenKind.TupleId =>
        parseSingletonTupleLike(state0, expectedSlots)(decodeElement, addElement)
      case TokenKind.LParen =>
        val openOffset = tupleCurrentOffset()
        tupleAdvance()
        if tupleCurrentKind() == TokenKind.RParen then
          raise(DecodeError.UnitValueNotAllowed().atToken(tupleCurrentSpan()))
        if expectedSlots == 0 then
          raise(DecodeError.FieldCountMismatch(0, 1).atToken(tupleCurrentSpan()))
        decodeElement(0).check
        tupleCurrentKind() match
          case TokenKind.Comma =>
            def commaTail() = parseTupleCommaTailLike(
              addElement(state0, 0),
              startCount = 1,
              expectedSlots
            )(
              decodeElement,
              addElement
            )
            commaTail().check
            pullAny().asInstanceOf[State]
          case TokenKind.RParen =>
            raise(
              DecodeError
                .ExpectedType(schema.describeSelf, "(...)")
                .atToken(tupleSpanAt(openOffset))
            )
          case _ =>
            raise(DecodeError.ExpectedRParen(tupleDescribeCurrent()).atToken(tupleCurrentSpan()))
      case _ =>
        raise(
          DecodeError
            .ExpectedType(schema.describeSelf, tupleDescribeCurrent())
            .atToken(
              tupleCurrentSpan()
            )
        )

  protected inline def parseSingletonTupleLike[State](
      state0: State,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State
  ): Resulting[State, DecodeError] =
    val tupleOffset = tupleCurrentOffset()
    tupleAdvance()
    if tupleCurrentKind() == TokenKind.LParen then tupleAdvance()
    else
      raise(
        DecodeError.ExpectedType("Tuple(...)", tupleDescribeCurrent()).atToken(tupleCurrentSpan())
      )
    if tupleCurrentKind() == TokenKind.RParen then
      raise(DecodeError.FieldCountMismatch(1, 0).atToken(tupleCurrentSpan()))
    if expectedSlots == 0 then
      raise(DecodeError.FieldCountMismatch(0, 1).atToken(tupleSpanAt(tupleOffset)))

    decodeElement(0).check
    tupleCurrentKind() match
      case TokenKind.RParen =>
        tupleAdvance()
        if expectedSlots > 1 then
          raise(DecodeError.FieldCountMismatch(expectedSlots, 1).atToken(tupleSpanAt(tupleOffset)))
        addElement(state0, 0)
      case TokenKind.Comma =>
        raise(DecodeError.FieldCountMismatch(1, 2).atToken(tupleCurrentSpan()))
      case _ =>
        raise(DecodeError.ExpectedRParen(tupleDescribeCurrent()).atToken(tupleCurrentSpan()))

  protected inline def parseTupleCommaTailLike[State](
      state0: State,
      startCount: Int,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State
  ): Result[Unit, DecodeError] = Result.task {
    var state                         = state0
    val closingSpan0                  = tupleCurrentSpan()
    var count                         = startCount
    val closingSpan: DecodeError.Span = loop {
      tupleCurrentKind() match
        case TokenKind.Comma =>
          tupleAdvance()
          tupleCurrentKind() match
            case TokenKind.RParen =>
              loop.break(tupleCurrentSpan())
            case _ =>
              if expectedSlots > 0 && count >= expectedSlots then
                raise(
                  DecodeError
                    .FieldCountMismatch(expectedSlots, count + 1)
                    .atToken(tupleCurrentSpan())
                )
              decodeElement(count).check
              state = addElement(state, count)
              count += 1
        case TokenKind.RParen =>
          loop.break(tupleCurrentSpan())
        case _ =>
          raise(DecodeError.ExpectedRParen(tupleDescribeCurrent()).atToken(tupleCurrentSpan()))
    }
    assert(tupleCurrentKind() == TokenKind.RParen)
    tupleAdvance()
    if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(closingSpan))
    if expectedSlots > 0 && count != expectedSlots then
      raise(DecodeError.FieldCountMismatch(expectedSlots, count).atToken(closingSpan))
    pushRef(state)
  }

  protected final def decodeTupleSlotValue(
      slots: IArray[RawSchema],
      index: Int
  ): Result[Unit, DecodeError] =
    decodeBase(slots(index)) match
      case Result.Err(error) => Result.Err(error.atPath(s"[$index]"))
      case ok                => ok
