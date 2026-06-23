package scalanotation.internal

import scalanotation.BuilderSlots
import scalanotation.DecodeError
import scalanotation.internal.Internal.loop
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

import scala.util.boundary.Label
import scalanotation.schema.RawSchema

private[scalanotation] trait BaseDecoders extends SharedHelpers:
  self: TokenStream =>

  protected type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  protected def namedTupleParseResult: NamedTupleParseResult

  protected final def expectedTypeAtCurrent(schema: RawSchema[?]): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  // The sign is passed into `literal` rather than multiplied in afterwards, so integer literals
  // are range-checked with the sign known — `-2147483648` is valid although its magnitude
  // overflows a positive Int. Parameters are inline so the lambda literals beta-reduce away
  // instead of going through Function1, which is not specialized for Float and would box on
  // every float decode.
  protected inline def decodeSigned[N](
      inline literal: Boolean => N,
      inline store: N => Unit
  ): Unit =
    val negative =
      if currentKind() == TokenKind.Minus then
        advance()
        true
      else false
    val value = literal(negative)
    advance()
    store(value)

  protected final def expectVal(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.ValKw then advance()
    else raise(DecodeError.ExpectedVal(describeCurrent()).atToken(currentSpan()))

  protected final def expectPackageStatement(packageName: String): Result[Unit, DecodeError] =
    Result.task:
      if packageName.nonEmpty then
        expectPackage().check
        expectQualifiedIdentifier().check
        val declaredName = pullStringStrict()
        if declaredName != packageName then raise(DecodeError.UnexpectedPackage(declaredName))
        acceptStatementSeparator()

  protected final def acceptStatementSeparator(): Unit =
    if currentKind() == TokenKind.Semicolon then advance()

  protected final def expectPackage(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.PackageKw then advance()
    else raise(DecodeError.ExpectedPackage(describeCurrent()).atToken(currentSpan()))

  /** parses a dotted identifier path, pushing it into [[stringSlot]] */
  protected final def expectQualifiedIdentifier(): Result[Unit, DecodeError] = Result.task:
    expectIdentifier().check
    if currentKind() == TokenKind.Dot then
      val builder = new StringBuilder(pullStringStrict())
      while currentKind() == TokenKind.Dot do
        advance()
        builder.append('.')
        expectIdentifier().check
        builder ++= pullStringStrict()
      pushString(builder.result())

  /** parses an identifier, pushing it into [[stringSlot]] */
  protected final def expectIdentifier(): Result[Unit, DecodeError] = Result.task:
    val name = currentKind() match
      case TokenKind.Identifier   => currentName()
      case TokenKind.VectorId     => "Vector"
      case TokenKind.EmptyTupleId => "EmptyTuple"
      case TokenKind.TupleId      => "Tuple"
      case TokenKind.Plus         => "+"
      case TokenKind.Minus        => "-"
      case _                      =>
        raise(DecodeError.ExpectedIdentifier(describeCurrent()).atToken(currentSpan()))
    advance()
    pushString(name)

  protected final def expectEquals(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.Equals then advance()
    else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  protected final def expectEof(): Result[Unit, DecodeError] = Result.task:
    if currentKind() != TokenKind.Eof then
      raise(DecodeError.ExpectedEof(describeCurrent()).atToken(currentSpan()))

  protected inline def eval[T](inline op: T): T =
    def exprToEval(): T = op
    exprToEval()

  protected inline val VariableTupleSlots = -1

  protected inline def parseTupleLike[State](
      schema: RawSchema[?],
      state0: State,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State
  ): Resulting[State, DecodeError] =
    currentKind() match
      case TokenKind.EmptyTupleId =>
        val emptyTupleOffset = currentOffset()
        advance()
        if expectedSlots > 0 then
          raise(
            DecodeError.FieldCountMismatch(expectedSlots, 0).atToken(spanAt(emptyTupleOffset))
          )
        state0
      case TokenKind.TupleId =>
        parseSingletonTupleLike(state0, expectedSlots)(decodeElement, addElement)
      case TokenKind.LParen =>
        val openOffset = currentOffset()
        advance()
        if currentKind() == TokenKind.RParen then
          raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))
        if expectedSlots == 0 then
          raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))
        decodeElement(0).check
        currentKind() match
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
                .atToken(spanAt(openOffset))
            )
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
      case _ =>
        raise(
          DecodeError
            .ExpectedType(schema.describeSelf, describeCurrent())
            .atToken(currentSpan())
        )

  protected inline def parseSingletonTupleLike[State](
      state0: State,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State
  ): Resulting[State, DecodeError] =
    val tupleOffset = currentOffset()
    advance()
    if currentKind() == TokenKind.LParen then advance()
    else
      raise(
        DecodeError.ExpectedType("Tuple(...)", describeCurrent()).atToken(currentSpan())
      )
    if currentKind() == TokenKind.RParen then
      raise(DecodeError.FieldCountMismatch(1, 0).atToken(currentSpan()))
    if expectedSlots == 0 then
      raise(DecodeError.FieldCountMismatch(0, 1).atToken(spanAt(tupleOffset)))

    decodeElement(0).check
    currentKind() match
      case TokenKind.RParen =>
        advance()
        if expectedSlots > 1 then
          raise(DecodeError.FieldCountMismatch(expectedSlots, 1).atToken(spanAt(tupleOffset)))
        addElement(state0, 0)
      case TokenKind.Comma =>
        raise(DecodeError.FieldCountMismatch(1, 2).atToken(currentSpan()))
      case _ =>
        raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  protected inline def parseTupleCommaTailLike[State](
      state0: State,
      startCount: Int,
      expectedSlots: Int
  )(
      inline decodeElement: Int => Result[Unit, DecodeError],
      inline addElement: (State, Int) => State
  ): Result[Unit, DecodeError] = Result.task {
    var state                         = state0
    var count                         = startCount
    val closingSpan: DecodeError.Span = loop {
      currentKind() match
        case TokenKind.Comma =>
          advance()
          currentKind() match
            case TokenKind.RParen =>
              loop.break(currentSpan())
            case _ =>
              if expectedSlots > 0 && count >= expectedSlots then
                raise(
                  DecodeError
                    .FieldCountMismatch(expectedSlots, count + 1)
                    .atToken(currentSpan())
                )
              decodeElement(count).check
              state = addElement(state, count)
              count += 1
        case TokenKind.RParen =>
          loop.break(currentSpan())
        case _ =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
    }
    assert(currentKind() == TokenKind.RParen)
    advance()
    if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(closingSpan))
    if expectedSlots > 0 && count != expectedSlots then
      raise(DecodeError.FieldCountMismatch(expectedSlots, count).atToken(closingSpan))
    pushRef(state)
  }

  /** parses `<name> =`, pushing the field name into [[stringSlot]] */
  protected final def parseNamedFieldStart(): Result[Unit, DecodeError] =
    Result.task:
      val actualName = currentKind() match
        case TokenKind.Identifier   => currentName()
        case TokenKind.VectorId     => "Vector"
        case TokenKind.EmptyTupleId => "EmptyTuple"
        case TokenKind.TupleId      => "Tuple"
        case TokenKind.Plus         => "+"
        case TokenKind.Minus        => "-"
        case _                      =>
          raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))
      advance()
      if currentKind() == TokenKind.Equals then
        advance()
        pushString(actualName)
      else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  /** parses `<name> =` without materializing or pushing the field name */
  protected final def parseNamedFieldStartNoPush(): Result[Unit, DecodeError] =
    Result.task:
      currentKind() match
        case TokenKind.Identifier | TokenKind.VectorId | TokenKind.EmptyTupleId |
            TokenKind.TupleId | TokenKind.Plus | TokenKind.Minus =>
          advance()
        case _ =>
          raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))
      if currentKind() == TokenKind.Equals then advance()
      else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  protected inline def parseNamedTupleStructure(
      schema: RawSchema[?],
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    if currentKind() == TokenKind.LParen then advance()
    else
      raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan()))

    val parsed: NamedTupleParseResult =
      parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
        case parsed: NamedTupleParseResult => parsed
        case err: Result.Err[DecodeError]  =>
          scala.util.boundary.break(err) // TODO: replace with Result.breakErr

    if !allowEmpty && parsed.fieldCount == 0 then
      raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))
    parsed
  }

  protected inline def parsePartialNamedTupleStructure(
      schema: RawSchema[?]
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = {
    parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
      case parsed: NamedTupleParseResult => parsed
      case err: Result.Err[DecodeError]  =>
        scala.util.boundary.break(err) // TODO: replace with Result.breakErr
  }

  protected inline def parsePartialNamedTupleStructureByCurrentNameResult(
      inline consumeFieldValue: (Int, Int) => Result[Unit, DecodeError]
  ): NamedTupleParseResult | Result.Err[DecodeError] =
    currentKind() match
      case TokenKind.RParen =>
        val closingOffset = currentOffset()
        advance()
        namedTupleParseResult.push(0, null, closingOffset)
      case _ =>
        var fieldIndex: Int                     = 0
        var closingOffset: Int                  = 0
        var err: Result.Err[DecodeError] | Null = null
        var done                                = false
        while !done && err == null do
          val nameOffset = currentOffset()
          consumeFieldValue(nameOffset, fieldIndex) match
            case fieldErr: Result.Err[DecodeError] => err = fieldErr
            case _                                 =>
              fieldIndex += 1
              currentKind() match
                case TokenKind.Comma =>
                  advance()
                  if currentKind() == TokenKind.RParen then
                    closingOffset = currentOffset()
                    done = true
                case TokenKind.RParen =>
                  closingOffset = currentOffset()
                  done = true
                case _ =>
                  err = Result.Err(
                    DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan())
                  )
        if err != null then err
        else
          advance()
          namedTupleParseResult.push(fieldIndex, null, closingOffset)

  protected inline def parsePartialNamedTupleStructureInner(
      schema: RawSchema[?]
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): NamedTupleParseResult | Result.Err[DecodeError] =
    // to share the logic without breaking the label optimisation, we need to cache the result and
    // then redispatch the break at the call-site. i.e. nested inline calls dont seem to compose
    // well enough to pass along the label. i would like to investigate why.
    {
      scala.util.boundary {
        currentKind() match {
          case TokenKind.RParen =>
            val closingOffset = currentOffset()
            advance()
            namedTupleParseResult.push(0, null, closingOffset)
          case _ =>
            var fieldIndex: Int              = 0
            var lastFieldName: String | Null = null
            val closingOffset: Int           = loop {
              val nameOffset = currentOffset()
              parseNamedFieldStart().check
              val actualName = pullStringStrict()
              consumeFieldValue(actualName, nameOffset, fieldIndex)
              lastFieldName = actualName
              fieldIndex += 1

              currentKind() match
                case TokenKind.Comma =>
                  advance()
                  if currentKind() == TokenKind.RParen then loop.break(currentOffset())
                case TokenKind.RParen =>
                  loop.break(currentOffset())
                case _ =>
                  raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
            }
            advance()
            namedTupleParseResult.push(fieldIndex, lastFieldName, closingOffset)
        }
      }
    }

  protected inline def parseVectorStructure(schema: RawSchema[?])(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    if currentKind() == TokenKind.VectorId && peekKind() == TokenKind.LParen then
      advance()
      advance()
    else
      raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan()))

    var indexInVector = 0

    if currentKind() == TokenKind.RParen then advance()
    else
      var done = false
      while !done do
        consumeElementValue(indexInVector)
        indexInVector += 1

        currentKind() match
          case TokenKind.Comma =>
            advance()
            if currentKind() == TokenKind.RParen then done = true
          case TokenKind.RParen => done = true
          case _                =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
  }
