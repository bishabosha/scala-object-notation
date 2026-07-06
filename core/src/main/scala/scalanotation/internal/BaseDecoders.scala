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

  @deprecated("unused", "0.4.1")
  inline def __access(): TokenKind.type = TokenKind // preserves bincompat.

  protected type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  /** The raising channel of a value-returning decoder: a plain `using` parameter — NOT a context
    * function result type, which would allocate a closure per call — through which decode errors
    * break to the enclosing boundary while decoded values return directly to the caller.
    */
  protected type Raise = Label[Result.Err[DecodeError]]

  protected def namedTupleParseResult: NamedTupleParseResult

  protected final def expectedTypeAtCurrent(schema: RawSchema[?]): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  // The sign is passed into `literal` rather than multiplied in afterwards, so integer literals
  // are range-checked with the sign known — `-2147483648` is valid although its magnitude
  // overflows a positive Int. Parameters are inline so the lambda literals beta-reduce away
  // instead of going through Function1, which is not specialized for Float and would box on
  // every float decode.
  protected inline def decodeSigned[N](inline literal: Boolean => N): N =
    val negative =
      if currentKind() == TokenKind.Minus then
        advance()
        true
      else false
    val value = literal(negative)
    if pendingValueError == null then advance()
    value

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

  protected final def collectionLiteralsEnabled: Boolean =
    experimentalFlagEnabled(ExperimentalFlags.AllowCollectionLiterals)

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

  protected inline def parseVectorStructure(schema: RawSchema[?])(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    val closingKind =
      if currentKind() == TokenKind.VectorId && peekKind() == TokenKind.LParen then TokenKind.RParen
      else if collectionLiteralsEnabled && currentKind() == TokenKind.LBracket then
        TokenKind.RBracket
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )

    if closingKind == TokenKind.RParen then
      advance()
      advance()
    else advance()

    var indexInVector = 0

    if currentKind() == closingKind then advance()
    else
      var done = false
      while !done do
        consumeElementValue(indexInVector)
        indexInVector += 1

        currentKind() match
          case TokenKind.Comma =>
            advance()
            if currentKind() == closingKind then done = true
          case kind if kind == closingKind => done = true
          case _                           =>
            raise(expectedClosingError(closingKind))

      if currentKind() == closingKind then advance()
      else raise(expectedClosingError(closingKind))
  }

  protected final def expectedArrowError(): DecodeError =
    DecodeError.Custom(s"Expected '->' but found ${describeCurrent()}").atToken(currentSpan())

  private def expectedClosingError(closingKind: Int): DecodeError =
    if closingKind == TokenKind.RParen then
      DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan())
    else DecodeError.Custom(s"Expected ']' but found ${describeCurrent()}").atToken(currentSpan())
