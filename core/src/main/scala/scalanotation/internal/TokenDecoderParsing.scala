package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

private[scalanotation] trait TokenExpressionParser extends TokenDecoderParsing:
  self: TokenStream =>
  // used to contain some methods for Expr decoding.

private[scalanotation] trait TokenDecoderParsing extends TokenDecoderSupport:
  self: TokenStream =>

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
      case TokenKind.Plus         => "+"
      case TokenKind.Minus        => "-"
      case TokenKind.StarColon    => "*:"
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

  /** parses `<name> =`, pushing the field name into [[stringSlot]] */
  protected final def parseNamedFieldStart(): Result[Unit, DecodeError] =
    Result.task:
      val actualName = currentKind() match
        case TokenKind.Identifier   => currentName()
        case TokenKind.VectorId     => "Vector"
        case TokenKind.EmptyTupleId => "EmptyTuple"
        case TokenKind.Plus         => "+"
        case TokenKind.Minus        => "-"
        case TokenKind.StarColon    => "*:"
        case _                      =>
          raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))
      advance()
      if currentKind() == TokenKind.Equals then
        advance()
        pushString(actualName)
      else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  protected inline def parseNamedTupleStructure(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    if currentKind() == TokenKind.LParen then advance()
    else
      raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan()))

    // parseNamedTupleStructureAfterOpen(schema, allowEmpty)(consumeFieldValue)
    val parsed: NamedTupleParseResult =
      parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
        case parsed: NamedTupleParseResult => parsed
        case err: Result.Err[DecodeError]  =>
          scala.util.boundary.break(err) // TODO: replace with Result.breakErr

    if !allowEmpty && parsed.fieldCount == 0 then
      raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))
    parsed
  }

  protected inline def parseNamedTupleStructureAfterOpen(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
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
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = {
    parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
      case parsed: NamedTupleParseResult => parsed
      case err: Result.Err[DecodeError]  =>
        scala.util.boundary.break(err) // TODO: replace with Result.breakErr
  }

  protected inline def parsePartialNamedTupleStructureInner(
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): NamedTupleParseResult | Result.Err[DecodeError] =
    // to share the logic without breaking the label optimisation, we need to cache the result and
    // then redispatch the break at the call-site. i.e. nested inline calls dont seem to compose
    // well enough to pass along the label. i would like to investigate why.
    {
      scala.util.boundary {
        import Internal.loop

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

  protected inline def parseVectorStructure(schema: RawSchema)(
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
