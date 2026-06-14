package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

import TokenDecoder.describe

private[scalanotation] trait TokenExpressionParser extends TokenDecoderParsing:
  self: TokenStream =>

  private object expressionVisitor:
    private val AnyNamedTupleSchema: RawSchema.NamedTuple =
      RawSchema.NamedTuple(IArray.empty[RawSchema.Field], read = null, write = null)
    private val AnyTupleSchema: RawSchema.Tuple =
      RawSchema.Tuple(IArray.empty[RawSchema], read = null, write = null)
    private val AnyVectorSchema: RawSchema.Vector =
      RawSchema.Vector(
        RawSchema.AnyExpr,
        read = null,
        write = null
      )
    private val EmptyTupleExpr: Expr =
      Expr.TupleExpr(Vector.empty)

    /** parses an expression, pushing the resulting [[Expr]] into the Any slot */
    def inferExpr(): Result[Unit, DecodeError] =
      onStringConcat()

    private def pulledExpr(): Expr = pullAny().asInstanceOf[Expr]

    private def onStringConcat(): Result[Unit, DecodeError] = Result.task {
      onTupleCons().check
      if currentKind() == TokenKind.Plus then
        val builder = pulledExpr() match
          case Expr.StringConstant(value) => new StringBuilder ++= value
          case other                      =>
            raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
        while currentKind() == TokenKind.Plus do
          advance()
          onTupleCons().check
          pulledExpr() match
            case Expr.StringConstant(value) => builder ++= value
            case other                      =>
              raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
        pushRef(Expr.StringConstant(builder.result()))
    }

    private def onTupleCons(): Result[Unit, DecodeError] = Result.task {
      onPrimary().check
      if currentKind() == TokenKind.StarColon then
        val head = pulledExpr()
        advance()
        onTupleCons().check
        pulledExpr() match
          case Expr.TupleExpr(elements) =>
            pushRef(Expr.TupleExpr(head +: elements))
          case other =>
            raise(DecodeError.ExpectedType("Tuple", describe(other)))
    }

    private def onPrimary(): Result[Unit, DecodeError] =
      currentKind() match
        case TokenKind.LParen                     => onParenthesized()
        case TokenKind.VectorId                   => onVector(AnyVectorSchema)
        case TokenKind.EmptyTupleId               => onEmptyTuple()
        case TokenKind.StringLit                  => onString()
        case TokenKind.CharLit                    => onChar()
        case TokenKind.IntLit                     => onInt()
        case TokenKind.LongLit                    => onLong()
        case TokenKind.FloatLit                   => onFloat()
        case TokenKind.DoubleLit                  => onDouble()
        case TokenKind.TrueKw | TokenKind.FalseKw => onBoolean()
        case TokenKind.NullKw                     => onNull()
        case TokenKind.Minus                      =>
          peekKind() match
            case TokenKind.IntLit    => onInt()
            case TokenKind.LongLit   => onLong()
            case TokenKind.FloatLit  => onFloat()
            case TokenKind.DoubleLit => onDouble()
            case _                   =>
              Result.Err(DecodeError.ExpectedType("Number", describePeek()).atToken(peekSpan()))
        case _ =>
          Result.Err(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

    def onParenthesized(): Result[Unit, DecodeError] = Result.task {
      if currentKind() == TokenKind.LParen then advance()
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      if peekKind() == TokenKind.Equals then onNamedTupleAfterOpen(AnyNamedTupleSchema).check
      else
        inferExpr().check
        currentKind() match
          case TokenKind.Comma =>
            onTupleAfterGroupedHead(pulledExpr()).check
          case TokenKind.RParen =>
            advance()
            // the grouped expression remains in the Any slot
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
    }

    def onTupleAfterGroupedHead(first: Expr): Result[Unit, DecodeError] = Result.task {
      val elements = IArray.newBuilder[Expr]
      elements += first
      var count = 1
      var done  = false
      while !done do
        currentKind() match
          case TokenKind.Comma =>
            advance()
            currentKind() match
              case TokenKind.RParen =>
                if count == 1 then
                  raise(DecodeError.FieldCountMismatch(2, 1).atToken(currentSpan()))
                done = true
              case _ =>
                inferExpr().check
                elements += pulledExpr()
                count += 1
          case TokenKind.RParen =>
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1))
      pushRef(Expr.TupleExpr(elements.result()))
    }

    def onNamedTuple(schema: RawSchema.NamedTuple): Result[Unit, DecodeError] = Result.task {
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )
      onNamedTupleAfterOpen(schema).check
    }

    def onNamedTupleAfterOpen(schema: RawSchema.NamedTuple): Result[Unit, DecodeError] =
      namesPool.withBorrowed { seenNames =>
        Result.task {
          val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
          val allowEmpty = false
          val parsed     =
            parseNamedTupleStructureAfterOpen(schema, allowEmpty) { (name, nameOffset, _) =>
              if seenNames.alreadySeen(name) then
                raise(
                  DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset))
                )
              checkOrRaise(inferExpr())(_.atPath(s".${name}"))
              fieldExprs += ((name, pulledExpr()))
            }
          val _ = parsed.closingOffset
          val _ = parsed.fieldName
          val _ = parsed.fieldCount
          pushRef(Expr.NamedTupleExpr(fieldExprs.result()))
        }
      }

    def onVector(schema: RawSchema.Vector): Result[Unit, DecodeError] = Result.task {
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure(schema) { _ =>
        inferExpr().check
        elements += pulledExpr()
      }
      pushRef(Expr.VectorExpr(elements.result()))
    }

    def onString(): Result[Unit, DecodeError] = Result.task:
      decodeString().check
      pushRef(Expr.StringConstant(pullStringStrict()))

    def onChar(): Result[Unit, DecodeError] = Result.task:
      decodeChar().check
      pushRef(Expr.CharConstant(pullCharStrict()))

    def onInt(): Result[Unit, DecodeError] = Result.task:
      decodeInt().check
      pushRef(Expr.IntConstant(pullIntStrict()))

    def onLong(): Result[Unit, DecodeError] = Result.task:
      decodeLong().check
      pushRef(Expr.LongConstant(pullLongStrict()))

    def onFloat(): Result[Unit, DecodeError] = Result.task:
      decodeFloat().check
      pushRef(Expr.FloatConstant(pullFloatStrict()))

    def onDouble(): Result[Unit, DecodeError] = Result.task:
      decodeDouble().check
      pushRef(Expr.DoubleConstant(pullDoubleStrict()))

    def onBoolean(): Result[Unit, DecodeError] = Result.task:
      decodeBoolean().check
      pushRef(Expr.BooleanConstant(pullBooleanStrict()))

    def onNull(): Result[Unit, DecodeError] = Result.task:
      decodeNull().check
      val _ = pullRefStrict().ensuring(_ == null, "Expected null in Any slot after decoding Null")
      pushRef(Expr.NullConstant)

    def onEmptyTuple(): Result[Unit, DecodeError] = Result.task:
      if currentKind() == TokenKind.EmptyTupleId then
        advance()
        pushRef(EmptyTupleExpr)
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

  /** parses an expression, pushing the resulting [[Expr]] into the Any slot */
  protected final def inferExpr(): Result[Unit, DecodeError] =
    expressionVisitor.inferExpr()

  protected final def inferNamedTuple(schema: RawSchema.NamedTuple): Result[Unit, DecodeError] =
    expressionVisitor.onNamedTuple(schema)

  protected final def inferVector(schema: RawSchema.Vector): Result[Unit, DecodeError] =
    expressionVisitor.onVector(schema)

  protected final def inferString(): Result[Unit, DecodeError] =
    expressionVisitor.onString()

  protected final def inferChar(): Result[Unit, DecodeError] =
    expressionVisitor.onChar()

  protected final def inferInt(): Result[Unit, DecodeError] =
    expressionVisitor.onInt()

  protected final def inferLong(): Result[Unit, DecodeError] =
    expressionVisitor.onLong()

  protected final def inferFloat(): Result[Unit, DecodeError] =
    expressionVisitor.onFloat()

  protected final def inferDouble(): Result[Unit, DecodeError] =
    expressionVisitor.onDouble()

  protected final def inferBoolean(): Result[Unit, DecodeError] =
    expressionVisitor.onBoolean()

  protected final def inferNull(): Result[Unit, DecodeError] =
    expressionVisitor.onNull()

  protected final def decodeAnyExpr(): Result[Unit, DecodeError] =
    inferExpr()
