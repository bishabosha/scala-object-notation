package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.internal.Token
import steps.result.Result
import steps.result.Result.eval.ok
import steps.result.Result.eval.raise

import scala.annotation.constructorOnly
import scala.compiletime.uninitialized

import TokenDecoder.describe

private[scalanotation] object ExprDecoder:
  def decodeExpr[A: Reader as reader](expr: Expr): Result[A, DecodeError] =
    ExprDecoder().decodeInto(reader, expr)

private[scalanotation] class ExprDecoder extends Internal.PoolHolder:

  def decodeInto[A](reader: Reader[A], expr: Expr): Result[A, DecodeError] =
    val (base, stack) = Reader.unwrap(reader)
    Reader.applyTransforms(decodeBase(base, expr), stack)

  private def decodeBase(
      reader: Reader[Any],
      expr: Expr
  ): Result[Any, DecodeError] =
    reader match
      case _: Reader.MappedSchema[?, ?] =>
        throw IllegalStateException("Mapped readers must be unwrapped before decoding")
      case sc: Reader.NamedTupleReader[a] =>
        decodeNamedTuple(sc, expr).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.SumReader[a] =>
        decodeSum(sc, expr).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.VectorReader[elem, repr, a] =>
        decodeVector(sc, expr).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.DictReader[elem, repr, a] =>
        decodeDict(sc, expr).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.NullaryReader[a] =>
        expr match
          case Expr.NullConstant => Result.Ok(sc.value)
          case other             =>
            Result.Err(DecodeError.ExpectedType(sc.schema.describeSelf, describe(other)))
      case sc: Reader.OptionReader[a] =>
        expr match
          case Expr.NullConstant => Result.Ok(None)
          case other             => decodeInto(sc.inner, other).map(Some(_))
      case sc: Reader.PrimitiveReader[?] =>
        sc.schema match
          case RawSchema.AnyExpr =>
            Result.Ok(expr)
          case schema @ RawSchema.String =>
            expr match
              case Expr.StringConstant(value) => Result.Ok(value)
              case other                      =>
                Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
          case schema @ RawSchema.Char =>
            expr match
              case Expr.CharConstant(value) => Result.Ok(value)
              case other                    =>
                Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
          case schema @ RawSchema.Int =>
            expr match
              case Expr.IntConstant(value) => Result.Ok(value)
              case other                   =>
                Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
          case schema @ RawSchema.Long =>
            expr match
              case Expr.LongConstant(value) => Result.Ok(value)
              case other                    =>
                Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
          case schema @ RawSchema.Float =>
            expr match
              case Expr.FloatConstant(value) => Result.Ok(value)
              case other                     =>
                Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
          case schema @ RawSchema.Double =>
            expr match
              case Expr.DoubleConstant(value) => Result.Ok(value)
              case other                      =>
                Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
          case schema @ RawSchema.Boolean =>
            expr match
              case Expr.BooleanConstant(value) => Result.Ok(value)
              case other                       =>
                Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
          case other =>
            throw IllegalStateException(
              s"Unsupported primitive reader schema ${other.describeSelf}"
            )

  private def decodeVector[Elem, Repr, A](
      schema: Reader.VectorReader[Elem, Repr, A],
      expr: Expr
  ): Result[A, DecodeError] = Result:
    expr match
      case Expr.VectorExpr(elements) =>
        val buf    = schema.builder
        var values = buf.init()
        var index  = 0
        while index < elements.length do
          val value = decodeInto(schema.element, elements(index))
            .mapErr(_.atPath(s"[$index]"))
            .ok
          values = buf.add(values, value)
          index += 1
        buf.finish(values)
      case other =>
        raise(DecodeError.ExpectedType(schema.schema.describeSelf, describe(other)))

  private def decodeNamedTuple[A](
      schema: Reader.NamedTupleReader[A],
      expr: Expr
  ): Result[A, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result:
      schema.schema.isValidNamedTuple(namesPool).ok
      expr match
        case Expr.NamedTupleExpr(fieldExprs) =>
          val fields = schema.fields
          if fieldExprs.length != fields.length then
            raise(DecodeError.FieldCountMismatch(fields.length, fieldExprs.length))
          var index  = 0
          val values = new Array[AnyRef](fields.length)
          while index < fields.length do
            val field     = fields(index)
            val fieldExpr = fieldExprs(index)
            val fieldName = fieldExpr.name
            if fieldName != field.name then
              raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

            val value = decodeInto(field.decoder, fieldExpr.value)
              .mapErr(_.atPath(s".${field.name}"))
              .ok
            values(index) = value.asInstanceOf[AnyRef]
            index += 1

          schema.build(values)
        case other =>
          raise(DecodeError.ExpectedType(schema.schema.describeSelf, describe(other)))
  }

  private def decodeDict[Elem, Repr, A](
      schema: Reader.DictReader[Elem, Repr, A],
      expr: Expr
  ): Result[A, DecodeError] = Result:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val element = schema.element
        val buf     = schema.builder
        var index   = 0
        var state   = buf.init()
        while index < fieldExprs.length do
          val fieldExpr = fieldExprs(index)
          val fieldName = fieldExpr.name
          val value     = decodeInto(element, fieldExpr.value)
            .mapErr(_.atPath(s".${fieldName}"))
            .ok
          state = buf.add(state, fieldName, value)
          index += 1

        buf.finish(state)
      case other =>
        raise(DecodeError.ExpectedType(schema.schema.describeSelf, describe(other)))

  private def decodeSum[A](
      schema: Reader.SumReader[A],
      expr: Expr
  ): Result[A, DecodeError] = Result:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val cases = schema.cases
        if fieldExprs.length != 1 then raise(DecodeError.FieldCountMismatch(1, fieldExprs.length))
        val fieldExpr = fieldExprs(0)
        val caseName  = fieldExpr.name
        val value     = fieldExpr.value
        val sumCase   = cases.get(caseName) match
          case Some(c) => c
          case _       => raise(DecodeError.UnexpectedField(caseName).atPath(s".$caseName"))
        decodeInto(sumCase.decoder, value).mapErr(_.atPath(s".$caseName")).ok
      case other =>
        raise(DecodeError.ExpectedType(schema.schema.describeSelf, describe(other)))

private[scalanotation] object TokenDecoder:

  private[scalanotation] def decode[T](
      tokens: List[Token],
      rootName: String,
      decoder: Reader[T]
  ): Result[T, DecodeError] =
    TokenDecoder(tokens).decodeRoot(decoder, rootName)

  private[scalanotation] def decodeAnyRoot[T](
      tokens: List[Token],
      decoder: Reader[T]
  ): Result[Expr.SourceFile[T], DecodeError] =
    TokenDecoder(tokens).decodeAnyRoot(decoder)

  private[scalanotation] def decodeExpression[T](
      tokens: List[Token],
      decoder: Reader[T]
  ): Result[T, DecodeError] =
    TokenDecoder(tokens).decodeExpression(decoder)

  private[scalanotation] def describe(token: Token | Expr): String =
    token match
      case t: Token => describe(t)
      case e: Expr  => describe(e)

  private[scalanotation] def describe(token: Token): String =
    token match
      case Token.ValKw(_)             => "'val'"
      case Token.VectorId(_)          => "'Vector'"
      case Token.TrueKw(_)            => "'true'"
      case Token.FalseKw(_)           => "'false'"
      case Token.NullKw(_)            => "'null'"
      case Token.Identifier(name, _)  => s"identifier '$name'"
      case Token.IntLit(raw, _, _)    => s"integer literal '$raw'"
      case Token.LongLit(raw, _, _)   => s"long literal '$raw'"
      case Token.FloatLit(raw, _, _)  => s"float literal '$raw'"
      case Token.DoubleLit(raw, _, _) => s"double literal '$raw'"
      case Token.StringLit(raw, _, _) => s"string literal $raw"
      case Token.CharLit(raw, _, _)   => s"character literal '$raw'"
      case Token.Equals(_)            => "'='"
      case Token.Plus(_)              => "'+'"
      case Token.Minus(_)             => "'-'"
      case Token.Comma(_)             => "','"
      case Token.LParen(_)            => "'('"
      case Token.RParen(_)            => "')'"
      case Token.Eof(_)               => "end of input"

  private[scalanotation] def describe(expr: Expr): String =
    expr match
      case Expr.VectorExpr(_)              => "Vector(...)"
      case Expr.NamedTupleExpr(fieldExprs) =>
        if fieldExprs.isEmpty then "NamedTuple.Empty" else s"(${fieldExprs.head.name} = ...)"
      case Expr.StringConstant(value)  => s"""("$value": String)"""
      case Expr.CharConstant(value)    => s"('$value': Char)"
      case Expr.IntConstant(value)     => s"($value: Int)"
      case Expr.LongConstant(value)    => s"($value: Long)"
      case Expr.FloatConstant(value)   => s"($value: Float)"
      case Expr.DoubleConstant(value)  => s"($value: Double)"
      case Expr.BooleanConstant(value) => s"($value: Boolean)"
      case Expr.NullConstant           => "null"

private final class TokenDecoder(@constructorOnly tokens: List[Token])
    extends Internal.TokenStream(tokens) {
  import scala.util.boundary.Label

  type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  def decodeRoot[T](
      schema: Reader[T],
      rootName: String
  ): Result[T, DecodeError] =
    Result:
      expectVal().ok
      val declaredName = expectIdentifier().ok
      if declaredName != rootName then raise(DecodeError.UnexpectedRoot(declaredName))
      expectEquals().ok
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      value

  def decodeAnyRoot[T](
      schema: Reader[T]
  ): Result[Expr.SourceFile[T], DecodeError] =
    Result:
      expectVal().ok
      val declaredName = expectIdentifier().ok
      expectEquals().ok
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      Expr.SourceFile(Map(declaredName -> value))

  def decodeExpression[T](schema: Reader[T]): Result[T, DecodeError] =
    Result:
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      value

  private def decodeTaggedAs[T](reader: Reader[T]): Result[T, DecodeError] =
    val (base, stack) = Reader.unwrap(reader)
    Reader.applyTransforms[T](decodeBase(base), stack)

  private def decodeBase(reader: Reader[Any]): Result[Any, DecodeError] =
    reader match
      case _: Reader.MappedSchema[?, ?] =>
        throw IllegalStateException("Mapped readers must be unwrapped before decoding")
      case sc: Reader.NamedTupleReader[a] =>
        decodeNamedTuple(sc).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.SumReader[a] =>
        decodeSum(sc).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.VectorReader[elem, repr, a] =>
        decodeVector(sc).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.DictReader[elem, repr, a] =>
        decodeDict(sc).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.OptionReader[a] =>
        decodeOption(sc).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.NullaryReader[a] =>
        decodeNull(_ => sc.value).asInstanceOf[Result[Any, DecodeError]]
      case sc: Reader.PrimitiveReader[?] =>
        sc.schema match
          case RawSchema.AnyExpr => exprVisitor.inferExpr()
          case RawSchema.String  => decodeString(identity)
          case RawSchema.Char    => decodeChar(identity)
          case RawSchema.Int     => decodeInt(identity)
          case RawSchema.Long    => decodeLong(identity)
          case RawSchema.Float   => decodeFloat(identity)
          case RawSchema.Double  => decodeDouble(identity)
          case RawSchema.Boolean => decodeBoolean(identity)
          case other             =>
            throw IllegalStateException(
              s"Unsupported primitive reader schema ${other.describeSelf}"
            )

  private def decodeNamedTuple[A](
      schema: Reader.NamedTupleReader[A]
  ): Result[A, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result {
      schema.schema.isValidNamedTuple(namesPool).ok
      val fields  = schema.fields
      val factory = schema.build
      val values  = new Array[AnyRef](fields.length)

      val allowEmpty =
        fields.isEmpty // FIXME: must be hoisted to allow inlining parseNamedTupleStructure!

      val parsed = parseNamedTupleStructure(schema.schema, allowEmpty = allowEmpty) {
        (actualName, nameSpan, fieldIndex) =>
          def actualFieldErr(err: DecodeError): DecodeError =
            err.atPath(s".${actualName}").atToken(nameSpan)
          if fieldIndex >= fields.length then
            raise(actualFieldErr(DecodeError.FieldCountMismatch(fields.length, fieldIndex + 1)))
          else if seenNames.alreadySeen(actualName) then
            raise(actualFieldErr(DecodeError.DuplicateField(actualName)))
          else
            val expectedField = fields(fieldIndex)
            if actualName != expectedField.name then
              raise(actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName)))
            else
              val value = decodeTaggedAs(expectedField.decoder).mapErr(actualFieldErr).ok
              values(fieldIndex) = value.asInstanceOf[AnyRef]
      }

      if parsed.fieldCount != fields.length then
        var err = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(parsed.closingSpan))

      factory(values)
    }
  }

  private def decodeSum(schema: Reader.SumReader[?]): Result[Any, DecodeError] =
    Result {
      val cases        = schema.cases
      var decoded: Any = null
      val parsed       = parseNamedTupleStructure(schema.schema, allowEmpty = false) {
        (actualName, nameSpan, fieldIndex) =>
          if fieldIndex >= 1 then
            raise(
              DecodeError
                .FieldCountMismatch(1, fieldIndex + 1)
                .atPath(s".${actualName}")
                .atToken(nameSpan)
            )
          else
            val sumCase = cases.get(actualName) match
              case Some(c) => c
              case _       =>
                raise(
                  DecodeError
                    .UnexpectedField(actualName)
                    .atPath(s".${actualName}")
                    .atToken(nameSpan)
                )
            decoded = decodeTaggedAs(sumCase.decoder)
              .mapErr(_.atPath(s".${actualName}"))
              .ok
      }
      if parsed.fieldCount != 1 then
        var err = DecodeError.FieldCountMismatch(1, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(parsed.closingSpan))
      decoded
    }

  private def decodeVector[Elem, Repr, A](
      schema: Reader.VectorReader[Elem, Repr, A]
  ): Result[A, DecodeError] =
    Result {
      val buf     = schema.builder
      var values  = buf.init()
      val element = schema.element
      parseVectorStructure(schema.schema) { indexInVector =>
        val value = decodeTaggedAs(element)
          .mapErr(_.atPath(s"[$indexInVector]"))
          .ok
        values = buf.add(values, value)
      }
      buf.finish(values)
    }

  private def decodeDict[Elem, Repr, A](
      schema: Reader.DictReader[Elem, Repr, A]
  ): Result[A, DecodeError] =
    namesPool.withBorrowed { seenNames =>
      Result {
        val buf     = schema.builder
        val element = schema.element
        var state   = buf.init()
        val parsed  = parseNamedTupleStructure(schema.schema, allowEmpty = false) {
          (name, nameSpan, _) =>
            if seenNames.alreadySeen(name) then
              raise(DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan))
            val elem = decodeTaggedAs(element).mapErr(_.atPath(s".${name}")).ok
            state = buf.add(state, name, elem)
        }
        val _ = parsed.closingSpan
        val _ = parsed.fieldName
        val _ = parsed.fieldCount
        buf.finish(state)
      }
    }

  private def decodeOption[A](schema: Reader.OptionReader[A]): Result[Option[A], DecodeError] =
    Result {
      currentToken() match
        case Token.NullKw(_) =>
          advance()
          None
        case _ =>
          Some(decodeTaggedAs(schema.inner).ok)
    }

  private object exprVisitor:
    private val AnyNamedTupleSchema: RawSchema.NamedTuple =
      RawSchema.NamedTuple(IArray.empty[RawSchema.Field])
    private val AnyVectorSchema: RawSchema.Vector =
      RawSchema.Vector(RawSchema.AnyExpr)

    def inferExpr(): Result[Expr, DecodeError] =
      currentToken() match
        case Token.LParen(_)                    => onNamedTuple(AnyNamedTupleSchema)
        case Token.VectorId(_)                  => onVector(AnyVectorSchema)
        case Token.StringLit(_, _, _)           => onString()
        case Token.CharLit(_, _, _)             => onChar()
        case Token.IntLit(_, _, _)              => onInt()
        case Token.LongLit(_, _, _)             => onLong()
        case Token.FloatLit(_, _, _)            => onFloat()
        case Token.DoubleLit(_, _, _)           => onDouble()
        case Token.TrueKw(_) | Token.FalseKw(_) => onBoolean()
        case Token.NullKw(_)                    => onNull()
        case Token.Minus(_)                     =>
          peekToken() match
            case Token.IntLit(_, _, _)    => onInt()
            case Token.LongLit(_, _, _)   => onLong()
            case Token.FloatLit(_, _, _)  => onFloat()
            case Token.DoubleLit(_, _, _) => onDouble()
            case token                    =>
              Result.Err(DecodeError.ExpectedType("Number", describe(token)).atToken(token.span))
        case other =>
          Result.Err(DecodeError.ExpectedExpression(describe(other)).atToken(other.span))

    def onNamedTuple(schema: RawSchema.NamedTuple): Result[Expr, DecodeError] =
      namesPool.withBorrowed { seenNames =>
        Result {
          val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
          val parsed = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameSpan, _) =>
            if seenNames.alreadySeen(name) then
              raise(DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan))
            val elem = inferExpr().mapErr(_.atPath(s".${name}")).ok
            fieldExprs += ((name, elem))
          }
          val _ = parsed.closingSpan
          val _ = parsed.fieldName
          val _ = parsed.fieldCount
          Expr.NamedTupleExpr(fieldExprs.result())
        }
      }

    def onVector(schema: RawSchema.Vector): Result[Expr, DecodeError] = Result {
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure(schema) { _ =>
        elements += inferExpr().ok
      }
      Expr.VectorExpr(elements.result())
    }

    def onString(): Result[Expr, DecodeError] = decodeString(Expr.StringConstant.apply)

    def onChar(): Result[Expr, DecodeError] = decodeChar(Expr.CharConstant.apply)

    def onInt(): Result[Expr, DecodeError] = decodeInt(Expr.IntConstant.apply)

    def onLong(): Result[Expr, DecodeError] = decodeLong(Expr.LongConstant.apply)

    def onFloat(): Result[Expr, DecodeError] = decodeFloat(Expr.FloatConstant.apply)

    def onDouble(): Result[Expr, DecodeError] = decodeDouble(Expr.DoubleConstant.apply)

    def onBoolean(): Result[Expr, DecodeError] = decodeBoolean(Expr.BooleanConstant.apply)

    def onNull(): Result[Expr, DecodeError] = decodeNull(_ => Expr.NullConstant)

  private class NamedTupleParseResultBuf() {
    var fieldCount: Int               = uninitialized
    var fieldName: String | Null      = uninitialized
    var closingSpan: DecodeError.Span = uninitialized
  }

  private object NamedTupleParseResultBuf extends NamedTupleParseResultBuf() {
    def push(fieldCount: Int, fieldName: String | Null, closingSpan: DecodeError.Span): this.type =
      this.fieldCount = fieldCount
      this.fieldName = fieldName
      this.closingSpan = closingSpan
      this
  }

  private inline def parseNamedTupleStructure(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, DecodeError.Span, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResultBuf, DecodeError] = {
    import Internal.loop

    currentToken() match
      case Token.LParen(_) => advance()
      case other           =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span))

    currentToken() match
      case token @ Token.RParen(_) =>
        advance()
        if allowEmpty then NamedTupleParseResultBuf.push(0, null, token.span)
        else raise(DecodeError.UnitValueNotAllowed().atToken(token.span))
      case _ =>
        var fieldIndex                   = 0
        var lastFieldName: String | Null = null
        val rparen: Token.RParen         = loop {
          currentToken() match
            case Token.Identifier(actualName, nameSpan) =>
              advance()
              currentToken() match
                case Token.Equals(_) => advance()
                case other           =>
                  raise(DecodeError.ExpectedEquals(describe(other)).atToken(other.span))

              consumeFieldValue(actualName, nameSpan, fieldIndex)
              lastFieldName = actualName
              fieldIndex += 1

              currentToken() match
                case Token.Comma(_) =>
                  advance()
                  currentToken() match
                    case rparen @ Token.RParen(_) => loop.break(rparen)
                    case _                        => ()
                case rparen @ Token.RParen(_) => loop.break(rparen)
                case other                    =>
                  raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))

            case other =>
              raise(DecodeError.ExpectedFieldName(describe(other)).atToken(other.span))
        }
        advance()
        NamedTupleParseResultBuf.push(fieldIndex, lastFieldName, rparen.span)
  }

  private inline def parseVectorStructure(schema: RawSchema)(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span))

    var indexInVector = 0

    if currentToken().isInstanceOf[Token.RParen] then advance()
    else
      var done = false
      while !done do
        consumeElementValue(indexInVector)
        indexInVector += 1

        currentToken() match
          case Token.Comma(_) =>
            advance()
            if currentToken().isInstanceOf[Token.RParen] then done = true
          case Token.RParen(_) => done = true
          case other           =>
            raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))

      currentToken() match
        case Token.RParen(_) =>
          advance()
        case other =>
          raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))
  }

  private def decodeString[A](wrap: String => A): Result[A, DecodeError] =
    Result {
      val first  = decodeStringAtom().ok
      var isPlus = currentToken().isInstanceOf[Token.Plus]
      if !isPlus then wrap(first)
      else
        val builder = StringBuilder() ++= first
        while isPlus do
          advance()
          builder ++= decodeStringAtom().ok
          isPlus = currentToken().isInstanceOf[Token.Plus]
        wrap(builder.toString())
    }

  private def decodeStringAtom(): Result[String, DecodeError] = Result:
    currentToken() match
      case Token.StringLit(value = value) =>
        advance()
        value
      case other =>
        raise(
          DecodeError
            .ExpectedType(RawSchema.String.describeSelf, describe(other))
            .atToken(other.span)
        )

  private def decodeChar[A](wrap: Char => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.CharLit(value = value) =>
        advance()
        wrap(value)
      case other =>
        raise(
          DecodeError.ExpectedType(RawSchema.Char.describeSelf, describe(other)).atToken(other.span)
        )

  private def decodeInt[A](wrap: Int => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = {
        case Token.IntLit(value = value) => value
        case other                       =>
          raise(
            DecodeError
              .ExpectedType(RawSchema.Int.describeSelf, describe(other))
              .atToken(other.span)
          )
      },
      negator = -1,
      one = 1,
      prod = _ * _,
      wrap = wrap
    )

  private def decodeLong[A](wrap: Long => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = {
        case Token.LongLit(value = value) => value
        case other                        =>
          raise(
            DecodeError
              .ExpectedType(RawSchema.Long.describeSelf, describe(other))
              .atToken(other.span)
          )
      },
      negator = -1L,
      one = 1L,
      prod = _ * _,
      wrap = wrap
    )

  private def decodeFloat[A](wrap: Float => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = {
        case Token.FloatLit(value = value) => value
        case other                         =>
          raise(
            DecodeError
              .ExpectedType(RawSchema.Float.describeSelf, describe(other))
              .atToken(other.span)
          )
      },
      negator = -1.0f,
      one = 1.0f,
      prod = _ * _,
      wrap = wrap
    )

  private def decodeDouble[A](wrap: Double => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = {
        case Token.DoubleLit(value = value) => value
        case other                          =>
          raise(
            DecodeError
              .ExpectedType(RawSchema.Double.describeSelf, describe(other))
              .atToken(other.span)
          )
      },
      negator = -1.0d,
      one = 1.0d,
      prod = _ * _,
      wrap = wrap
    )

  private def decodeBoolean[A](wrap: Boolean => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.TrueKw(_) =>
        advance()
        wrap(true)
      case Token.FalseKw(_) =>
        advance()
        wrap(false)
      case other =>
        raise(
          DecodeError
            .ExpectedType(RawSchema.Boolean.describeSelf, describe(other))
            .atToken(other.span)
        )

  private def decodeNull[A](wrap: Null => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.NullKw(_) =>
        advance()
        wrap(null)
      case other =>
        raise(
          DecodeError
            .ExpectedType(RawSchema.Nullary.describeSelf, describe(other))
            .atToken(other.span)
        )

  private inline def decodeSigned[N, A](
      inline literal: Token => N,
      negator: N,
      one: N,
      prod: (N, N) => N,
      wrap: N => A
  ): A =
    val sign =
      currentToken() match
        case Token.Minus(_) =>
          advance()
          negator
        case _ =>
          one
    val value = literal(currentToken())
    advance()
    wrap(prod(sign, value))

  private def expectVal(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.ValKw(_) =>
        advance()
      case other =>
        raise(DecodeError.ExpectedVal(describe(other)).atToken(other.span))

  private def expectIdentifier(): Result[String, DecodeError] = Result:
    currentToken() match
      case Token.Identifier(name, _) =>
        advance()
        name
      case other =>
        raise(DecodeError.ExpectedIdentifier(describe(other)).atToken(other.span))

  private def expectEquals(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.Equals(_) =>
        advance()
      case other =>
        raise(DecodeError.ExpectedEquals(describe(other)).atToken(other.span))

  private def expectEof(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.Eof(_) => ()
      case other        => raise(DecodeError.ExpectedEof(describe(other)).atToken(other.span))
}
