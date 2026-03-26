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
import scalanotation.internal.RawSchema.Field

private[scalanotation] object ExprDecoder:
  def decodeExpr[A: Reader as reader](expr: Expr): Result[A, DecodeError] =
    ExprDecoder().decodeInto(reader, expr)

private[scalanotation] class ExprDecoder extends Internal.PoolHolder:
  private def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private val pfSentinel: Any => Any = _ => pfSentinel
  private def decodeScalarExpr[T](
      schema: RawSchema,
      expr: Expr
  )(
      extract: PartialFunction[Expr, T]
  ): Result[T, DecodeError] =
    val res = extract.applyOrElse(expr, pfSentinel)
    if res.asInstanceOf[AnyRef] eq pfSentinel then
      Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(expr)))
    else Result.Ok(res.asInstanceOf[T])

  def decodeInto[A](reader: Reader[A], expr: Expr): Result[A, DecodeError] =
    decodeBase(reader.schema, expr).asInstanceOf[Result[A, DecodeError]]

  private def decodeBase(
      schema: RawSchema,
      expr: Expr
  ): Result[Any, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapped.mapping.mapResult(decodeBase(mapped.base, expr))
      case sc: RawSchema.NamedTuple =>
        decodeNamedTuple(sc, expr)
      case sc: RawSchema.Sum =>
        decodeSum(sc, expr)
      case sc: RawSchema.Vector =>
        decodeVector(sc, expr)
      case sc: RawSchema.Dict =>
        decodeDict(sc, expr)
      case sc: RawSchema.Option =>
        expr match
          case Expr.NullConstant => Result.Ok(None)
          case other             => decodeBase(sc.inner, other).map(Some(_))
      case RawSchema.AnyExpr =>
        Result.Ok(expr)
      case RawSchema.String =>
        decodeScalarExpr(RawSchema.String, expr) { case Expr.StringConstant(value) =>
          value
        }
      case RawSchema.Char =>
        decodeScalarExpr(RawSchema.Char, expr) { case Expr.CharConstant(value) =>
          value
        }
      case RawSchema.Int =>
        decodeScalarExpr(RawSchema.Int, expr) { case Expr.IntConstant(value) =>
          value
        }
      case RawSchema.Long =>
        decodeScalarExpr(RawSchema.Long, expr) { case Expr.LongConstant(value) =>
          value
        }
      case RawSchema.Float =>
        decodeScalarExpr(RawSchema.Float, expr) { case Expr.FloatConstant(value) =>
          value
        }
      case RawSchema.Double =>
        decodeScalarExpr(RawSchema.Double, expr) { case Expr.DoubleConstant(value) =>
          value
        }
      case RawSchema.Boolean =>
        decodeScalarExpr(RawSchema.Boolean, expr) { case Expr.BooleanConstant(value) =>
          value
        }
      case RawSchema.Null =>
        expr match
          case Expr.NullConstant => Result.Ok(null)
          case other             =>
            Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeVector(
      schema: RawSchema.Vector,
      expr: Expr
  ): Result[Any, DecodeError] = Result:
    expr match
      case Expr.VectorExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read   = schema.read.nn
        var values = read.init()
        var index  = 0
        while index < elements.length do
          val value = decodeBase(schema.element, elements(index))
            .mapErr(_.atPath(s"[$index]"))
            .ok
          values = read.add(values, value)
          index += 1
        read.finish(values)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple,
      expr: Expr
  ): Result[Any, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result:
      val read = schema.read
      if read == null then missingReadCapability(schema)
      schema.isValidNamedTuple(namesPool).ok
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

            val value = decodeBase(field.schema, fieldExpr.value)
              .mapErr(_.atPath(s".${field.name}"))
              .ok
            values(index) = value.asInstanceOf[AnyRef]
            index += 1

          read.build(values)
        case other =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
  }

  private def decodeDict(
      schema: RawSchema.Dict,
      expr: Expr
  ): Result[Any, DecodeError] = Result:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val read = schema.read
        if read == null then missingReadCapability(schema)
        var index = 0
        var state = read.init()
        while index < fieldExprs.length do
          val fieldExpr = fieldExprs(index)
          val fieldName = fieldExpr.name
          val value     = decodeBase(schema.element, fieldExpr.value)
            .mapErr(_.atPath(s".${fieldName}"))
            .ok
          state = read.add(state, fieldName, value)
          index += 1

        read.finish(state)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeSum(
      schema: RawSchema.Sum,
      expr: Expr
  ): Result[Any, DecodeError] = Result:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        if fieldExprs.length != 1 then raise(DecodeError.FieldCountMismatch(1, fieldExprs.length))
        val fieldExpr = fieldExprs(0)
        val caseName  = fieldExpr.name
        val value     = fieldExpr.value
        val sumCase   = schema.cases.iterator.find(_.name == caseName) match
          case Some(c) => c
          case _       => raise(DecodeError.UnexpectedField(caseName).atPath(s".$caseName"))
        decodeBase(sumCase.schema, value).mapErr(_.atPath(s".$caseName")).ok
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

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

  private def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private[scalanotation] def decodeTaggedAs[T](reader: Reader[T]): Result[T, DecodeError] =
    decodeBase(reader.schema).asInstanceOf[Result[T, DecodeError]]

  private def decodeBase(schema: RawSchema): Result[Any, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapped.mapping.mapResult(decodeBase(mapped.base))
      case sc: RawSchema.NamedTuple =>
        decodeNamedTuple(sc)
      case sc: RawSchema.Sum =>
        decodeSum(sc)
      case sc: RawSchema.Vector =>
        decodeVector(sc)
      case sc: RawSchema.Dict =>
        decodeDict(sc)
      case sc: RawSchema.Option =>
        decodeOption(sc)
      case RawSchema.AnyExpr =>
        decodeAnyExpr()
      case RawSchema.String =>
        decodeString(identity)
      case RawSchema.Char =>
        decodeChar(identity)
      case RawSchema.Int =>
        decodeInt(identity)
      case RawSchema.Long =>
        decodeLong(identity)
      case RawSchema.Float =>
        decodeFloat(identity)
      case RawSchema.Double =>
        decodeDouble(identity)
      case RawSchema.Boolean =>
        decodeBoolean(identity)
      case RawSchema.Null =>
        decodeNull(identity)

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple
  ): Result[Any, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result {
      val read = schema.read
      if read == null then missingReadCapability(schema)
      schema.isValidNamedTuple(namesPool).ok
      val fields = schema.fields
      val values = new Array[AnyRef](fields.length)

      val allowEmpty =
        fields.isEmpty // FIXME: must be hoisted to allow inlining parseNamedTupleStructure!

      val parsed = parseNamedTupleStructure(schema, allowEmpty = allowEmpty) {
        (actualName, nameSpan, fieldIndex) =>
          def actualFieldErr(err: DecodeError): DecodeError =
            err.atPath(s".${actualName}").atToken(nameSpan)
          val validated: DecodeError | Field = eval {
            if fieldIndex >= fields.length then
              actualFieldErr(DecodeError.FieldCountMismatch(fields.length, fieldIndex + 1))
            else if seenNames.alreadySeen(actualName) then
              actualFieldErr(DecodeError.DuplicateField(actualName))
            else
              val expectedField = fields(fieldIndex)
              if actualName != expectedField.name then
                actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
              else expectedField
          }
          validated match
            case expectedField: Field =>
              def decoded = decodeBase(expectedField.schema).mapErr(actualFieldErr)
              val value   = decoded.ok
              values(fieldIndex) = value.asInstanceOf[AnyRef]
            case err: DecodeError => raise(err)
      }

      if parsed.fieldCount != fields.length then
        def err =
          var err0 = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
          if parsed.fieldName != null then err0 = err0.atPath(s".${parsed.fieldName}")
          err0.atToken(parsed.closingSpan)
        raise(err)

      read.build(values)
    }
  }

  private def decodeSum(schema: RawSchema.Sum): Result[Any, DecodeError] =
    Result {
      var decoded: Any = null
      val parsed       = parseNamedTupleStructure(schema, allowEmpty = false) {
        (actualName, nameSpan, fieldIndex) =>
          if fieldIndex >= 1 then
            raise(
              DecodeError
                .FieldCountMismatch(1, fieldIndex + 1)
                .atPath(s".${actualName}")
                .atToken(nameSpan)
            )
          else
            val sumCase = schema.cases.iterator.find(_.name == actualName) match
              case Some(c) => c
              case _       =>
                raise(
                  DecodeError
                    .UnexpectedField(actualName)
                    .atPath(s".${actualName}")
                    .atToken(nameSpan)
                )
            decoded = decodeBase(sumCase.schema)
              .mapErr(_.atPath(s".${actualName}"))
              .ok
      }
      if parsed.fieldCount != 1 then
        var err = DecodeError.FieldCountMismatch(1, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(parsed.closingSpan))
      decoded
    }

  private def decodeVector(schema: RawSchema.Vector): Result[Any, DecodeError] =
    Result {
      val read = schema.read
      if read == null then missingReadCapability(schema)
      var values = read.init()
      parseVectorStructure(schema) { indexInVector =>
        val value = decodeBase(schema.element)
          .mapErr(_.atPath(s"[$indexInVector]"))
          .ok
        values = read.add(values, value)
      }
      read.finish(values)
    }

  private def decodeDict(schema: RawSchema.Dict): Result[Any, DecodeError] =
    namesPool.withBorrowed { seenNames =>
      Result {
        val read = schema.read
        if read == null then missingReadCapability(schema)
        var state  = read.init()
        val parsed = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameSpan, _) =>
          if seenNames.alreadySeen(name) then
            raise(DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan))
          val elem = decodeBase(schema.element).mapErr(_.atPath(s".${name}")).ok
          state = read.add(state, name, elem)
        }
        val _ = parsed.closingSpan
        val _ = parsed.fieldName
        val _ = parsed.fieldCount
        read.finish(state)
      }
    }

  private def decodeOption(schema: RawSchema.Option): Result[Any, DecodeError] =
    Result {
      currentToken() match
        case Token.NullKw(_) =>
          advance()
          None
        case _ =>
          Some(decodeBase(schema.inner).ok)
    }

  private object exprVisitor:
    private val AnyNamedTupleSchema: RawSchema.NamedTuple =
      RawSchema.NamedTuple(IArray.empty[RawSchema.Field], read = null, write = null)
    private val AnyVectorSchema: RawSchema.Vector =
      RawSchema.Vector(
        RawSchema.AnyExpr,
        read = null,
        write = null
      )

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
          val parsed     =
            parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameSpan, _) =>
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

  private[scalanotation] def decodeAnyExpr(): Result[Expr, DecodeError] =
    exprVisitor.inferExpr()

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

  private inline def eval[T](inline op: T): T =
    def exprToEval(): T = op
    exprToEval()

  private inline def parseNamedTupleStructure(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, DecodeError.Span, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResultBuf, DecodeError] = {
    import Internal.loop

    val preamble: DecodeError | Token | DecodeError.Span = eval {
      currentToken() match
        case Token.LParen(_) =>
          advance()
          currentToken() match
            case token @ Token.RParen(_) =>
              advance()
              if allowEmpty then token.span
              else DecodeError.UnitValueNotAllowed().atToken(token.span)
            case nextToken => nextToken
        case other =>
          DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span)
    }

    preamble match
      case rParen: DecodeError.Span => NamedTupleParseResultBuf.push(0, null, rParen)
      case err: DecodeError         => raise(err)
      case _                        =>
        var fieldIndex                   = 0
        var lastFieldName: String | Null = null
        val rparen: Token.RParen         = loop {
          currentToken() match
            case Token.Identifier(actualName, nameSpan) =>
              val skipToValue = eval {
                advance()
                currentToken() match
                  case Token.Equals(_) =>
                    advance()
                    null
                  case other =>
                    DecodeError.ExpectedEquals(describe(other)).atToken(other.span)
              }

              skipToValue match
                case null => ()
                case err  => raise(err)

              consumeFieldValue(actualName, nameSpan, fieldIndex)
              lastFieldName = actualName
              fieldIndex += 1

              val expectCommaOrRParen: Token | DecodeError = eval {
                currentToken() match
                  case Token.Comma(_) =>
                    advance()
                    currentToken() match
                      case rparen @ Token.RParen(_) => rparen
                      case nextToken                => nextToken
                  case rparen @ Token.RParen(_) => rparen
                  case other                    =>
                    DecodeError.ExpectedRParen(describe(other)).atToken(other.span)
              }
              expectCommaOrRParen match
                case rparen: Token.RParen => loop.break(rparen)
                case err: DecodeError     => raise(err)
                case _                    => ()

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

  private[scalanotation] def decodeString[A](wrap: String => A): Result[A, DecodeError] =
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

  private[scalanotation] def decodeChar[A](wrap: Char => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.CharLit(value = value) =>
        advance()
        wrap(value)
      case other =>
        raise(
          DecodeError.ExpectedType(RawSchema.Char.describeSelf, describe(other)).atToken(other.span)
        )

  private[scalanotation] def decodeInt[A](wrap: Int => A): Result[A, DecodeError] = Result:
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

  private[scalanotation] def decodeLong[A](wrap: Long => A): Result[A, DecodeError] = Result:
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

  private[scalanotation] def decodeFloat[A](wrap: Float => A): Result[A, DecodeError] = Result:
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

  private[scalanotation] def decodeDouble[A](wrap: Double => A): Result[A, DecodeError] = Result:
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

  private[scalanotation] def decodeBoolean[A](wrap: Boolean => A): Result[A, DecodeError] =
    Result:
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

  private[scalanotation] def decodeNull[A](wrap: Null => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.NullKw(_) =>
        advance()
        wrap(null)
      case other =>
        raise(
          DecodeError
            .ExpectedType(RawSchema.Null.describeSelf, describe(other))
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
