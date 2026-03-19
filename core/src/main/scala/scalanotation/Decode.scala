package scalanotation

import scalanotation.TaggedSchema.MappedSchema
import steps.result.Result
import steps.result.Result.eval.break
import steps.result.Result.eval.ok
import steps.result.Result.eval.raise

import scala.annotation.constructorOnly
import scala.annotation.implicitNotFound
import scala.annotation.publicInBinary
import scala.collection.mutable
import scala.deriving.Mirror
import scala.util.NotGiven
import scala.util.boundary

import compiletime.uninitialized
import NamedTuple.NamedTuple
import NamedTuple.AnyNamedTuple
import NamedTuple.NamedTuple as SNamedTuple
import TaggedSchema.Builders.AtPath
import TokenDecoder.describe
import Internal.showType
import scala.reflect.ClassTag
import scalanotation.Schema.Dict

private[scalanotation] object ExprDecoder:

  def decodeTagged[A](schema: TaggedSchema[A], expr: Expr): Result[A, DecodeError] =
    TaggedSchema.finalize(schema, decode(schema.schema, expr))

  def decodeVector[Elem, Repr, A](
      schema: Schema.Vector[Elem, Repr, A],
      expr: Expr
  ): Result[A, DecodeError] = Result:
    expr match
      case Expr.VectorExpr(elements) =>
        val buf    = schema.builder
        var values = buf.init()
        var index  = 0
        while index < elements.length do
          val value = decodeTagged(schema.element, elements(index))
            .mapErr(_.atPath(s"[$index]"))
            .ok
          values = buf.add(values, value)
          index += 1
        buf.finish(values)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  def decodeNamedTuple[A](
      schema: Schema.NamedTuple[A],
      expr: Expr
  ): Result[A, DecodeError] = Result:
    schema.validNamedTuple.ok
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val fields = schema.fields
        if fieldExprs.length != fields.length then
          raise(DecodeError.FieldCountMismatch(fields.length, fieldExprs.length))
        val seen   = Internal.JumboNameSet.alloc() // TODO: weak global pool?
        var index  = 0
        val values = new Array[AnyRef](fields.length)
        while index < fields.length do
          val field     = fields(index)
          val fieldExpr = fieldExprs(index)
          val fieldName = fieldExpr.name
          if seen.alreadySeen(fieldName) then raise(DecodeError.DuplicateField(fieldName))
          if fieldName != field.name then
            raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

          val value = decodeTagged(field.decoder, fieldExpr.value)
            .mapErr(_.atPath(s".${field.name}"))
            .ok
          values(index) = value.asInstanceOf[AnyRef]
          index += 1

        schema.build(values)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  def decodeDict[Elem, Repr, A](
      schema: Schema.Dict[Elem, Repr, A],
      expr: Expr
  ): Result[A, DecodeError] = Result:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val seen    = Internal.JumboNameSet.alloc() // TODO: weak global pool?
        val element = schema.element
        val buf     = schema.builder
        var index   = 0
        var state   = buf.init()
        while index < fieldExprs.length do
          val fieldExpr = fieldExprs(index)
          val fieldName = fieldExpr.name
          if seen.alreadySeen(fieldName) then raise(DecodeError.DuplicateField(fieldName))
          val value = decodeTagged(element, fieldExpr.value)
            .mapErr(_.atPath(s".${fieldName}"))
            .ok
          state = buf.add(state, fieldName, value)
          index += 1

        buf.finish(state)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  def decodeSum[A](
      schema: Schema.Sum { val cases: Map[String, Schema.SumCase[A]] },
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
        break(decodeTagged(sumCase.decoder, value).mapErr(_.atPath(s".$caseName")))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  def decode(schema: Schema, expr: Expr): Result[Any, DecodeError] = Result:
    schema match
      case sc @ Schema.NamedTuple(fields, build) =>
        break(decodeNamedTuple(sc, expr))

      case sc @ Schema.Sum(cases) =>
        type Case
        val sc0: Schema.Sum { val cases: Map[String, Schema.SumCase[Case]] } =
          sc.asInstanceOf[Schema.Sum { val cases: Map[String, Schema.SumCase[Case]] }]
        break(decodeSum(sc0, expr))
      case sc @ Schema.Vector(_, _) =>
        break(decodeVector(sc, expr))
      case sc @ Schema.Dict(_, _) =>
        break(decodeDict(sc, expr))

      case Schema.AnyExpr =>
        expr

      case sc @ Schema.String =>
        expr match
          case Expr.StringConstant(value) => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case sc @ Schema.Char =>
        expr match
          case Expr.CharConstant(value) => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case sc @ Schema.Int =>
        expr match
          case Expr.IntConstant(value) => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case sc @ Schema.Long =>
        expr match
          case Expr.LongConstant(value) => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case sc @ Schema.Float =>
        expr match
          case Expr.FloatConstant(value) => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case sc @ Schema.Double =>
        expr match
          case Expr.DoubleConstant(value) => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case sc @ Schema.Boolean =>
        expr match
          case Expr.BooleanConstant(value) => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case sc @ Schema.Nullary(value) =>
        expr match
          case Expr.NullConstant => value
          case other => raise(DecodeError.ExpectedType(sc.describeSelf, describe(other)))

      case Schema.Option(innerDecoder) =>
        expr match
          case Expr.NullConstant => None
          case other             => Some(decodeTagged(innerDecoder, other).ok)

private[scalanotation] object TokenDecoder:

  private[scalanotation] def decode[T](
      tokens: List[Token],
      rootName: String,
      decoder: TaggedSchema[T]
  ): Result[T, DecodeError] =
    TokenDecoder(tokens).decodeRoot(decoder, rootName)

  private[scalanotation] def decodeAnyRoot[T](
      tokens: List[Token],
      decoder: TaggedSchema[T]
  ): Result[SourceFile[T], DecodeError] =
    TokenDecoder(tokens).decodeAnyRoot(decoder)

  private[scalanotation] def decodeExpression[T](
      tokens: List[Token],
      decoder: TaggedSchema[T]
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
      case Token.StringLit(raw, _, _) => s"string literal $"$raw$""
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
      case Expr.VectorExpr(elements)       => "Vector(...)"
      case Expr.NamedTupleExpr(fieldExprs) =>
        if fieldExprs.isEmpty then "NamedTuple.Empty" else s"(${fieldExprs.head.name} = ...)"
      case Expr.StringConstant(value)  => s"($"$value$": String)"
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

  // TODO: should think how this could scale to making a global shared object.
  private val namesPool = Internal.HashNamesLocalPool()

  def decodeRoot[T](
      schema: TaggedSchema[T],
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
      schema: TaggedSchema[T]
  ): Result[SourceFile[T], DecodeError] =
    Result:
      expectVal().ok
      val declaredName = expectIdentifier().ok
      expectEquals().ok
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      SourceFile(ValDecl(declaredName, value))

  def decodeExpression[T](schema: TaggedSchema[T]): Result[T, DecodeError] =
    Result:
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      value

  private trait BasicDecodingVisitor[A]:
    def onNamedTuple[B](schema: Schema.NamedTuple[B]): Result[A, DecodeError]
    def onVector[Elem, Repr, B](schema: Schema.Vector[Elem, Repr, B]): Result[A, DecodeError]
    def onString(): Result[A, DecodeError]
    def onChar(): Result[A, DecodeError]
    def onInt(): Result[A, DecodeError]
    def onLong(): Result[A, DecodeError]
    def onFloat(): Result[A, DecodeError]
    def onDouble(): Result[A, DecodeError]
    def onBoolean(): Result[A, DecodeError]
    def onNull(): Result[A, DecodeError]

  private trait ExtendedDecodingVisitor[A] extends BasicDecodingVisitor[A]:
    def onSum(schema: Schema.Sum): Result[A, DecodeError]
    def onDict[Elem, Repr, B](schema: Schema.Dict[Elem, Repr, B]): Result[A, DecodeError]
    def onOption[B](inner: TaggedSchema[B]): Result[A, DecodeError]
    def onNullary[B](value: B): Result[A, DecodeError]

  private object checkedVisitor extends ExtendedDecodingVisitor[Any]:
    def decodeChecked(schema: Schema): Result[Any, DecodeError] =
      schema match
        case sc @ Schema.NamedTuple(_, _) => onNamedTuple(sc)
        case sc @ Schema.Sum(_)           => onSum(sc)
        case sc @ Schema.Vector(_, _)     => onVector(sc)
        case sc @ Schema.Dict(_, _)       => onDict(sc)
        case Schema.String                => onString()
        case Schema.Char                  => onChar()
        case Schema.Int                   => onInt()
        case Schema.Long                  => onLong()
        case Schema.Float                 => onFloat()
        case Schema.Double                => onDouble()
        case Schema.Boolean               => onBoolean()
        case Schema.Nullary(value)        => onNullary(value)
        case Schema.Option(inner)         => onOption(inner)
        case Schema.AnyExpr               =>
          // possible to decode some parts to typed, and have a nested part that is Expr
          exprVisitor.inferExpr()

    def onNamedTuple[A](
        schema: Schema.NamedTuple[A]
    ): Result[A, DecodeError] = namesPool.withBorrowed { seenNames =>
      Result {
        schema.validNamedTuple.ok
        val fields  = schema.fields
        val factory = schema.build
        val values  = new Array[AnyRef](fields.length)

        // FIXME: need to lift out or else the boundary/break optimisation fails.
        val emptyFields = fields.isEmpty

        val parsed = parseNamedTupleStructure(schema, allowEmpty = emptyFields) {
          (actualName, nameSpan, fieldIndex) =>
            def actualFieldErr[T](err: DecodeError): DecodeError =
              err.atPath(s".${actualName}").atToken(nameSpan)
            if fieldIndex >= fields.length then
              raise(actualFieldErr(DecodeError.FieldCountMismatch(fields.length, fieldIndex + 1)))
            else if seenNames.alreadySeen(actualName) then
              def dupErr = actualFieldErr(DecodeError.DuplicateField(actualName))
              raise(dupErr)
            else
              val expectedField = fields(fieldIndex)
              if actualName != expectedField.name then
                def wrongNameErr =
                  actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                raise(wrongNameErr)
              else
                def decoded = decodeTaggedAs(expectedField.decoder).mapErr(actualFieldErr)
                val value   = decoded.ok
                values(fieldIndex) = value.asInstanceOf[AnyRef]
        }
        if parsed.fieldCount != fields.length then
          def tooManyFieldsErr =
            var err = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
            if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
            err.atToken(parsed.closingSpan)
          raise(tooManyFieldsErr)
        factory(values)
      }
    }

    def onSum(schema: Schema.Sum): Result[Any, DecodeError] =
      Result {
        val cases        = schema.cases
        var decoded: Any = null
        val parsed       = parseNamedTupleStructure(schema, allowEmpty = false) {
          (actualName, nameSpan, fieldIndex) =>
            if fieldIndex >= 1 then
              def tooManyFieldsErr =
                DecodeError
                  .FieldCountMismatch(1, fieldIndex + 1)
                  .atPath(s".${actualName}")
                  .atToken(nameSpan)
              raise(tooManyFieldsErr)
            else
              val sumCase = cases.get(actualName) match
                case Some(c) => c
                case _       =>
                  def unexpectedCase =
                    DecodeError
                      .UnexpectedField(actualName)
                      .atPath(s".${actualName}")
                      .atToken(nameSpan)
                  raise(unexpectedCase)
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

    def onVector[Elem, Repr, A](
        schema: Schema.Vector[Elem, Repr, A]
    ): Result[A, DecodeError] =
      Result {
        val buf     = schema.builder
        var values  = buf.init()
        val element = schema.element
        parseVectorStructure(schema) { indexInVector =>
          val value = decodeTaggedAs(element)
            .mapErr(_.atPath(s"[$indexInVector]"))
            .ok
          values = buf.add(values, value)
        }
        buf.finish(values)
      }

    def onDict[Elem, Repr, A](schema: Dict[Elem, Repr, A]): Result[A, DecodeError] =
      namesPool.withBorrowed { seenNames =>
        Result {
          val buf     = schema.builder
          val element = schema.element
          var state   = buf.init()
          val res = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameSpan, _) =>
            if seenNames.alreadySeen(name) then
              def dupErr = DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan)
              raise(dupErr)
            def tryExpr = decodeTaggedAs(element).mapErr(_.atPath(s".${name}"))
            val elem    = tryExpr.ok
            state = buf.add(state, name, elem)
          }
          val _ = res.closingSpan // consume result
          val _ = res.fieldName   // consume result
          val _ = res.fieldCount  // consume result
          buf.finish(state)
        }
      }

    def onOption[A](inner: TaggedSchema[A]): Result[Option[A], DecodeError] =
      Result {
        currentToken() match
          case Token.NullKw(_) =>
            advance()
            None
          case _ =>
            Some(decodeTaggedAs(inner).ok)
      }

    def onString(): Result[String, DecodeError] = decodeString(
      identity
    )
    def onChar(): Result[Char, DecodeError]            = decodeChar(identity)
    def onInt(): Result[Int, DecodeError]              = decodeInt(identity)
    def onLong(): Result[Long, DecodeError]            = decodeLong(identity)
    def onFloat(): Result[Float, DecodeError]          = decodeFloat(identity)
    def onDouble(): Result[Double, DecodeError]        = decodeDouble(identity)
    def onBoolean(): Result[Boolean, DecodeError]      = decodeBoolean(identity)
    def onNullary[A](value: A): Result[A, DecodeError] = decodeNull(_ => value)
    def onNull(): Result[Null, DecodeError]            = decodeNull(identity)

  private object exprVisitor extends BasicDecodingVisitor[Expr]:
    private val AnyNamedTupleSchema: Schema.NamedTuple[Expr] =
      Schema.NamedTuple[Expr](IArray.empty[Schema.Field[?]], _ => ???)
    private val AnyVectorSchema: Schema.Vector[Expr, Expr, Expr] =
      Schema.Vector(TaggedSchema.identity(Schema.AnyExpr), null)

    final def inferExpr(): Result[Expr, DecodeError] =
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

    def onNamedTuple[A](schema: Schema.NamedTuple[A]): Result[Expr, DecodeError] =
      namesPool.withBorrowed { seenNames =>
        Result {
          val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
          val buf = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameSpan, _) =>
            if seenNames.alreadySeen(name) then
              def dupErr = DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan)
              raise(dupErr)
            def tryExpr = inferExpr().mapErr(_.atPath(s".${name}"))
            val elem    = tryExpr.ok
            fieldExprs += ((name, elem))
          }
          val _ = buf.closingSpan // consume result
          val _ = buf.fieldName   // consume result
          val _ = buf.fieldCount  // consume result
          Expr.NamedTupleExpr(fieldExprs.result())
        }
      }

    def onVector[Elem, Repr, A](
        schema: Schema.Vector[Elem, Repr, A]
    ): Result[Expr, DecodeError] = Result {
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure(schema) { _ =>
        elements += inferExpr().ok
      }
      Expr.VectorExpr(elements.result())
    }

    def onString(): Result[Expr, DecodeError] =
      decodeString(s => Expr.StringConstant(s))
    def onChar(): Result[Expr, DecodeError] =
      decodeChar(c => Expr.CharConstant(c))
    def onInt(): Result[Expr, DecodeError] =
      decodeInt(i => Expr.IntConstant(i))
    def onLong(): Result[Expr, DecodeError] =
      decodeLong(l => Expr.LongConstant(l))
    def onFloat(): Result[Expr, DecodeError] =
      decodeFloat(f => Expr.FloatConstant(f))
    def onDouble(): Result[Expr, DecodeError] =
      decodeDouble(d => Expr.DoubleConstant(d))
    def onBoolean(): Result[Expr, DecodeError] =
      decodeBoolean(b => Expr.BooleanConstant(b))
    def onNull(): Result[Expr, DecodeError] =
      decodeNull(_ => Expr.NullConstant)

  private def decodeTaggedAs[T](
      schema: TaggedSchema[T]
  ): Result[T, DecodeError] =
    val result = schema.schema match
      case Schema.AnyExpr => exprVisitor.inferExpr()
      case other          => checkedVisitor.decodeChecked(other)
    TaggedSchema.finalize(schema, result)

  private class NamedTupleParseResultBuf() {
    var fieldCount: Int          = uninitialized
    var fieldName: String | Null = uninitialized
    var closingSpan: Span        = uninitialized
  }
  // have to be careful not to share this! currently we have single threaded decode.
  // just have to ensure that results are processed before calling parseNamedTupleStructure again.
  private object NamedTupleParseResultBuf extends NamedTupleParseResultBuf() {
    def push(fieldCount: Int, fieldName: String | Null, closingSpan: Span): this.type =
      this.fieldCount = fieldCount
      this.fieldName = fieldName
      this.closingSpan = closingSpan
      this
  }
  private inline def parseNamedTupleStructure(
      schema: Schema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Span, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResultBuf, DecodeError] = {
    import Internal.loop
    currentToken() match
      case Token.LParen(_) => advance()
      case other           =>
        def expectedNT =
          DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span)
        raise(expectedNT)

    currentToken() match
      case token @ Token.RParen(_) =>
        advance()
        if allowEmpty then
          // TODO: add early ok return as well!
          NamedTupleParseResultBuf.push(0, null, token.span)
        else raise(DecodeError.UnitValueNotAllowed().atToken(token.span))
      case _ =>
        var fieldIndex                   = 0
        var lastFieldName: String | Null = null
        val rparen: Token.RParen         = loop {
          currentToken() match
            case Token.Identifier(actualName, nameSpan) =>
              def skipToValue(): Token | Null =
                advance()
                currentToken() match
                  case Token.Equals(_) => advance(); null
                  case other           => other

              skipToValue() match
                case null  => // continue loop
                case other =>
                  raise(DecodeError.ExpectedEquals(describe(other)).atToken(other.span))

              consumeFieldValue(actualName, nameSpan, fieldIndex)
              lastFieldName = actualName
              fieldIndex += 1

              def nextParen(): Token | Null =
                currentToken() match
                  case Token.Comma(_) =>
                    advance()
                    currentToken() match
                      case rparen @ Token.RParen(_) => rparen
                      case _                        => null
                  case rparen @ Token.RParen(_) => rparen
                  case other                    => other

              nextParen() match
                case rparen @ Token.RParen(_) => loop.break(rparen)
                case null                     => // continue loop
                case other                    =>
                  raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))

            case other =>
              raise(DecodeError.ExpectedFieldName(describe(other)).atToken(other.span))
        }
        advance()
        NamedTupleParseResultBuf.push(fieldIndex, lastFieldName, rparen.span)
  }

  private inline def parseVectorStructure(schema: Schema)(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) =>
        def expectedVec =
          DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span)
        raise(expectedVec)

    var indexInVector = 0

    if currentToken().isInstanceOf[Token.RParen] then advance()
    else {
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
          DecodeError.ExpectedType(Schema.String.describeSelf, describe(other)).atToken(other.span)
        )

  private def decodeChar[A](wrap: Char => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.CharLit(value = value) =>
        advance()
        wrap(value)
      case other =>
        raise(
          DecodeError.ExpectedType(Schema.Char.describeSelf, describe(other)).atToken(other.span)
        )

  private def decodeInt[A](wrap: Int => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = {
        case Token.IntLit(value = value) => value
        case other                       =>
          raise(
            DecodeError.ExpectedType(Schema.Int.describeSelf, describe(other)).atToken(other.span)
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
            DecodeError.ExpectedType(Schema.Long.describeSelf, describe(other)).atToken(other.span)
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
            DecodeError.ExpectedType(Schema.Float.describeSelf, describe(other)).atToken(other.span)
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
              .ExpectedType(Schema.Double.describeSelf, describe(other))
              .atToken(other.span)
          )
      },
      negator = -1.0,
      one = 1.0,
      prod = _ * _,
      wrap = wrap
    )

  private inline def decodeSigned[T, A](
      inline literal: Resulting[Token => T, DecodeError],
      inline negator: T,
      inline one: T,
      inline prod: (T, T) => T,
      wrap: T => A
  ): Resulting[A, DecodeError] =
    val sign =
      currentToken() match
        case Token.Minus(_) =>
          advance()
          negator
        case _ =>
          one
    val t = literal(currentToken())
    advance()
    wrap(prod(sign, t))

  private def decodeBoolean[A](wrap: Boolean => A): Result[A, DecodeError] =
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
              .ExpectedType(Schema.Boolean.describeSelf, describe(other))
              .atToken(other.span)
          )

  private val AnyNullSchema: Schema.Nullary[Null]                    = Schema.Nullary[Null](null)
  private def decodeNull[A](wrap: Null => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.NullKw(_) =>
        advance()
        wrap(null)
      case other =>
        raise(
          DecodeError.ExpectedType(AnyNullSchema.describeSelf, describe(other)).atToken(other.span)
        )

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
