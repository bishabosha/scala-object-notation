package scalanotation

import scala.annotation.implicitNotFound
import scala.collection.mutable
import steps.result.Result
import steps.result.Result.eval.{ok, raise, break}

import NamedTuple.NamedTuple
import NamedTuple.AnyNamedTuple
import NamedTuple.NamedTuple as SNamedTuple
import TaggedSchema.Builders.AtPath
import SchemaTokenDecoder.describe

object DecodeError:
  given Conversion[Tokenizer.TokenError, DecodeError]:
    def apply(err: Tokenizer.TokenError): DecodeError = DecodeError.UnexpectedToken(err)

enum DecodeError:
  case UnexpectedToken(err: Tokenizer.TokenError)
  case ExpectedExpression(found: Token)
  case ExpectedNamedTuple(found: Expr | Token)
  case ExpectedVector(found: Expr | Token)
  case ExpectedString(found: Expr | Token)
  case ExpectedChar(found: Expr | Token)
  case ExpectedInt(found: Expr | Token)
  case ExpectedLong(found: Expr | Token)
  case ExpectedFloat(found: Expr | Token)
  case ExpectedDouble(found: Expr | Token)
  case ExpectedBoolean(found: Expr | Token)
  case ExpectedNumber(found: Expr | Token)
  case ExpectedNull(found: Expr | Token)
  case UnitValueNotAllowed()
  case ExpectedEquals(found: Token)
  case ExpectedRParen(found: Token)
  case ExpectedFieldName(found: Token)
  case ExpectedVal(found: Token)
  case ExpectedIdentifier(found: Token)
  case ExpectedEof(found: Token)
  case FieldCountMismatch(expected: Int, actual: Int)
  case FieldOrderMismatch(index: Int, expected: String, actual: String)
  case MissingField(fieldName: String)
  case UnexpectedField(fieldName: String)
  case UnexpectedRoot(rootName: String)
  case DuplicateField(fieldName: String)
  case AtPath(segment: String, cause: DecodeError)
  case AtToken(tokenSpan: Span, cause: DecodeError)

  def atPath(segment: String): DecodeError = DecodeError.AtPath(segment, this)
  def atToken(span: Span): DecodeError = DecodeError.AtToken(span, this)

  def path: List[String] =
    this match
      case DecodeError.AtPath(segment, cause) => segment :: cause.path
      case DecodeError.AtToken(_, cause) => cause.path
      case _ => Nil

  def span: Option[Span] =
    this match
      case DecodeError.AtToken(tokenSpan, _) => Some(tokenSpan)
      case DecodeError.AtPath(_, cause) => cause.span
      case _ => None

  def rootCause: DecodeError =
    this match
      case DecodeError.AtPath(_, cause) => cause.rootCause
      case DecodeError.AtToken(_, cause) => cause.rootCause
      case other => other

  def format: String =
    this match
      case DecodeError.UnexpectedToken(err) => s"Unexpected token: ${err.format}"
      case DecodeError.ExpectedNumber(found) => s"Expected a number but found ${describe(found)}"
      case DecodeError.ExpectedExpression(found) => s"Expected an expression but found ${describe(found)}"
      case DecodeError.ExpectedNamedTuple(found) => s"Expected a named tuple but found ${describe(found)}"
      case DecodeError.ExpectedVector(found) => s"Expected a vector but found ${describe(found)}"
      case DecodeError.ExpectedString(found) => s"Expected a string constant but found ${describe(found)}"
      case DecodeError.ExpectedChar(found) => s"Expected a char constant but found ${describe(found)}"
      case DecodeError.ExpectedInt(found) => s"Expected an int constant but found ${describe(found)}"
      case DecodeError.ExpectedLong(found) => s"Expected a long constant but found ${describe(found)}"
      case DecodeError.ExpectedFloat(found) => s"Expected a float constant but found ${describe(found)}"
      case DecodeError.ExpectedDouble(found) => s"Expected a double constant but found ${describe(found)}"
      case DecodeError.ExpectedBoolean(found) => s"Expected a boolean constant but found ${describe(found)}"
      case DecodeError.ExpectedNull(found) => s"Expected null but found ${describe(found)}"
      case DecodeError.UnitValueNotAllowed() => "Unit value '()' is not valid."
      case DecodeError.ExpectedEquals(found) => s"Expected '=' but found ${describe(found)}"
      case DecodeError.ExpectedRParen(found) => s"Expected ')' but found ${describe(found)}"
      case DecodeError.ExpectedFieldName(found) => s"expected field name 'x = ' but found ${describe(found)}"
      case DecodeError.ExpectedVal(found) => s"Expected 'val' but found ${describe(found)}"
      case DecodeError.ExpectedIdentifier(found) => s"Expected an identifier but found ${describe(found)}"
      case DecodeError.ExpectedEof(found) => s"Expected end of input but found ${describe(found)}"
      case DecodeError.FieldCountMismatch(expected, actual) => s"Expected $expected fields but found $actual"
      case DecodeError.FieldOrderMismatch(index, expected, actual) => s"Field #$index was expected to be '$expected' but was '$actual'"
      case DecodeError.MissingField(fieldName) => s"Missing required field '$fieldName'"
      case DecodeError.UnexpectedField(fieldName) => s"Unexpected field '$fieldName'"
      case DecodeError.UnexpectedRoot(rootName) => s"Unexpected root declaration '$rootName'"
      case DecodeError.DuplicateField(fieldName) => s"Duplicate field '$fieldName'"
      case DecodeError.AtPath(segment, cause) => s"In path '${segment}': ${cause.format}"
      case DecodeError.AtToken(tokenSpan, cause) => s"${cause.format} at ${tokenSpan.line}:${tokenSpan.column}"

enum Schema:
  case NamedTuple(fields: IArray[Schema.Field])
  case Vector(element: Schema)
  case AnyExpr
  case String
  case Char
  case Int
  case Long
  case Float
  case Double
  case Boolean
  case Null

  def validate(expr: Expr): Result[Checked[Any], DecodeError] =
    this match
      case Schema.NamedTuple(fields) =>
        expr match
          case Expr.NamedTupleExpr(names, elements) =>
            if names.length != fields.length then
              return Result.Err(DecodeError.FieldCountMismatch(fields.length, names.length))

            var index = 0
            val values = IArray.newBuilder[Any]
            while index < fields.length do
              val field = fields(index)
              val fieldName = names(index)
              if fieldName != field.name then
                return Result.Err(DecodeError.FieldOrderMismatch(index, field.name, fieldName))

              field.schema.validate(elements(index)) match
                case Result.Ok(value) => values += value.value
                case Result.Err(error) => return Result.Err(error.atPath(field.name))
              index += 1

            Result.Ok(Checked(Tuple.fromIArray(values.result())))
          case other =>
            Result.Err(DecodeError.ExpectedNamedTuple(other))

      case Schema.Vector(elementSchema) =>
        expr match
          case Expr.VectorExpr(elements) =>
            val values = scala.Vector.newBuilder[Any]
            var index = 0
            while index < elements.length do
              elementSchema.validate(elements(index)) match
                case Result.Ok(value) => values += value.value
                case Result.Err(error) => return Result.Err(error.atPath(s"[$index]"))
              index += 1
            Result.Ok(Checked(values.result()))
          case other =>
            Result.Err(DecodeError.ExpectedVector(other))

      case Schema.AnyExpr =>
        Result.Ok(Checked(expr))

      case Schema.String =>
        expr match
          case Expr.StringConstant(value) => Result.Ok(Checked(value))
          case other => Result.Err(DecodeError.ExpectedString(other))

      case Schema.Char =>
        expr match
          case Expr.CharConstant(value) => Result.Ok(Checked(value))
          case other => Result.Err(DecodeError.ExpectedChar(other))

      case Schema.Int =>
        expr match
          case Expr.IntConstant(value) => Result.Ok(Checked(value))
          case other => Result.Err(DecodeError.ExpectedInt(other))

      case Schema.Long =>
        expr match
          case Expr.LongConstant(value) => Result.Ok(Checked(value))
          case other => Result.Err(DecodeError.ExpectedLong(other))

      case Schema.Float =>
        expr match
          case Expr.FloatConstant(value) => Result.Ok(Checked(value))
          case other => Result.Err(DecodeError.ExpectedFloat(other))

      case Schema.Double =>
        expr match
          case Expr.DoubleConstant(value) => Result.Ok(Checked(value))
          case other => Result.Err(DecodeError.ExpectedDouble(other))

      case Schema.Boolean =>
        expr match
          case Expr.BooleanConstant(value) => Result.Ok(Checked(value))
          case other => Result.Err(DecodeError.ExpectedBoolean(other))

      case Schema.Null =>
        expr match
          case Expr.NullConstant => Result.Ok(Checked(null))
          case other => Result.Err(DecodeError.ExpectedNull(other))

object Schema:
  final case class Field(name: String, schema: Schema)

opaque type Checked[+T] = T
object Checked:
  private[scalanotation] def apply[T](value: T): Checked[T] = value
  extension [T](value: Checked[T]) def value: T = value

trait TaggedSchema[T]:
  def schema: Schema

  final def checked(expr: Expr): Result[Checked[T], DecodeError] =
    schema.validate(expr).map(_.asInstanceOf[Checked[T]])

  final def decode(expr: Expr): Result[T, DecodeError] =
    schema.validate(expr).map(_.value.asInstanceOf[T])

object TaggedSchema:
  given TaggedSchema[Expr]:
    val schema: Schema = Schema.AnyExpr

  given TaggedSchema[String]:
    val schema: Schema = Schema.String

  given TaggedSchema[Char]:
    val schema: Schema = Schema.Char

  given TaggedSchema[Int]:
    val schema: Schema = Schema.Int

  given TaggedSchema[Long]:
    val schema: Schema = Schema.Long

  given TaggedSchema[Float]:
    val schema: Schema = Schema.Float

  given TaggedSchema[Double]:
    val schema: Schema = Schema.Double

  given TaggedSchema[Boolean]:
    val schema: Schema = Schema.Boolean

  given TaggedSchema[Null]:
    val schema: Schema = Schema.Null

  given VectorDecoder: [T] => (atPath: AtPath["", Vector[T]]) => TaggedSchema[Vector[T]]:
    val schema = atPath.schema

  given NamedTupleDecoder: [NT <: NamedTuple.AnyNamedTuple] => (atPath: AtPath["", NT]) => TaggedSchema[NT]:
    val schema = atPath.schema

  object Builders:
    import Helpers.showType

    opaque type AtPath[Path <: String, T] = Schema | List[Schema.Field]

    object AtPath:
      import compiletime.ops.string.+

      extension [Path <: String, T](schema: AtPath[Path, T])
        def schema: Schema = schema match
          case s: Schema => s
          case ls: List[Schema.Field] => Schema.NamedTuple(IArray.from(ls))

      inline given atPath[Path <: String, T]: AtPath[Path, T] =
        compiletime.summonFrom {
          case d: TaggedSchema[T] => d.schema
          case _ => compiletime.error("at path '" + compiletime.constValue[Path] + "': Could not find TaggedSchema[" + showType[T] + "].")
        }

      given [Path <: String, T](using wrapped: AtPath[Path + "[]", T]): AtPath[Path, Vector[T]] =
        Schema.Vector(wrapped.schema)

      given [Path <: String, N <: String, V, Ns <: Tuple, Vs <: Tuple](using
          vn: ValueOf[N],
          ap: AtPath[Path + "." + N, V],
          rest: AtPath[Path, NamedTuple[Ns, Vs]]
      ): AtPath[Path, NamedTuple[N *: Ns, V *: Vs]] =
        rest match
          case fs: List[Schema.Field] =>
            Schema.Field(vn.value, ap.schema) :: fs
          case _ =>
            throw IllegalArgumentException("Expected the rest of the named tuple to be a NamedTuple schema")

      given [Path <: String]: AtPath[Path, NamedTuple.Empty] =
        Nil

  object Helpers:
    import quoted.{Expr as QExpr, *}
    inline def showType[T] = ${showTypeImpl[T]}
    def showTypeImpl[T: Type](using Quotes): QExpr[String] =
      import quotes.reflect.*
      QExpr(Type.show[T])

extension (expr: Expr)
  def decodeAs[T](using decoder: TaggedSchema[T]): Result[T, DecodeError] =
    decoder.decode(expr)

  def checkedAs[T](using decoder: TaggedSchema[T]): Result[Checked[T], DecodeError] =
    decoder.checked(expr)

extension (sourceFile: SourceFile[Expr])
  def decodeValueAs[T](name: String)(using decoder: TaggedSchema[T]): Result[T, DecodeError] =
    if sourceFile.declaration.name != name then
      Result.Err(DecodeError.UnexpectedRoot(sourceFile.declaration.name))
    else
      sourceFile.declaration.value.decodeAs[T]

private[scalanotation] object SchemaTokenDecoder:
  def decode[T](tokens: IArray[Token], rootName: String, decoder: TaggedSchema[T]): Result[T, DecodeError] =
    val parser = new SchemaTokenDecoder(tokens)
    parser.decodeRoot(decoder, rootName).map(_.value)
  def decodeAnyRoot[T](tokens: IArray[Token], decoder: TaggedSchema[T]): Result[SourceFile[T], DecodeError] =
    val parser = new SchemaTokenDecoder(tokens)
    parser.decodeAnyRoot(decoder)

  private[scalanotation] def describe(token: Token | Expr): String =
    token match
      case t: Token => describe(t)
      case e: Expr => describe(e)

  private[scalanotation] def describe(token: Token): String =
    token match
      case Token.ValKw(_) => "'val'"
      case Token.VectorId(_) => "'Vector'"
      case Token.TrueKw(_) => "'true'"
      case Token.FalseKw(_) => "'false'"
      case Token.NullKw(_) => "'null'"
      case Token.Identifier(name, _) => s"identifier '$name'"
      case Token.IntLit(raw, _, _) => s"integer literal '$raw'"
      case Token.LongLit(raw, _, _) => s"long literal '$raw'"
      case Token.FloatLit(raw, _, _) => s"float literal '$raw'"
      case Token.DoubleLit(raw, _, _) => s"double literal '$raw'"
      case Token.StringLit(raw, _, _) => s"string literal $raw"
      case Token.CharLit(raw, _, _) => s"character literal $raw"
      case Token.Equals(_) => "'='"
      case Token.Plus(_) => "'+'"
      case Token.Minus(_) => "'-'"
      case Token.Comma(_) => "','"
      case Token.LParen(_) => "'('"
      case Token.RParen(_) => "')'"
      case Token.Eof(_) => "end of input"

  private[scalanotation] def describe(expr: Expr): String =
    expr match
      case Expr.VectorExpr(elements) => "vector"
      case Expr.NamedTupleExpr(names, elements) => "named tuple"
      case Expr.StringConstant(_) => "string constant"
      case Expr.CharConstant(_) => "char constant"
      case Expr.IntConstant(_) => "int constant"
      case Expr.LongConstant(_) => "long constant"
      case Expr.FloatConstant(_) => "float constant"
      case Expr.DoubleConstant(_) => "double constant"
      case Expr.BooleanConstant(_) => "boolean constant"
      case Expr.NullConstant => "null constant"

private final class SchemaTokenDecoder(tokens: IArray[Token]):
  private var index = 0

  private final case class NamedTupleParseResult(fieldCount: Int, closingSpan: Span)

  private trait DecodingVisitor[A]:
    def onNamedTuple(fields: IArray[Schema.Field]): Result[A, DecodeError]
    def onVector(element: Schema): Result[A, DecodeError]
    def onString(): Result[A, DecodeError]
    def onChar(): Result[A, DecodeError]
    def onInt(): Result[A, DecodeError]
    def onLong(): Result[A, DecodeError]
    def onFloat(): Result[A, DecodeError]
    def onDouble(): Result[A, DecodeError]
    def onBoolean(): Result[A, DecodeError]
    def onNull(): Result[A, DecodeError]

  private object checkedVisitor extends DecodingVisitor[Checked[Any]]:
    def decodeChecked(schema: Schema): Result[Checked[Any], DecodeError] =
      schema match
        case Schema.NamedTuple(fields) => onNamedTuple(fields)
        case Schema.Vector(element) => onVector(element)
        case Schema.String => onString()
        case Schema.Char => onChar()
        case Schema.Int => onInt()
        case Schema.Long => onLong()
        case Schema.Float => onFloat()
        case Schema.Double => onDouble()
        case Schema.Boolean => onBoolean()
        case Schema.Null => onNull()
        case Schema.AnyExpr =>
           // possible to decode some parts to typed, and have a nested part that is Expr
           exprVisitor.inferExpr()

    def onNamedTuple(fields: IArray[Schema.Field]): Result[Checked[AnyNamedTuple], DecodeError] = Result {
      val values = new Array[AnyRef](fields.length)
      val parsed = parseNamedTupleStructure(allowEmpty = fields.isEmpty) { (actualName, nameSpan, fieldIndex) =>
        Result {
          if fieldIndex >= fields.length then
            raise(DecodeError.FieldCountMismatch(fields.length, fieldIndex + 1).atToken(nameSpan))
          else
            val expectedField = fields(fieldIndex)
            if actualName != expectedField.name then
              raise(DecodeError.FieldOrderMismatch(fieldIndex, expectedField.name, actualName).atToken(nameSpan))
            else
              val value = decodeChecked(expectedField.schema)
                .mapErr(_.atPath(expectedField.name))
                .ok
              values(fieldIndex) = value.value.asInstanceOf[AnyRef]
        }
      }.ok
      if parsed.fieldCount != fields.length then
        raise(DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount).atToken(parsed.closingSpan))
      else
        val finalValues = IArray.unsafeFromArray(values)
        Checked(NamedTuple(Tuple.fromIArray(finalValues)))
    }

    def onVector(element: Schema): Result[Checked[Vector[Any]], DecodeError] =
      Result {
        val values = scala.Vector.newBuilder[Any]
        parseVectorStructure { indexInVector =>
          Result {
            val value = decodeChecked(element)
              .mapErr(_.atPath(s"[$indexInVector]"))
              .ok
            values += value.value
          }
        }.ok
        Checked(values.result())
      }

    def onString(): Result[Checked[String], DecodeError] = decodeString(Checked(_))
    def onChar(): Result[Checked[Char], DecodeError] = decodeChar(Checked(_))
    def onInt(): Result[Checked[Int], DecodeError] = decodeInt(Checked(_))
    def onLong(): Result[Checked[Long], DecodeError] = decodeLong(Checked(_))
    def onFloat(): Result[Checked[Float], DecodeError] = decodeFloat(Checked(_))
    def onDouble(): Result[Checked[Double], DecodeError] = decodeDouble(Checked(_))
    def onBoolean(): Result[Checked[Boolean], DecodeError] = decodeBoolean(Checked(_))
    def onNull(): Result[Checked[Null], DecodeError] = decodeNull(Checked(_))

  private object exprVisitor extends DecodingVisitor[Checked[Expr]]:
    private val AnyNamedTupleSchemaFields = IArray.empty[Schema.Field]
    final def inferExpr(): Result[Checked[Expr], DecodeError] =
      currentToken() match
        case Token.LParen(_) => onNamedTuple(AnyNamedTupleSchemaFields)
        case Token.VectorId(_) => onVector(Schema.AnyExpr)
        case Token.StringLit(_, _, _) => onString()
        case Token.CharLit(_, _, _) => onChar()
        case Token.IntLit(_, _, _) => onInt()
        case Token.LongLit(_, _, _) => onLong()
        case Token.FloatLit(_, _, _) => onFloat()
        case Token.DoubleLit(_, _, _) => onDouble()
        case Token.TrueKw(_) | Token.FalseKw(_) => onBoolean()
        case Token.NullKw(_) => onNull()
        case Token.Minus(_) =>
          peekToken() match
            case Token.IntLit(_, _, _) => onInt()
            case Token.LongLit(_, _, _) => onLong()
            case Token.FloatLit(_, _, _) => onFloat()
            case Token.DoubleLit(_, _, _) => onDouble()
            case token =>
              Result.Err(DecodeError.ExpectedNumber(token).atToken(token.span))
        case other =>
          Result.Err(DecodeError.ExpectedExpression(other).atToken(other.span))

    def onNamedTuple(fields: IArray[Schema.Field]): Result[Checked[Expr], DecodeError] = Result:
      val names = IArray.newBuilder[String]
      val elements = IArray.newBuilder[Expr]
      parseNamedTupleStructure(allowEmpty = false) { (name, _, _) =>
        Result:
          val elem = inferExpr().ok
          names += name
          elements += elem.value
      }.ok
      Checked(Expr.NamedTupleExpr(names.result(), elements.result()))

    def onVector(element: Schema): Result[Checked[Expr], DecodeError] = Result:
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure { _ =>
        Result:
          elements += inferExpr().ok.value
      }.ok
      Checked(Expr.VectorExpr(elements.result()))

    def onString(): Result[Checked[Expr], DecodeError] = decodeString(s => Checked(Expr.StringConstant(s)))
    def onChar(): Result[Checked[Expr], DecodeError] = decodeChar(c => Checked(Expr.CharConstant(c)))
    def onInt(): Result[Checked[Expr], DecodeError] = decodeInt(i => Checked(Expr.IntConstant(i)))
    def onLong(): Result[Checked[Expr], DecodeError] = decodeLong(l => Checked(Expr.LongConstant(l)))
    def onFloat(): Result[Checked[Expr], DecodeError] = decodeFloat(f => Checked(Expr.FloatConstant(f)))
    def onDouble(): Result[Checked[Expr], DecodeError] = decodeDouble(d => Checked(Expr.DoubleConstant(d)))
    def onBoolean(): Result[Checked[Expr], DecodeError] = decodeBoolean(b => Checked(Expr.BooleanConstant(b)))
    def onNull(): Result[Checked[Expr], DecodeError] = decodeNull(_ => Checked(Expr.NullConstant))

  def decodeRoot[T](schema: TaggedSchema[T], rootName: String): Result[Checked[T], DecodeError] =
    Result:
      expectVal().ok
      val declaredName = expectIdentifier().ok
      if declaredName != rootName then raise(DecodeError.UnexpectedRoot(declaredName))
      expectEquals().ok
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      value

  def decodeAnyRoot[T](schema: TaggedSchema[T]): Result[SourceFile[T], DecodeError] =
    Result:
      expectVal().ok
      val declaredName = expectIdentifier().ok
      expectEquals().ok
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      SourceFile(ValDecl(declaredName, value.value))

  private def decodeTaggedAs[T](schema: TaggedSchema[T]): Result[Checked[T], DecodeError] =
    val result = schema.schema match
      case Schema.AnyExpr => exprVisitor.inferExpr()
      case other => checkedVisitor.decodeChecked(other)
    result.map(_.asInstanceOf[Checked[T]])

  private def parseNamedTupleStructure(allowEmpty: Boolean)(
      consumeFieldValue: (String, Span, Int) => Result[Unit, DecodeError]
  ): Result[NamedTupleParseResult, DecodeError] = Result {
    currentToken() match
      case Token.LParen(_) => advance()
      case other =>
        raise(DecodeError.ExpectedNamedTuple(other).atToken(other.span))

    currentToken() match
      case token @ Token.RParen(_) =>
        advance()
        if allowEmpty then
          // TODO: add early ok return as well!
          break(Result.Ok(NamedTupleParseResult(0, token.span)))
        else
          raise(DecodeError.UnitValueNotAllowed().atToken(token.span))
      case _ =>

    var fieldIndex = 0
    var done = false

    while !done do
      if currentToken().isInstanceOf[Token.RParen] then
        done = true
      else
        currentToken() match
          case Token.Identifier(actualName, nameSpan) =>
            advance()
            currentToken() match
              case Token.Equals(_) => advance()
              case other =>
                raise(DecodeError.ExpectedEquals(other).atToken(other.span))

            consumeFieldValue(actualName, nameSpan, fieldIndex).ok
            fieldIndex += 1

            currentToken() match
              case Token.Comma(_) =>
                advance()
                if currentToken().isInstanceOf[Token.RParen] then done = true
              case Token.RParen(_) => done = true
              case other =>
                raise(DecodeError.ExpectedRParen(other).atToken(other.span))

          case other =>
            raise(DecodeError.ExpectedFieldName(other).atToken(other.span))

    currentToken() match
      case Token.RParen(rParenSpan) =>
        advance()
        NamedTupleParseResult(fieldIndex, rParenSpan)
      case other =>
        raise(DecodeError.ExpectedRParen(other).atToken(other.span))
  }

  private def parseVectorStructure(
      consumeElementValue: Int => Result[Unit, DecodeError]
  ): Result[Unit, DecodeError] = Result {
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) =>
        raise(DecodeError.ExpectedVector(other).atToken(other.span))

    var indexInVector = 0

    if currentToken().isInstanceOf[Token.RParen] then
      advance()
      // TODO: add early ok return as well!
      break(Result.Ok(()))

    var done = false
    while !done do
      consumeElementValue(indexInVector).ok
      indexInVector += 1

      currentToken() match
        case Token.Comma(_) =>
          advance()
          if currentToken().isInstanceOf[Token.RParen] then done = true
        case Token.RParen(_) => done = true
        case other =>
          raise(DecodeError.ExpectedRParen(other).atToken(other.span))

    currentToken() match
      case Token.RParen(_) =>
        advance()
        Result.Ok(())
      case other =>
        raise(DecodeError.ExpectedRParen(other).atToken(other.span))
  }

  private def decodeString[A](wrap: String => A): Result[A, DecodeError] = Result {
    val first = decodeStringAtom().ok
    var isPlus = currentToken().isInstanceOf[Token.Plus]
    if !isPlus then
      wrap(first)
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
      case Token.StringLit(_, value, _) =>
        advance()
        value
      case other =>
        raise(DecodeError.ExpectedString(other).atToken(other.span))

  private def decodeChar[A](wrap: Char => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.CharLit(_, value, _) =>
        advance()
        wrap(value)
      case other =>
        raise(DecodeError.ExpectedChar(other).atToken(other.span))

  private def decodeInt[A](wrap: Int => A): Result[A, DecodeError] =
    decodeSigned(
      literal = {
        case Token.IntLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedInt(token).atToken(token.span),
      wrap = wrap
    )

  private def decodeLong[A](wrap: Long => A): Result[A, DecodeError] =
    decodeSigned(
      literal = {
        case Token.LongLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedLong(token).atToken(token.span),
      wrap = wrap
    )

  private def decodeFloat[A](wrap: Float => A): Result[A, DecodeError] =
    decodeSigned(
      literal = {
        case Token.FloatLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedFloat(token).atToken(token.span),
      wrap = wrap
    )

  private def decodeDouble[A](wrap: Double => A): Result[A, DecodeError] =
    decodeSigned(
      literal = {
        case Token.DoubleLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedDouble(token).atToken(token.span),
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
        raise(DecodeError.ExpectedBoolean(other).atToken(other.span))

  private def decodeNull[A](wrap: Null => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.NullKw(_) =>
        advance()
        wrap(null)
      case other =>
        raise(DecodeError.ExpectedNull(other).atToken(other.span))

  private val PFTombStone: PartialFunction[Any, Any] = {
    case _ => PFTombStone
  }
  private def decodeSigned[T, A](
      literal: PartialFunction[Token, T],
      expected: Token => DecodeError,
      wrap: T => A
  )(using num: Numeric[T]): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.Minus(_) =>
        advance()
        val token = currentToken()
        literal.applyOrElse(token, PFTombStone) match
          case PFTombStone => raise(expected(token))
          case t: T @unchecked =>
            advance()
            wrap(num.negate(t))
      case token =>
        literal.applyOrElse(token, PFTombStone) match
          case PFTombStone => raise(expected(token))
          case t: T @unchecked =>
            advance()
            wrap(t)

  private def expectVal(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.ValKw(_) =>
        advance()
        ()
      case other =>
        raise(DecodeError.ExpectedVal(other).atToken(other.span))

  private def expectIdentifier(): Result[String, DecodeError] = Result:
    currentToken() match
      case Token.Identifier(name, _) =>
        advance()
        name
      case other =>
        raise(DecodeError.ExpectedIdentifier(other).atToken(other.span))

  private def expectEquals(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.Equals(_) =>
        advance()
        ()
      case other =>
        raise(DecodeError.ExpectedEquals(other).atToken(other.span))

  private def expectEof(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.Eof(_) => ()
      case other => raise(DecodeError.ExpectedEof(other).atToken(other.span))

  private def currentToken(): Token = tokens(index)

  private def peekToken(): Token =
    if index + 1 < tokens.length then tokens(index + 1)
    else tokens.last

  private def advance(): Token =
    val token = tokens(index)
    if index < tokens.length - 1 then index += 1
    token
