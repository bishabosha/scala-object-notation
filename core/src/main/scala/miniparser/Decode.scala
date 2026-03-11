package scalanotation

import scala.annotation.implicitNotFound
import scala.collection.mutable

import NamedTuple.NamedTuple
import NamedTuple.AnyNamedTuple
import NamedTuple.NamedTuple as SNamedTuple
import TaggedSchema.Builders.AtPath
import SchemaTokenDecoder.describe

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

  def validate(expr: Expr): Either[DecodeError, Checked[Any]] =
    this match
      case Schema.NamedTuple(fields) =>
        expr match
          case Expr.NamedTupleExpr(names, elements) =>
            if names.length != fields.length then
              return Left(DecodeError.FieldCountMismatch(fields.length, names.length))

            var index = 0
            val values = IArray.newBuilder[Any]
            while index < fields.length do
              val field = fields(index)
              val fieldName = names(index)
              if fieldName != field.name then
                return Left(DecodeError.FieldOrderMismatch(index, field.name, fieldName))

              field.schema.validate(elements(index)) match
                case Right(value) => values += value.value
                case Left(error) => return Left(error.atPath(field.name))
              index += 1

            Right(Checked(Tuple.fromIArray(values.result())))
          case other =>
            Left(DecodeError.ExpectedNamedTuple(other))

      case Schema.Vector(elementSchema) =>
        expr match
          case Expr.VectorExpr(elements) =>
            val values = scala.Vector.newBuilder[Any]
            var index = 0
            while index < elements.length do
              elementSchema.validate(elements(index)) match
                case Right(value) => values += value.value
                case Left(error) => return Left(error.atPath(s"[$index]"))
              index += 1
            Right(Checked(values.result()))
          case other =>
            Left(DecodeError.ExpectedVector(other))

      case Schema.AnyExpr =>
        Right(Checked(expr))

      case Schema.String =>
        expr match
          case Expr.StringConstant(value) => Right(Checked(value))
          case other => Left(DecodeError.ExpectedString(other))

      case Schema.Char =>
        expr match
          case Expr.CharConstant(value) => Right(Checked(value))
          case other => Left(DecodeError.ExpectedChar(other))

      case Schema.Int =>
        expr match
          case Expr.IntConstant(value) => Right(Checked(value))
          case other => Left(DecodeError.ExpectedInt(other))

      case Schema.Long =>
        expr match
          case Expr.LongConstant(value) => Right(Checked(value))
          case other => Left(DecodeError.ExpectedLong(other))

      case Schema.Float =>
        expr match
          case Expr.FloatConstant(value) => Right(Checked(value))
          case other => Left(DecodeError.ExpectedFloat(other))

      case Schema.Double =>
        expr match
          case Expr.DoubleConstant(value) => Right(Checked(value))
          case other => Left(DecodeError.ExpectedDouble(other))

      case Schema.Boolean =>
        expr match
          case Expr.BooleanConstant(value) => Right(Checked(value))
          case other => Left(DecodeError.ExpectedBoolean(other))

      case Schema.Null =>
        expr match
          case Expr.NullConstant => Right(Checked(null))
          case other => Left(DecodeError.ExpectedNull(other))

object Schema:
  final case class Field(name: String, schema: Schema)

opaque type Checked[+T] = T
object Checked:
  private[scalanotation] def apply[T](value: T): Checked[T] = value
  extension [T](value: Checked[T]) def value: T = value

trait TaggedSchema[T]:
  def schema: Schema

  final def checked(expr: Expr): Either[DecodeError, Checked[T]] =
    schema.validate(expr).map(_.asInstanceOf[Checked[T]])

  final def decode(expr: Expr): Either[DecodeError, T] =
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
  def decodeAs[T](using decoder: TaggedSchema[T]): Either[DecodeError, T] =
    decoder.decode(expr)

  def checkedAs[T](using decoder: TaggedSchema[T]): Either[DecodeError, Checked[T]] =
    decoder.checked(expr)

extension (sourceFile: SourceFile[Expr])
  def decodeValueAs[T](name: String)(using decoder: TaggedSchema[T]): Either[DecodeError, T] =
    if sourceFile.declaration.name != name then
      Left(DecodeError.UnexpectedRoot(sourceFile.declaration.name))
    else
      sourceFile.declaration.value.decodeAs[T]

private[scalanotation] object SchemaTokenDecoder:
  def decode[T](tokens: IArray[Token], rootName: String, decoder: TaggedSchema[T]): Either[DecodeError, T] =
    val parser = new SchemaTokenDecoder(tokens)
    parser.decodeRoot(decoder, rootName).map(_.value)
  def decodeAnyRoot[T](tokens: IArray[Token], decoder: TaggedSchema[T]): Either[DecodeError, SourceFile[T]] =
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
    def onNamedTuple(fields: IArray[Schema.Field]): Either[DecodeError, A]
    def onVector(element: Schema): Either[DecodeError, A]
    def onString(): Either[DecodeError, A]
    def onChar(): Either[DecodeError, A]
    def onInt(): Either[DecodeError, A]
    def onLong(): Either[DecodeError, A]
    def onFloat(): Either[DecodeError, A]
    def onDouble(): Either[DecodeError, A]
    def onBoolean(): Either[DecodeError, A]
    def onNull(): Either[DecodeError, A]

  private object checkedVisitor extends DecodingVisitor[Checked[Any]]:
    def onNamedTuple(fields: IArray[Schema.Field]): Either[DecodeError, Checked[AnyNamedTuple]] =
      val values = IArray.newBuilder[Any]
      parseNamedTupleStructure(allowEmpty = fields.isEmpty) { (actualName, nameSpan, fieldIndex) =>
        if fieldIndex >= fields.length then
          Left(DecodeError.FieldCountMismatch(fields.length, fieldIndex + 1).atToken(nameSpan))
        else
          val expectedField = fields(fieldIndex)
          if actualName != expectedField.name then
            Left(DecodeError.FieldOrderMismatch(fieldIndex, expectedField.name, actualName).atToken(nameSpan))
          else
            decodeChecked(expectedField.schema) match
              case Right(value) =>
                values += value.value
                Right(())
              case Left(error) => Left(error.atPath(expectedField.name))
      }.flatMap { parsed =>
        if parsed.fieldCount != fields.length then
          Left(DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount).atToken(parsed.closingSpan))
        else
          Right(Checked(
            NamedTuple.build[EmptyTuple]()(Tuple.fromIArray(values.result()))
          ))
      }

    def onVector(element: Schema): Either[DecodeError, Checked[Vector[Any]]] =
      val values = scala.Vector.newBuilder[Any]
      parseVectorStructure { indexInVector =>
        decodeChecked(element) match
          case Right(value) =>
            values += value.value
            Right(())
          case Left(error) => Left(error.atPath(s"[$indexInVector]"))
      }.map(_ => Checked(values.result()))

    def onString(): Either[DecodeError, Checked[String]] = decodeString(Checked(_))
    def onChar(): Either[DecodeError, Checked[Char]] = decodeChar(Checked(_))
    def onInt(): Either[DecodeError, Checked[Int]] = decodeInt(Checked(_))
    def onLong(): Either[DecodeError, Checked[Long]] = decodeLong(Checked(_))
    def onFloat(): Either[DecodeError, Checked[Float]] = decodeFloat(Checked(_))
    def onDouble(): Either[DecodeError, Checked[Double]] = decodeDouble(Checked(_))
    def onBoolean(): Either[DecodeError, Checked[Boolean]] = decodeBoolean(Checked(_))
    def onNull(): Either[DecodeError, Checked[Null]] = decodeNull(Checked(_))

  private object exprVisitor extends DecodingVisitor[Checked[Expr]]:
    private val AnyNamedTupleSchemaFields = IArray.empty[Schema.Field]
    final def inferExpr(): Either[DecodeError, Checked[Expr]] =
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
              Left(DecodeError.ExpectedNumber(token).atToken(token.span))
        case other =>
          Left(DecodeError.ExpectedExpression(other).atToken(other.span))

    def onNamedTuple(fields: IArray[Schema.Field]): Either[DecodeError, Checked[Expr]] =
      val names = IArray.newBuilder[String]
      val elements = IArray.newBuilder[Expr]
      parseNamedTupleStructure(allowEmpty = false) { (name, _, _) =>
        decodeAnyExpr() match
          case Right(elem) =>
            names += name
            elements += elem.value
            Right(())
          case Left(error) => Left(error)
      }.map(_ => Checked(Expr.NamedTupleExpr(names.result(), elements.result())))

    def onVector(element: Schema): Either[DecodeError, Checked[Expr]] =
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure { _ =>
        decodeAnyExpr() match
          case Right(elem) =>
            elements += elem.value
            Right(())
          case Left(error) => Left(error)
      }.map(_ => Checked(Expr.VectorExpr(elements.result())))

    def onString(): Either[DecodeError, Checked[Expr]] = decodeString(s => Checked(Expr.StringConstant(s)))
    def onChar(): Either[DecodeError, Checked[Expr]] = decodeChar(c => Checked(Expr.CharConstant(c)))
    def onInt(): Either[DecodeError, Checked[Expr]] = decodeInt(i => Checked(Expr.IntConstant(i)))
    def onLong(): Either[DecodeError, Checked[Expr]] = decodeLong(l => Checked(Expr.LongConstant(l)))
    def onFloat(): Either[DecodeError, Checked[Expr]] = decodeFloat(f => Checked(Expr.FloatConstant(f)))
    def onDouble(): Either[DecodeError, Checked[Expr]] = decodeDouble(d => Checked(Expr.DoubleConstant(d)))
    def onBoolean(): Either[DecodeError, Checked[Expr]] = decodeBoolean(b => Checked(Expr.BooleanConstant(b)))
    def onNull(): Either[DecodeError, Checked[Expr]] = decodeNull(_ => Checked(Expr.NullConstant))

  def decodeRoot[T](schema: TaggedSchema[T], rootName: String): Either[DecodeError, Checked[T]] =
    for
      _ <- expectVal()
      declaredName <- expectIdentifier()
      _ <-
        if declaredName != rootName then Left(DecodeError.UnexpectedRoot(declaredName))
        else Right(())
      _ <- expectEquals()
      value <- decodeCheckedAs(schema)
      _ <- expectEof()
    yield value

  def decodeAnyRoot[T](schema: TaggedSchema[T]): Either[DecodeError, SourceFile[T]] =
    for
      _ <- expectVal()
      declaredName <- expectIdentifier()
      _ <- expectEquals()
      value <- decodeCheckedAs(schema)
      _ <- expectEof()
    yield SourceFile(ValDecl(declaredName, value.value))

  private def decodeCheckedAs[T](schema: TaggedSchema[T]): Either[DecodeError, Checked[T]] =
    decodeChecked(schema.schema).map(_.asInstanceOf[Checked[T]])

  private def decodeChecked(schema: Schema): Either[DecodeError, Checked[Any]] =
    schema match
      case Schema.AnyExpr => decodeAnyExpr()
      case other => decodeWithVisitor(other, checkedVisitor)

  private def decodeWithVisitor[A](schema: Schema, visitor: DecodingVisitor[A]): Either[DecodeError, A] =
    schema match
      case Schema.NamedTuple(fields) => visitor.onNamedTuple(fields)
      case Schema.Vector(element) => visitor.onVector(element)
      case Schema.AnyExpr =>
        throw IllegalStateException("Schema.AnyExpr must be handled by decodeChecked/decodeAnyExpr")
      case Schema.String => visitor.onString()
      case Schema.Char => visitor.onChar()
      case Schema.Int => visitor.onInt()
      case Schema.Long => visitor.onLong()
      case Schema.Float => visitor.onFloat()
      case Schema.Double => visitor.onDouble()
      case Schema.Boolean => visitor.onBoolean()
      case Schema.Null => visitor.onNull()

  private def decodeAnyExpr(): Either[DecodeError, Checked[Expr]] =
    exprVisitor.inferExpr()

  private def parseNamedTupleStructure(allowEmpty: Boolean)(
      consumeFieldValue: (String, Span, Int) => Either[DecodeError, Unit]
  ): Either[DecodeError, NamedTupleParseResult] =
    currentToken() match
      case Token.LParen(_) => advance()
      case other =>
        return Left(DecodeError.ExpectedNamedTuple(other).atToken(other.span))

    currentToken() match
      case token @ Token.RParen(_) =>
        advance()
        if allowEmpty then
          return Right(NamedTupleParseResult(0, token.span))
        else
          return Left(DecodeError.UnitValueNotAllowed().atToken(token.span))
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
                return Left(DecodeError.ExpectedEquals(other).atToken(other.span))

            consumeFieldValue(actualName, nameSpan, fieldIndex) match
              case Right(_) => ()
              case Left(error) => return Left(error)
            fieldIndex += 1

            currentToken() match
              case Token.Comma(_) =>
                advance()
                if currentToken().isInstanceOf[Token.RParen] then done = true
              case Token.RParen(_) => done = true
              case other =>
                return Left(DecodeError.ExpectedRParen(other).atToken(other.span))

          case other =>
            return Left(DecodeError.ExpectedFieldName(other).atToken(other.span))

    currentToken() match
      case Token.RParen(rParenSpan) =>
        advance()
        Right(NamedTupleParseResult(fieldIndex, rParenSpan))
      case other =>
        Left(DecodeError.ExpectedRParen(other).atToken(other.span))

  private def parseVectorStructure(
      consumeElementValue: Int => Either[DecodeError, Unit]
  ): Either[DecodeError, Unit] =
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) =>
        return Left(DecodeError.ExpectedVector(other).atToken(other.span))

    var indexInVector = 0

    if currentToken().isInstanceOf[Token.RParen] then
      advance()
      return Right(())

    var done = false
    while !done do
      consumeElementValue(indexInVector) match
        case Right(_) => ()
        case Left(error) => return Left(error)
      indexInVector += 1

      currentToken() match
        case Token.Comma(_) =>
          advance()
          if currentToken().isInstanceOf[Token.RParen] then done = true
        case Token.RParen(_) => done = true
        case other =>
          return Left(DecodeError.ExpectedRParen(other).atToken(other.span))

    currentToken() match
      case Token.RParen(_) =>
        advance()
        Right(())
      case other =>
        Left(DecodeError.ExpectedRParen(other).atToken(other.span))

  private def decodeString[A](wrap: String => A): Either[DecodeError, A] =
    decodeStringAtom() match
      case Left(error) => Left(error)
      case Right(first) =>
        val builder = StringBuilder() ++= first
        while currentToken().isInstanceOf[Token.Plus] do
          advance()
          decodeStringAtom() match
            case Right(next) => builder ++= next
            case Left(error) => return Left(error)
        Right(wrap(builder.toString()))

  private def decodeStringAtom(): Either[DecodeError, String] =
    currentToken() match
      case Token.StringLit(_, value, _) =>
        advance()
        Right(value)
      case other => Left(DecodeError.ExpectedString(other).atToken(other.span))

  private def decodeChar[A](wrap: Char => A): Either[DecodeError, A] =
    currentToken() match
      case Token.CharLit(_, value, _) => advance(); Right(wrap(value))
      case other => Left(DecodeError.ExpectedChar(other).atToken(other.span))

  private def decodeInt[A](wrap: Int => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.IntLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedInt(token).atToken(token.span),
      wrap = wrap
    )

  private def decodeLong[A](wrap: Long => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.LongLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedLong(token).atToken(token.span),
      wrap = wrap
    )

  private def decodeFloat[A](wrap: Float => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.FloatLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedFloat(token).atToken(token.span),
      wrap = wrap
    )

  private def decodeDouble[A](wrap: Double => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.DoubleLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedDouble(token).atToken(token.span),
      wrap = wrap
    )

  private def decodeBoolean[A](wrap: Boolean => A): Either[DecodeError, A] =
    currentToken() match
      case Token.TrueKw(_) => advance(); Right(wrap(true))
      case Token.FalseKw(_) => advance(); Right(wrap(false))
      case other => Left(DecodeError.ExpectedBoolean(other).atToken(other.span))

  private def decodeNull[A](wrap: Null => A): Either[DecodeError, A] =
    currentToken() match
      case Token.NullKw(_) => advance(); Right(wrap(null))
      case other => Left(DecodeError.ExpectedNull(other).atToken(other.span))

  private val PFTombStone: PartialFunction[Any, Any] = {
    case _ => PFTombStone
  }
  private def decodeSigned[T, A](
      literal: PartialFunction[Token, T],
      expected: Token => DecodeError,
      wrap: T => A
  )(using num: Numeric[T]): Either[DecodeError, A] =
    currentToken() match
      case Token.Minus(_) =>
        advance()
        val token = currentToken()
        literal.applyOrElse(token, PFTombStone) match
          case PFTombStone => Left(expected(token))
          case t: T @unchecked =>
            advance()
            Right(wrap(num.negate(t)))
      case token =>
        literal.applyOrElse(token, PFTombStone) match
          case PFTombStone => Left(expected(token))
          case t: T @unchecked =>
            advance()
            Right(wrap(t))

  private def expectVal(): Either[DecodeError, Unit] =
    currentToken() match
      case Token.ValKw(_) => advance(); Right(())
      case other => Left(DecodeError.ExpectedVal(other).atToken(other.span))

  private def expectIdentifier(): Either[DecodeError, String] =
    currentToken() match
      case Token.Identifier(name, _) => advance(); Right(name)
      case other => Left(DecodeError.ExpectedIdentifier(other).atToken(other.span))

  private def expectEquals(): Either[DecodeError, Unit] =
    currentToken() match
      case Token.Equals(_) => advance(); Right(())
      case other => Left(DecodeError.ExpectedEquals(other).atToken(other.span))

  private def expectEof(): Either[DecodeError, Unit] =
    currentToken() match
      case Token.Eof(_) => Right(())
      case other => Left(DecodeError.ExpectedEof(other).atToken(other.span))

  private def currentToken(): Token = tokens(index)

  private def peekToken(): Token =
    if index + 1 < tokens.length then tokens(index + 1)
    else tokens.last

  private def advance(): Token =
    val token = tokens(index)
    if index < tokens.length - 1 then index += 1
    token
