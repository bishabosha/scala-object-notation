package scalanotation

import scala.annotation.implicitNotFound
import scala.collection.mutable

import NamedTuple.NamedTuple
import NamedTuple.AnyNamedTuple
import NamedTuple.NamedTuple as SNamedTuple
import TaggedSchema.Builders.AtPath

enum DecodeError:
  case ExpectedNamedTuple(found: Expr)
  case ExpectedVector(found: Expr)
  case ExpectedString(found: Expr)
  case ExpectedChar(found: Expr)
  case ExpectedInt(found: Expr)
  case ExpectedLong(found: Expr)
  case ExpectedFloat(found: Expr)
  case ExpectedDouble(found: Expr)
  case ExpectedBoolean(found: Expr)
  case ExpectedNull(found: Expr)
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

  final private[scalanotation] def decodeTokens(tokens: IArray[Token], rootName: String): Either[DecodeError, T] =
    SchemaTokenDecoder.decode(tokens, rootName, this)

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

extension (sourceFile: SourceFile)
  def decodeValueAs[T](name: String)(using decoder: TaggedSchema[T]): Either[DecodeError, T] =
    if sourceFile.declaration.name != name then
      Left(DecodeError.UnexpectedRoot(sourceFile.declaration.name))
    else
      sourceFile.declaration.value.decodeAs[T]

private[scalanotation] object SchemaTokenDecoder:
  def decode[T](tokens: IArray[Token], rootName: String, decoder: TaggedSchema[T]): Either[DecodeError, T] =
    val parser = new SchemaTokenDecoder(tokens)
    parser.decodeRoot(decoder.schema, rootName).map(_.value.asInstanceOf[T])
  def decodeAnyRoot(tokens: IArray[Token]): Either[DecodeError, SourceFile] =
    val parser = new SchemaTokenDecoder(tokens)
    parser.decodeAnyRoot()

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

  def decodeRoot(schema: Schema, rootName: String): Either[DecodeError, Checked[Any]] =
    expectVal()
    val declaredName = expectIdentifier()
    if declaredName != rootName then
      return Left(DecodeError.UnexpectedRoot(declaredName))
    expectEquals()
    for
      value <- decodeChecked(schema)
      _ <- expectEof()
    yield value

  def decodeAnyRoot(): Either[DecodeError, SourceFile] =
    expectVal()
    val declaredName = expectIdentifier()
    expectEquals()
    for
      value <- decodeAnyExpr()
      _ <- expectEof()
    yield SourceFile(ValDecl(declaredName, value.value))

  private def decodeChecked(schema: Schema): Either[DecodeError, Checked[Any]] =
    schema match
      case Schema.AnyExpr => decodeAnyExpr()
      case other => decodeWithVisitor(other, checkedVisitor)

  private def decodeAnyExpr(): Either[DecodeError, Checked[Expr]] =
      decodeWithVisitor[Checked[Expr]](inferAnyExprSchema(), exprVisitor)

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

  private val AnyNamedTupleSchema = Schema.NamedTuple(IArray.empty)
  private val AnyVectorSchema = Schema.Vector(Schema.AnyExpr)
  private def inferAnyExprSchema(): Schema =
    currentToken() match
      case Token.LParen(_) => AnyNamedTupleSchema
      case Token.VectorId(_) => AnyVectorSchema
      case Token.StringLit(_, _, _) => Schema.String
      case Token.CharLit(_, _, _) => Schema.Char
      case Token.IntLit(_, _, _) => Schema.Int
      case Token.LongLit(_, _, _) => Schema.Long
      case Token.FloatLit(_, _, _) => Schema.Float
      case Token.DoubleLit(_, _, _) => Schema.Double
      case Token.TrueKw(_) | Token.FalseKw(_) => Schema.Boolean
      case Token.NullKw(_) => Schema.Null
      case Token.Minus(_) =>
        peekToken() match
          case Token.IntLit(_, _, _) => Schema.Int
          case Token.LongLit(_, _, _) => Schema.Long
          case Token.FloatLit(_, _, _) => Schema.Float
          case Token.DoubleLit(_, _, _) => Schema.Double
          case _ =>
            val minus = currentToken().span
            throw ParseException(s"A minus sign must be followed by a numeric literal at ${minus.line}:${minus.column}")
      case other =>
        throw ParseException(s"Expected an expression but found ${describe(other)} at ${other.span.line}:${other.span.column}")

  private def parseNamedTupleStructure(allowEmpty: Boolean)(
      consumeFieldValue: (String, Span, Int) => Either[DecodeError, Unit]
  ): Either[DecodeError, NamedTupleParseResult] =
    currentToken() match
      case Token.LParen(_) => advance()
      case other =>
        return Left(DecodeError.ExpectedNamedTuple(tokenAsExpr(other)).atToken(other.span))

    currentToken() match
      case token @ Token.RParen(_) =>
        advance()
        if allowEmpty then
          return Right(NamedTupleParseResult(0, token.span))
        else
          throw ParseException(s"Unit value '()' is not valid. at ${token.span.line}:${token.span.column}")
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
                throw ParseException(s"Expected '=' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

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
                throw ParseException(s"Expected ')' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

          case other =>
            throw ParseException(s"expected field name 'x = ' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

    currentToken() match
      case Token.RParen(rParenSpan) =>
        advance()
        Right(NamedTupleParseResult(fieldIndex, rParenSpan))
      case other =>
        throw ParseException(s"Expected ')' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

  private def parseVectorStructure(
      consumeElementValue: Int => Either[DecodeError, Unit]
  ): Either[DecodeError, Unit] =
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) =>
        return Left(DecodeError.ExpectedVector(tokenAsExpr(other)).atToken(other.span))

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
          throw ParseException(s"Expected ')' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

    currentToken() match
      case Token.RParen(_) =>
        advance()
        Right(())
      case other =>
        throw ParseException(s"Expected ')' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

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
      case other => Left(DecodeError.ExpectedString(tokenAsExpr(other)).atToken(other.span))

  private def decodeChar[A](wrap: Char => A): Either[DecodeError, A] =
    currentToken() match
      case Token.CharLit(_, value, _) => advance(); Right(wrap(value))
      case other => Left(DecodeError.ExpectedChar(tokenAsExpr(other)).atToken(other.span))

  private def decodeInt[A](wrap: Int => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.IntLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedInt(tokenAsExpr(token)).atToken(token.span),
      wrap = wrap
    )

  private def decodeLong[A](wrap: Long => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.LongLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedLong(tokenAsExpr(token)).atToken(token.span),
      wrap = wrap
    )

  private def decodeFloat[A](wrap: Float => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.FloatLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedFloat(tokenAsExpr(token)).atToken(token.span),
      wrap = wrap
    )

  private def decodeDouble[A](wrap: Double => A): Either[DecodeError, A] =
    decodeSigned(
      literal = {
        case Token.DoubleLit(_, value, _) => value
      },
      expected = token => DecodeError.ExpectedDouble(tokenAsExpr(token)).atToken(token.span),
      wrap = wrap
    )

  private def decodeBoolean[A](wrap: Boolean => A): Either[DecodeError, A] =
    currentToken() match
      case Token.TrueKw(_) => advance(); Right(wrap(true))
      case Token.FalseKw(_) => advance(); Right(wrap(false))
      case other => Left(DecodeError.ExpectedBoolean(tokenAsExpr(other)).atToken(other.span))

  private def decodeNull[A](wrap: Null => A): Either[DecodeError, A] =
    currentToken() match
      case Token.NullKw(_) => advance(); Right(wrap(null))
      case other => Left(DecodeError.ExpectedNull(tokenAsExpr(other)).atToken(other.span))

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

  private def expectVal(): Unit =
    currentToken() match
      case Token.ValKw(_) => advance()
      case other => throw ParseException(s"Expected 'val' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

  private def expectIdentifier(): String =
    currentToken() match
      case Token.Identifier(name, _) => advance(); name
      case other => throw ParseException(s"Expected an identifier but found ${describe(other)} at ${other.span.line}:${other.span.column}")

  private def expectEquals(): Unit =
    currentToken() match
      case Token.Equals(_) => advance()
      case other => throw ParseException(s"Expected '=' but found ${describe(other)} at ${other.span.line}:${other.span.column}")

  private def expectEof(): Either[DecodeError, Unit] =
    currentToken() match
      case Token.Eof(_) => Right(())
      case other => throw ParseException(s"Expected end of input but found ${describe(other)} at ${other.span.line}:${other.span.column}")

  private def currentToken(): Token = tokens(index)

  private def peekToken(): Token =
    if index + 1 < tokens.length then tokens(index + 1)
    else tokens.last

  private def advance(): Token =
    val token = tokens(index)
    if index < tokens.length - 1 then index += 1
    token

  private def tokenAsExpr(token: Token): Expr =
    token match
      case Token.StringLit(_, value, _) => Expr.StringConstant(value)
      case Token.CharLit(_, value, _) => Expr.CharConstant(value)
      case Token.IntLit(_, value, _) => Expr.IntConstant(value)
      case Token.LongLit(_, value, _) => Expr.LongConstant(value)
      case Token.FloatLit(_, value, _) => Expr.FloatConstant(value)
      case Token.DoubleLit(_, value, _) => Expr.DoubleConstant(value)
      case Token.TrueKw(_) => Expr.BooleanConstant(true)
      case Token.FalseKw(_) => Expr.BooleanConstant(false)
      case Token.NullKw(_) => Expr.NullConstant
      case _ => Expr.NullConstant

  private def describe(token: Token): String =
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
