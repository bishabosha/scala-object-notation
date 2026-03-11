package miniparser

import scala.annotation.implicitNotFound
import scala.collection.mutable

import NamedTuple.NamedTuple
import NamedTuple.NamedTuple as SNamedTuple
import AstDecoder.Builders.AtPath

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

  def atPath(segment: String): DecodeError = DecodeError.AtPath(segment, this)

  def path: List[String] =
    this match
      case DecodeError.AtPath(segment, cause) => segment :: cause.path
      case _ => Nil

  def rootCause: DecodeError =
    this match
      case DecodeError.AtPath(_, cause) => cause.rootCause
      case other => other

enum Schema:
  case NamedTuple(fields: IArray[Schema.Field])
  case Vector(element: Schema)
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
                case Right(value) => values += Checked.unwrap(value)
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
                case Right(value) => values += Checked.unwrap(value)
                case Left(error) => return Left(error.atPath(s"[$index]"))
              index += 1
            Right(Checked(values.result()))
          case other =>
            Left(DecodeError.ExpectedVector(other))

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

opaque type Checked[+T] = Any
object Checked:
  private[miniparser] def apply[T](value: Any): Checked[T] = value
  private[miniparser] def unwrap[T](value: Checked[T]): Any = value
  extension [T](value: Checked[T]) def value: T = value.asInstanceOf[T]

trait AstDecoder[T]:
  def schema: Schema

  final def checked(expr: Expr): Either[DecodeError, Checked[T]] =
    schema.validate(expr).map(_.asInstanceOf[Checked[T]])

  final def decode(expr: Expr): Either[DecodeError, T] =
    schema.validate(expr).map(_.value.asInstanceOf[T])

object AstDecoder:
  given AstDecoder[String]:
    val schema: Schema = Schema.String

  given AstDecoder[Char]:
    val schema: Schema = Schema.Char

  given AstDecoder[Int]:
    val schema: Schema = Schema.Int

  given AstDecoder[Long]:
    val schema: Schema = Schema.Long

  given AstDecoder[Float]:
    val schema: Schema = Schema.Float

  given AstDecoder[Double]:
    val schema: Schema = Schema.Double

  given AstDecoder[Boolean]:
    val schema: Schema = Schema.Boolean

  given AstDecoder[Null]:
    val schema: Schema = Schema.Null

  given VectorDecoder: [T] => (atPath: AtPath["", Vector[T]]) => AstDecoder[Vector[T]]:
    val schema = atPath.schema

  given NamedTupleDecoder: [NT <: NamedTuple.AnyNamedTuple] => (atPath: AtPath["", NT]) => AstDecoder[NT]:
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
          case d: AstDecoder[T] => d.schema
          case _ => compiletime.error("at path '" + compiletime.constValue[Path] + "': Could not find AstDecoder[" + showType[T] + "].")
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
  def decodeAs[T](using decoder: AstDecoder[T]): Either[DecodeError, T] =
    decoder.decode(expr)

  def checkedAs[T](using decoder: AstDecoder[T]): Either[DecodeError, Checked[T]] =
    decoder.checked(expr)

extension (sourceFile: SourceFile)
  def decodeValueAs[T](name: String)(using decoder: AstDecoder[T]): Either[DecodeError, T] =
    if sourceFile.declaration.name != name then
      Left(DecodeError.UnexpectedRoot(sourceFile.declaration.name))
    else
      sourceFile.declaration.value.decodeAs[T]
