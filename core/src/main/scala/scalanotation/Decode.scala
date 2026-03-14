package scalanotation

import steps.result.Result
import steps.result.Result.eval.break
import steps.result.Result.eval.ok
import steps.result.Result.eval.raise

import scala.annotation.constructorOnly
import scala.annotation.implicitNotFound
import scala.annotation.publicInBinary
import scala.collection.mutable
import scala.util.NotGiven
import scala.util.boundary

import compiletime.uninitialized
import NamedTuple.NamedTuple
import NamedTuple.AnyNamedTuple
import NamedTuple.NamedTuple as SNamedTuple
import TaggedSchema.Builders.AtPath
import TokenDecoder.describe
import scalanotation.TaggedSchema.MappedSchema

object DecodeError:
  private[scalanotation] given AdaptTokenError: Conversion[Tokenizer.TokenError, DecodeError]:
    def apply(err: Tokenizer.TokenError): DecodeError =
      DecodeError.UnexpectedToken(err)

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
  case FieldOrderMismatch(expected: String, actual: String)
  case MissingField(fieldName: String)
  case UnexpectedField(fieldName: String)
  case UnexpectedRoot(rootName: String)
  case DuplicateField(fieldName: String)
  case Custom(message: String)
  case AtPath(segment: String, cause: DecodeError)
  case AtToken(tokenSpan: Span, cause: DecodeError)

  def atPath(segment: String): DecodeError = DecodeError.AtPath(segment, this)
  def atToken(span: Span): DecodeError     = DecodeError.AtToken(span, this)

  def path: List[String] =
    this match
      case DecodeError.AtPath(segment, cause) => segment :: cause.path
      case DecodeError.AtToken(_, cause)      => cause.path
      case _                                  => Nil

  def span: Option[Span] =
    this match
      case DecodeError.AtToken(tokenSpan, _) => Some(tokenSpan)
      case DecodeError.AtPath(_, cause)      => cause.span
      case _                                 => None

  def rootCause: DecodeError =
    this match
      case DecodeError.AtPath(_, cause)  => cause.rootCause
      case DecodeError.AtToken(_, cause) => cause.rootCause
      case other                         => other

  def format: String =
    this match
      case DecodeError.UnexpectedToken(err) =>
        err.format
      case DecodeError.ExpectedNumber(found) =>
        s"Expected a number but found ${describe(found)}"
      case DecodeError.ExpectedExpression(found) =>
        s"Expected an expression but found ${describe(found)}"
      case DecodeError.ExpectedNamedTuple(found) =>
        s"Expected a named tuple but found ${describe(found)}"
      case DecodeError.ExpectedVector(found) =>
        s"Expected a vector but found ${describe(found)}"
      case DecodeError.ExpectedString(found) =>
        s"Expected a string constant but found ${describe(found)}"
      case DecodeError.ExpectedChar(found) =>
        s"Expected a char constant but found ${describe(found)}"
      case DecodeError.ExpectedInt(found) =>
        s"Expected an int constant but found ${describe(found)}"
      case DecodeError.ExpectedLong(found) =>
        s"Expected a long constant but found ${describe(found)}"
      case DecodeError.ExpectedFloat(found) =>
        s"Expected a float constant but found ${describe(found)}"
      case DecodeError.ExpectedDouble(found) =>
        s"Expected a double constant but found ${describe(found)}"
      case DecodeError.ExpectedBoolean(found) =>
        s"Expected a boolean constant but found ${describe(found)}"
      case DecodeError.ExpectedNull(found) =>
        s"Expected null but found ${describe(found)}"
      case DecodeError.UnitValueNotAllowed() => "Unit value '()' is not valid."
      case DecodeError.ExpectedEquals(found) =>
        s"Expected '=' but found ${describe(found)}"
      case DecodeError.ExpectedRParen(found) =>
        s"Expected ')' but found ${describe(found)}"
      case DecodeError.ExpectedFieldName(found) =>
        s"expected field name 'x = ' but found ${describe(found)}"
      case DecodeError.ExpectedVal(found) =>
        s"Expected 'val' but found ${describe(found)}"
      case DecodeError.ExpectedIdentifier(found) =>
        s"Expected an identifier but found ${describe(found)}"
      case DecodeError.ExpectedEof(found) =>
        s"Expected end of input but found ${describe(found)}"
      case DecodeError.FieldCountMismatch(expected, actual) =>
        s"Expected $expected fields but found $actual"
      case DecodeError.FieldOrderMismatch(expected, actual) =>
        s"Field was expected to be '$expected' but was '$actual'"
      case DecodeError.MissingField(fieldName) =>
        s"Missing required field '$fieldName'"
      case DecodeError.UnexpectedField(fieldName) =>
        s"Unexpected field '$fieldName'"
      case DecodeError.UnexpectedRoot(rootName) =>
        s"Unexpected root declaration '$rootName'"
      case DecodeError.DuplicateField(fieldName) =>
        s"Duplicate field '$fieldName'"
      case DecodeError.Custom(message) =>
        message
      case DecodeError.AtPath(segment, cause) =>
        def loop(cause: DecodeError, acc: List[String]): String =
          cause match
            case DecodeError.AtPath(segment, innerCause) =>
              loop(innerCause, segment :: acc)
            case DecodeError.AtToken(tokenSpan, innerCause) =>
              s"${tokenSpan.line}:${tokenSpan.column}: In path '${acc.reverseIterator.mkString}': ${innerCause.format}"
            case other =>
              s"In path '${acc.reverseIterator.mkString}': ${other.format}"

        loop(cause, List(segment))
      case DecodeError.AtToken(tokenSpan, cause) =>
        s"${tokenSpan.line}:${tokenSpan.column}: ${cause.format}"

enum Schema:
  case NamedTuple(fields: IArray[Schema.Field])
  case Vector(element: TaggedSchema[Any])
  case AnyExpr
  case String
  case Char
  case Int
  case Long
  case Float
  case Double
  case Boolean
  case Option(inner: TaggedSchema[Any])

  def validate(expr: Expr): Result[Checked[Any], DecodeError] = Result:
    this match
      case Schema.NamedTuple(fields) =>
        expr match
          case Expr.NamedTupleExpr(names, elements) =>
            if names.length != fields.length then
              raise(DecodeError.FieldCountMismatch(fields.length, names.length))

            var index  = 0
            val values = new Array[AnyRef](fields.length)
            while index < fields.length do
              val field     = fields(index)
              val fieldName = names(index)
              if fieldName != field.name then
                raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

              val value = field.decoder
                .checked(elements(index))
                .mapErr(_.atPath(s".${field.name}"))
                .ok
              values(index) = value.value.asInstanceOf[AnyRef]
              index += 1

            Checked(scala.NamedTuple.build()(Tuple.fromIArray(IArray.unsafeFromArray(values))))
          case other =>
            raise(DecodeError.ExpectedNamedTuple(other))

      case Schema.Vector(elementDecoder) =>
        expr match
          case Expr.VectorExpr(elements) =>
            val values = scala.Vector.newBuilder[Any]
            var index  = 0
            while index < elements.length do
              val value = elementDecoder
                .checked(elements(index))
                .mapErr(_.atPath(s"[$index]"))
                .ok
              values += value.value
              index += 1
            Checked(values.result())
          case other =>
            raise(DecodeError.ExpectedVector(other))

      case Schema.AnyExpr =>
        Checked(expr)

      case Schema.String =>
        expr match
          case Expr.StringConstant(value) => Checked(value)
          case other                      => raise(DecodeError.ExpectedString(other))

      case Schema.Char =>
        expr match
          case Expr.CharConstant(value) => Checked(value)
          case other                    => raise(DecodeError.ExpectedChar(other))

      case Schema.Int =>
        expr match
          case Expr.IntConstant(value) => Checked(value)
          case other                   => raise(DecodeError.ExpectedInt(other))

      case Schema.Long =>
        expr match
          case Expr.LongConstant(value) => Checked(value)
          case other                    => raise(DecodeError.ExpectedLong(other))

      case Schema.Float =>
        expr match
          case Expr.FloatConstant(value) => Checked(value)
          case other                     => raise(DecodeError.ExpectedFloat(other))

      case Schema.Double =>
        expr match
          case Expr.DoubleConstant(value) => Checked(value)
          case other                      => raise(DecodeError.ExpectedDouble(other))

      case Schema.Boolean =>
        expr match
          case Expr.BooleanConstant(value) => Checked(value)
          case other                       => raise(DecodeError.ExpectedBoolean(other))

      case Schema.Option(innerDecoder) =>
        expr match
          case Expr.NullConstant => Checked(None)
          case other             =>
            val value = innerDecoder.checked(other).ok
            Checked(Some(value.value))

object Schema:
  final case class Field(name: String, decoder: TaggedSchema[Any]):
    def schema: Schema = decoder.schema

opaque type Checked[+T] = T
object Checked:
  private[scalanotation] def apply[T](value: T): Checked[T] = value
  extension [T](value: Checked[T]) def value: T             = value

sealed trait TaggedSchema[T]:
  def schema: Schema

  // private[scalanotation] def decodeValidated(value: Checked[Any]): Result[Checked[T], DecodeError]

  final def checked(expr: Expr): Result[Checked[T], DecodeError] =
    TaggedSchema.finalize(this, schema.validate(expr))

  final def decode(expr: Expr): Result[T, DecodeError] =
    checked(expr).map(_.value)

  final def map[U](f: T => U): TaggedSchema[U] =
    TaggedSchema.from(this)(f)

  final def emap[U](f: T => Result[U, DecodeError]): TaggedSchema[U] =
    TaggedSchema.fromResult(this)(f)

object TaggedSchema {
  private[scalanotation] final def finalize[T](decoder: TaggedSchema[T], checked: Result[Checked[Any], DecodeError]): Result[Checked[T], DecodeError] =
    decoder match
      case mapped: MappedSchema[T] => checked.flatMap(mapped.decodeValidated)
      case _                       => checked.asInstanceOf[Result[Checked[T], DecodeError]]

  private[scalanotation] sealed trait MappedSchema[T] extends TaggedSchema[T]:
    private[scalanotation] def decodeValidated(value: Checked[Any]): Result[Checked[T], DecodeError]

  private def identity[T](schema0: Schema): TaggedSchema[T] =
    new TaggedSchema[T]:
      val schema: Schema = schema0

  def from[A, B](base: TaggedSchema[A])(transform: A => B): TaggedSchema[B] =
    fromResult(base)(value => Result.Ok(transform(value)))

  def from[A: TaggedSchema as base, B](transform: A => B): TaggedSchema[B] =
    from(base)(transform)

  def fromResult[A, B](base: TaggedSchema[A])(
      transform: A => Result[B, DecodeError]
  ): TaggedSchema[B] =
    new MappedSchema[B]:
      val schema: Schema = base.schema

      private[scalanotation] def decodeValidated(
          value: Checked[Any]
      ): Result[Checked[B], DecodeError] = Result:
        val raw: A = base match
          case mapped: MappedSchema[A] =>
            mapped.decodeValidated(value).ok.value
          case _ =>
            value.value.asInstanceOf[A]
        break(transform(raw).asInstanceOf[Result[Checked[B], DecodeError]])

  def fromResult[A: TaggedSchema as base, B](transform: A => Result[B, DecodeError]): TaggedSchema[B] =
    fromResult(base)(transform)

  given TaggedSchema[Expr] = identity(Schema.AnyExpr)

  given TaggedSchema[String] = identity(Schema.String)

  given TaggedSchema[Char] = identity(Schema.Char)

  given TaggedSchema[Int] = identity(Schema.Int)

  given TaggedSchema[Long] = identity(Schema.Long)

  given TaggedSchema[Float] = identity(Schema.Float)

  given TaggedSchema[Double] = identity(Schema.Double)

  given TaggedSchema[Boolean] = identity(Schema.Boolean)

  given OptionDecoder: [T] => (atPath: AtPath["", Option[T]]) => TaggedSchema[Option[T]] =
    atPath.decoder

  given VectorDecoder: [T] => (atPath: AtPath["", Vector[T]]) => TaggedSchema[Vector[T]] =
    atPath.decoder

  given NamedTupleDecoder: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: AtPath["", NT]) => TaggedSchema[NT] =
    atPath.decoder

  object Builders {
    import Internal.showType

    inline def formatPath[Path <: String]: String = ("'" + compiletime.constValue[Path] + "'")

    opaque type NonNestedOption[Path <: String, T] = Unit

    object NonNestedOption:
      inline given Default: [Path <: String, T] => NonNestedOption[Path, T] =
        compiletime.summonFrom {
          case _: (T <:< Option[?]) =>
            compiletime.error(
              "at path " + formatPath[Path] +
                ": TaggedSchema[Option[Option[?]]] is not supported."
            )
          case _ =>
            ()
        }

    opaque type AtPath[Path <: String, T] = TaggedSchema[T] | List[Schema.Field]

    object AtPath:
      import compiletime.ops.string.+

      extension [Path <: String, T](schema: AtPath[Path, T])
        def decoder: TaggedSchema[T] = schema match
          case d: TaggedSchema[?]      => d.asInstanceOf[TaggedSchema[T]]
          case ls: List[Schema.Field]  => identity(Schema.NamedTuple(IArray.from(ls)))

        def schema: Schema = decoder.schema

      inline given DefaultAtPath: [Path <: String, T] => AtPath[Path, T] =
        compiletime.summonFrom {
          case d: TaggedSchema[T] => d
          case _                  =>
            compiletime.error(
              "at path " + formatPath[Path] + ": Could not find TaggedSchema[" + showType[T] + "]."
            )
        }

      given VectorAtPath: [Path <: String, T]
        => (wrapped: AtPath[Path + "[]", T])
        => AtPath[Path, Vector[T]] =
        identity(Schema.Vector(wrapped.decoder.asInstanceOf[TaggedSchema[Any]]))

      given OptionAtPath: [Path <: String, T]
        => NonNestedOption[Path, T]
        => (wrapped: AtPath[Path, T])
        => AtPath[Path, Option[T]] =
        identity(Schema.Option(wrapped.decoder.asInstanceOf[TaggedSchema[Any]]))

      given NamedTupleCons: [Path <: String, N <: String, V, Ns <: Tuple, Vs <: Tuple]
        => (vn: ValueOf[N], ap: AtPath[Path + "." + N, V], rest: AtPath[Path, NamedTuple[Ns, Vs]])
        => AtPath[Path, NamedTuple[N *: Ns, V *: Vs]] =
        rest match
          case fs: List[Schema.Field] =>
            val decoder: TaggedSchema[V] = ap.decoder
            Schema.Field(vn.value, decoder.asInstanceOf[TaggedSchema[Any]]) :: fs
          case _ =>
            throw IllegalArgumentException(
              "Expected the rest of the named tuple to be a NamedTuple schema"
            )

      given NamedTupleEmpty: [Path <: String] => AtPath[Path, NamedTuple.Empty] =
        Nil
  }
}

@publicInBinary
private[scalanotation] object Internal {
  import quoted.{Expr as QExpr, *}

  import scala.util.boundary.Label
  inline def loop[A](inline body: Label[A] ?=> Unit): A = {
    boundary[A] {
      while true do body
      ???
    }
  }
  object loop {
    inline def break[A](a: A)(using Label[A]): A = boundary.break(a)
  }

  // TODO: add to standard library!
  inline def showType[T] = ${ showTypeImpl[T] }

  def showTypeImpl[T: Type](using Quotes): QExpr[String] =
    import quotes.reflect.*
    QExpr(Type.show[T])

  trait HasDefault[T] {
    def Default: T
  }

  class TokenStream[T: Internal.HasDefault as default](@constructorOnly tokens: List[T]) {
    private var curr: T       = uninitialized
    private var rest: List[T] = tokens
    advance() // initialize curr and rest

    protected def currentToken(): T = curr

    protected def peekToken(): T =
      rest match
        case t :: _ => t
        case _      => default.Default

    protected def advance(): Unit =
      rest match
        case curr1 :: rest1 =>
          curr = curr1
          rest = rest1
        case _ =>
          curr = default.Default
          rest = Nil
  }
}

private[scalanotation] object TokenDecoder:
  def decode[T](
      tokens: List[Token],
      rootName: String,
      decoder: TaggedSchema[T]
  ): Result[T, DecodeError] =
    TokenDecoder(tokens).decodeRoot(decoder, rootName)
  def decodeAnyRoot[T](
      tokens: List[Token],
      decoder: TaggedSchema[T]
  ): Result[SourceFile[T], DecodeError] =
    TokenDecoder(tokens).decodeAnyRoot(decoder)

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
      case Token.CharLit(raw, _, _)   => s"character literal $raw"
      case Token.Equals(_)            => "'='"
      case Token.Plus(_)              => "'+'"
      case Token.Minus(_)             => "'-'"
      case Token.Comma(_)             => "','"
      case Token.LParen(_)            => "'('"
      case Token.RParen(_)            => "')'"
      case Token.Eof(_)               => "end of input"

  private[scalanotation] def describe(expr: Expr): String =
    expr match
      case Expr.VectorExpr(elements)            => "vector"
      case Expr.NamedTupleExpr(names, elements) => "named tuple"
      case Expr.StringConstant(_)               => "string constant"
      case Expr.CharConstant(_)                 => "char constant"
      case Expr.IntConstant(_)                  => "int constant"
      case Expr.LongConstant(_)                 => "long constant"
      case Expr.FloatConstant(_)                => "float constant"
      case Expr.DoubleConstant(_)               => "double constant"
      case Expr.BooleanConstant(_)              => "boolean constant"
      case Expr.NullConstant                    => "null constant"

private final class TokenDecoder(@constructorOnly tokens: List[Token])
    extends Internal.TokenStream(tokens) {
  import scala.util.boundary.Label
  type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  // NOTE: currently as we make one decoder per token stream then there is no issue
  // of leaked shared state.
  // if we switch to a design based on a reusable decoder for multiple token streams
  // (e.g. batch processing), then either state explicit thread unsafe, or
  // we should switch to concurrent safe state.
  private var identCount                = 0
  private val perfectNamesCache         = mutable.HashMap[String, Int]()
  private def nameId(name: String): Int =
    perfectNamesCache.getOrElseUpdate(
      name, {
        try identCount
        finally identCount += 1
      }
    )
  private val namesRing = mutable.ArrayDeque.empty[SharedBitSet]
  class SharedBitSet(@constructorOnly elems0: Array[Long]) extends mutable.BitSet(elems0):
    val initialSize            = elems0.length
    def resized: Boolean       = initialSize != this.elems.length
    override def clear(): Unit = java.util.Arrays.fill(this.elems, 0L)
  private def borrowNames(): SharedBitSet =
    // non-atomic pull, but this should be single-threaded code
    if namesRing.isEmpty then
      // if identCount grows to >= 1024 then
      // SharedBitSet will resize on observing the 1024th identifier and not pool.
      // this is probably rare? unless we have an attacker.
      new SharedBitSet(new Array[Long](16))
    else
      val unsafeMask = namesRing.removeHead()
      unsafeMask.clear()
      unsafeMask
  private def releaseNames(bitmask: SharedBitSet): Unit =
    namesRing.append(bitmask)
  private val schemaStateCache = mutable.HashMap.empty[IArray[Schema.Field], Boolean]
  private def validateFields(
      fields: IArray[Schema.Field]
  ): Boolean =
    schemaStateCache.getOrElseUpdate(
      fields,
      withNames { seenNames =>
        boundary {
          var i   = 0
          val len = fields.length
          while i < len do
            val id = nameId(fields(i).name)
            if seenNames.contains(id) || { seenNames.addOne(id); false } then boundary.break(false)
            i += 1
          true
        }
      }
    )
  private def duplicateField(
      fields: IArray[Schema.Field]
  ): String =
    // called after validation fails so we can use a slower path
    fields
      .groupBy(_.name)
      .collectFirst {
        case (name, fieldGroup) if fieldGroup.length > 1 => name
      }
      .getOrElse("...")

  private inline def withNames[A](inline body: mutable.BitSet => A): A = {
    val unsafeMask = borrowNames()
    try body(unsafeMask)
    finally
      if !unsafeMask.resized then releaseNames(unsafeMask)
  }

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
      value.value

  def decodeAnyRoot[T](
      schema: TaggedSchema[T]
  ): Result[SourceFile[T], DecodeError] =
    Result:
      expectVal().ok
      val declaredName = expectIdentifier().ok
      expectEquals().ok
      val value = decodeTaggedAs(schema).ok
      expectEof().ok
      SourceFile(ValDecl(declaredName, value.value))

  private trait DecodingVisitor[A]:
    def onNamedTuple(fields: IArray[Schema.Field]): Result[A, DecodeError]
    def onVector(element: TaggedSchema[Any]): Result[A, DecodeError]
    def onOption(inner: TaggedSchema[Any]): Result[A, DecodeError]
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
        case Schema.Vector(element)    => onVector(element)
        case Schema.String             => onString()
        case Schema.Char               => onChar()
        case Schema.Int                => onInt()
        case Schema.Long               => onLong()
        case Schema.Float              => onFloat()
        case Schema.Double             => onDouble()
        case Schema.Boolean            => onBoolean()
        case Schema.Option(inner)      => onOption(inner)
        case Schema.AnyExpr            =>
          // possible to decode some parts to typed, and have a nested part that is Expr
          exprVisitor.inferExpr()

    def onNamedTuple(
        fields: IArray[Schema.Field]
    ): Result[Checked[AnyNamedTuple], DecodeError] = {
      Result {
        if !validateFields(fields) then
          def dupErr = DecodeError.DuplicateField(duplicateField(fields))
          raise(dupErr)
        val values = new Array[AnyRef](fields.length)

        // FIXME: need to lift out or else the boundary/break optimisation fails.
        val emptyFields = fields.isEmpty

        val parsed = parseNamedTupleStructure(allowEmpty = emptyFields) {
          (actualName, nameSpan, fieldIndex) =>
            if fieldIndex >= fields.length then
              def tooManyFieldsErr =
                DecodeError
                  .FieldCountMismatch(fields.length, fieldIndex + 1)
                  .atPath(s".${actualName}")
                  .atToken(nameSpan)
              raise(tooManyFieldsErr)
            else
              val expectedField = fields(fieldIndex)
              if actualName != expectedField.name then
                def wrongNameErr = DecodeError
                  .FieldOrderMismatch(expectedField.name, actualName)
                  .atPath(s".${actualName}")
                  .atToken(nameSpan)
                raise(wrongNameErr)
              else
                def decoded = decodeTaggedAs(expectedField.decoder)
                  .mapErr(_.atPath(s".${expectedField.name}"))
                val value = decoded.ok
                values(fieldIndex) = value.value.asInstanceOf[AnyRef]
        }
        if parsed.fieldCount != fields.length then
          def countErr = DecodeError
            .FieldCountMismatch(fields.length, parsed.fieldCount)
            .atToken(parsed.closingSpan)
          raise(countErr)
        else
          val asTuple = Tuple.fromIArray(IArray.unsafeFromArray(values))
          Checked(NamedTuple.build()(asTuple))
      }
    }

    def onVector(element: TaggedSchema[Any]): Result[Checked[Vector[Any]], DecodeError] =
      Result {
        val values = scala.Vector.newBuilder[Any]
        parseVectorStructure { indexInVector =>
          val value = decodeTaggedAs(element)
            .mapErr(_.atPath(s"[$indexInVector]"))
            .ok
          values += value.value
        }
        Checked(values.result())
      }

    def onOption(inner: TaggedSchema[Any]): Result[Checked[Option[Any]], DecodeError] =
      Result {
        currentToken() match
          case Token.NullKw(_) =>
            advance()
            Checked(None)
          case _ =>
            val value = decodeTaggedAs(inner).ok
            Checked(Some(value.value))
      }

    def onString(): Result[Checked[String], DecodeError] = decodeString(
      Checked(_)
    )
    def onChar(): Result[Checked[Char], DecodeError]     = decodeChar(Checked(_))
    def onInt(): Result[Checked[Int], DecodeError]       = decodeInt(Checked(_))
    def onLong(): Result[Checked[Long], DecodeError]     = decodeLong(Checked(_))
    def onFloat(): Result[Checked[Float], DecodeError]   = decodeFloat(Checked(_))
    def onDouble(): Result[Checked[Double], DecodeError] = decodeDouble(
      Checked(_)
    )
    def onBoolean(): Result[Checked[Boolean], DecodeError] = decodeBoolean(
      Checked(_)
    )
    def onNull(): Result[Checked[Null], DecodeError] = decodeNull(Checked(_))

  private object exprVisitor extends DecodingVisitor[Checked[Expr]]:
    private val AnyNamedTupleSchemaFields                     = IArray.empty[Schema.Field]
    final def inferExpr(): Result[Checked[Expr], DecodeError] =
      currentToken() match
        case Token.LParen(_)                    => onNamedTuple(AnyNamedTupleSchemaFields)
        case Token.VectorId(_)                  => onVector(summon[TaggedSchema[Expr]].asInstanceOf[TaggedSchema[Any]])
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
              Result.Err(DecodeError.ExpectedNumber(token).atToken(token.span))
        case other =>
          Result.Err(DecodeError.ExpectedExpression(other).atToken(other.span))

    def onNamedTuple(
        fields: IArray[Schema.Field]
    ): Result[Checked[Expr], DecodeError] = withNames { seenNames =>
      Result {
        val names    = IArray.newBuilder[String]
        val elements = IArray.newBuilder[Expr]
        val buf      = parseNamedTupleStructure(allowEmpty = false) { (name, nameSpan, _) =>
          val id            = nameId(name)
          def testDuplicate = seenNames.contains(id) || { seenNames.addOne(id); false }
          if testDuplicate then
            def dupErr = DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan)
            raise(dupErr)
          def tryExpr = inferExpr().mapErr(_.atPath(s".${name}"))
          val elem    = tryExpr.ok
          names += name
          elements += elem.value
        }
        val _ = buf.closingSpan // consume result
        val _ = buf.fieldCount  // consume result
        Checked(Expr.NamedTupleExpr(names.result(), elements.result()))
      }
    }

    def onVector(element: TaggedSchema[Any]): Result[Checked[Expr], DecodeError] = Result {
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure { _ =>
        elements += inferExpr().ok.value
      }
      Checked(Expr.VectorExpr(elements.result()))
    }

    def onOption(inner: TaggedSchema[Any]): Result[Checked[Expr], DecodeError] =
      inferExpr()

    def onString(): Result[Checked[Expr], DecodeError] =
      decodeString(s => Checked(Expr.StringConstant(s)))
    def onChar(): Result[Checked[Expr], DecodeError] =
      decodeChar(c => Checked(Expr.CharConstant(c)))
    def onInt(): Result[Checked[Expr], DecodeError] =
      decodeInt(i => Checked(Expr.IntConstant(i)))
    def onLong(): Result[Checked[Expr], DecodeError] =
      decodeLong(l => Checked(Expr.LongConstant(l)))
    def onFloat(): Result[Checked[Expr], DecodeError] =
      decodeFloat(f => Checked(Expr.FloatConstant(f)))
    def onDouble(): Result[Checked[Expr], DecodeError] =
      decodeDouble(d => Checked(Expr.DoubleConstant(d)))
    def onBoolean(): Result[Checked[Expr], DecodeError] =
      decodeBoolean(b => Checked(Expr.BooleanConstant(b)))
    def onNull(): Result[Checked[Expr], DecodeError] =
      decodeNull(_ => Checked(Expr.NullConstant))

  private def decodeTaggedAs[T](
      schema: TaggedSchema[T]
  ): Result[Checked[T], DecodeError] =
    val result = schema.schema match
      case Schema.AnyExpr => exprVisitor.inferExpr()
      case other          => checkedVisitor.decodeChecked(other)
    TaggedSchema.finalize(schema, result)

  private class NamedTupleParseResultBuf() {
    var fieldCount: Int   = uninitialized
    var closingSpan: Span = uninitialized
  }
  // have to be careful not to share this! currently we have single threaded decode.
  // just have to ensure that results are processed before calling parseNamedTupleStructure again.
  private object NamedTupleParseResultBuf extends NamedTupleParseResultBuf() {
    def push(fieldCount: Int, closingSpan: Span): this.type =
      this.fieldCount = fieldCount
      this.closingSpan = closingSpan
      this
  }
  private inline def parseNamedTupleStructure(
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Span, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResultBuf, DecodeError] = {
    import Internal.loop
    currentToken() match
      case Token.LParen(_) => advance()
      case other           =>
        raise(DecodeError.ExpectedNamedTuple(other).atToken(other.span))

    currentToken() match
      case token @ Token.RParen(_) =>
        advance()
        if allowEmpty then
          // TODO: add early ok return as well!
          NamedTupleParseResultBuf.push(0, token.span)
        else raise(DecodeError.UnitValueNotAllowed().atToken(token.span))
      case _ =>
        var fieldIndex           = 0
        val rparen: Token.RParen = loop {
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
                  raise(DecodeError.ExpectedEquals(other).atToken(other.span))

              consumeFieldValue(actualName, nameSpan, fieldIndex)
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
                  raise(DecodeError.ExpectedRParen(other).atToken(other.span))

            case other =>
              raise(DecodeError.ExpectedFieldName(other).atToken(other.span))
        }
        advance()
        NamedTupleParseResultBuf.push(fieldIndex, rparen.span)
  }

  private inline def parseVectorStructure(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) =>
        raise(DecodeError.ExpectedVector(other).atToken(other.span))

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
            raise(DecodeError.ExpectedRParen(other).atToken(other.span))

      currentToken() match
        case Token.RParen(_) =>
          advance()
        case other =>
          raise(DecodeError.ExpectedRParen(other).atToken(other.span))
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
        raise(DecodeError.ExpectedString(other).atToken(other.span))

  private def decodeChar[A](wrap: Char => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.CharLit(value = value) =>
        advance()
        wrap(value)
      case other =>
        raise(DecodeError.ExpectedChar(other).atToken(other.span))

  private def decodeInt[A](wrap: Int => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = {
        case Token.IntLit(value = value) => value
        case token => raise(DecodeError.ExpectedInt(token).atToken(token.span))
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
        case token => raise(DecodeError.ExpectedLong(token).atToken(token.span))
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
        case token => raise(DecodeError.ExpectedFloat(token).atToken(token.span))
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
        case token => raise(DecodeError.ExpectedDouble(token).atToken(token.span))
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
          raise(DecodeError.ExpectedBoolean(other).atToken(other.span))

  private def decodeNull[A](wrap: Null => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.NullKw(_) =>
        advance()
        wrap(null)
      case other =>
        raise(DecodeError.ExpectedNull(other).atToken(other.span))

  private def expectVal(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.ValKw(_) =>
        advance()
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
      case other =>
        raise(DecodeError.ExpectedEquals(other).atToken(other.span))

  private def expectEof(): Result[Unit, DecodeError] = Result:
    currentToken() match
      case Token.Eof(_) => ()
      case other        => raise(DecodeError.ExpectedEof(other).atToken(other.span))
}
