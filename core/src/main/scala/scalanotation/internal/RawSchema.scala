package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Reader
import scalanotation.internal.Internal
import steps.result.Result

import scala.collection.mutable
import scala.reflect.ClassTag

import Result.eval.raise

/** Internal API of scalanotation that describes the structure of expected data, used to control how
  * to decode from either a token stream or an expression
  */
private[scalanotation] enum RawSchema:
  case NamedTuple[A](fields: IArray[RawSchema.Field[?]], build: Array[AnyRef] => A)
  case Sum(cases: Map[String, RawSchema.SumCase[?]])
  case Vector[Elem, Repr, A](
      element: Reader[Elem],
      builder: RawSchema.VectorBuilder[Elem, Repr, A]
  )
  case Dict[Elem, Repr, A](
      element: Reader[Elem],
      builder: RawSchema.DictBuilder[Elem, Repr, A]
  )
  case AnyExpr
  case String
  case Char
  case Int
  case Long
  case Float
  case Double
  case Boolean
  case Nullary[A](value: A)
  case Option[A](inner: Reader[A])

  private lazy val properties: java.util.concurrent.ConcurrentHashMap[RawSchema.Key[?], AnyRef] =
    java.util.concurrent.ConcurrentHashMap()

  private def getOrComputeProperty[T <: AnyRef](key: RawSchema.Key[T])(
      compute: => T
  ): T =
    properties.computeIfAbsent(key, _ => compute).asInstanceOf[T]

  def isValidNamedTuple[T: Internal.NameSet](
      pool: Internal.LocalPool[T]
  ): Result[Unit, DecodeError] =
    getOrComputeProperty(RawSchema.IsValidNamedTupleSchema) {
      validateNamedTuple(pool)
    }

  private def validateNamedTuple[T: Internal.NameSet](
      pool: Internal.LocalPool[T]
  ): Result[Unit, DecodeError] = pool.withBorrowed { seenNames =>
    Result:
      this match
        case NamedTuple(fields, _) =>
          val len = fields.length
          var i   = 0
          while i < len do
            val field                    = fields(i)
            val name                     = field.name
            def fmtErr(err: DecodeError) = err.atPath(s".${name}")
            if seenNames.alreadySeen(name) then
              raise(fmtErr(DecodeError.DuplicateSchemaField(name)))
            i += 1
        case _ => ()
  }

  final def describeSelf: String =
    // doesnt go deeper than one level of nesting.
    this match
      case NamedTuple(fields, build) =>
        if fields.isEmpty then "AnyNamedTuple"
        else fields.map(f => s"${f.name}: ...").mkString("(", ", ", ")")
      case Sum(cases) =>
        if cases.isEmpty then "AnyNamedTuple"
        else (cases.keysIterator.map(k => s"($k: ...)").mkString(" | "))
      case Vector(element, _) => s"Vector[...]"
      case Dict(element, _)   => "AnyNamedTuple"
      case AnyExpr            => "Any"
      case String             => "String"
      case Char               => "Char"
      case Int                => "Int"
      case Long               => "Long"
      case Float              => "Float"
      case Double             => "Double"
      case Boolean            => "Boolean"
      case Nullary(value)     => "Null"
      case Option(inner)      =>
        inner.schema match
          case RawSchema.Option(_) => "... | Null" // break recursion
          case other               => s"${other.describeSelf} | Null"

private[scalanotation] object RawSchema:
  final class Key[T]()
  val IsValidNamedTupleSchema: Key[Result[Unit, DecodeError]] = Key()

  trait VectorBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, elem: Elem): Repr
    def finish(repr: Repr): A

  trait DictBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, key: String, elem: Elem): Repr
    def finish(repr: Repr): A

  final case class Field[A](name: String, decoder: Reader[A]):
    def schema: RawSchema = decoder.schema

  final case class SumCase[A](name: String, decoder: Reader[A])
