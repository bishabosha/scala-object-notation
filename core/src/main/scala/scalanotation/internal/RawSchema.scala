package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.internal.Internal
import steps.result.Result

import Result.eval.raise

/** Internal API of scalanotation that describes the structure of expected data, used to control how
  * to decode from either a token stream or an expression
  */
private[scalanotation] enum RawSchema:
  case NamedTuple(fields: IArray[RawSchema.Field])
  case Sum(cases: Map[String, RawSchema.SumCase])
  case Vector(element: RawSchema)
  case Dict(element: RawSchema)
  case AnyExpr
  case String
  case Char
  case Int
  case Long
  case Float
  case Double
  case Boolean
  case Nullary
  case Option(inner: RawSchema)

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
        case NamedTuple(fields) =>
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
      case NamedTuple(fields) =>
        if fields.isEmpty then "AnyNamedTuple"
        else fields.map(f => s"${f.name}: ...").mkString("(", ", ", ")")
      case Sum(cases) =>
        if cases.isEmpty then "AnyNamedTuple"
        else (cases.keysIterator.map(k => s"($k: ...)").mkString(" | "))
      case Vector(element) => s"Vector[...]"
      case Dict(element)   => "AnyNamedTuple"
      case AnyExpr         => "Any"
      case String          => "String"
      case Char            => "Char"
      case Int             => "Int"
      case Long            => "Long"
      case Float           => "Float"
      case Double          => "Double"
      case Boolean         => "Boolean"
      case Nullary         => "Null"
      case Option(inner)   =>
        inner match
          case RawSchema.Option(_) => "... | Null" // break recursion
          case other               => s"${other.describeSelf} | Null"

private[scalanotation] object RawSchema:
  final class Key[T]()
  val IsValidNamedTupleSchema: Key[Result[Unit, DecodeError]] = Key()

  final case class Field(name: String, schema: RawSchema)

  final case class SumCase(name: String, schema: RawSchema)
