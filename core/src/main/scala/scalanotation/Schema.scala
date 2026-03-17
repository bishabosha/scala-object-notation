package scalanotation

import scala.reflect.ClassTag

/** Internal API of scalanotation that describes the structure of expected data, used to control how
  * to decode from either a token stream or an expression
  */
private[scalanotation] enum Schema:
  case NamedTuple[A](fields: IArray[Schema.Field[?]], build: Array[AnyRef] => A)
  case Sum(cases: Map[String, Schema.SumCase[?]])
  case Vector[Elem, Repr, A](
      element: TaggedSchema[Elem],
      builder: Schema.VectorBuilder[Elem, Repr, A]
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
  case Option[A](inner: TaggedSchema[A])

  private[scalanotation] final def describeSelf: String =
    // doesnt go deeper than one level of nesting.
    this match
      case NamedTuple(fields, build) =>
        if fields.isEmpty then "AnyNamedTuple"
        else fields.map(f => s"${f.name}: ...").mkString("(", ", ", ")")
      case Sum(cases) =>
        if cases.isEmpty then "AnyNamedTuple"
        else (cases.keysIterator.map(k => s"($k: ...)").mkString(" | "))
      case Vector(element, _) => s"Vector[...]"
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
          case Schema.Option(_) => "... | Null" // break recursion
          case other            => s"${other.describeSelf} | Null"

private[scalanotation] object Schema:
  private[scalanotation] trait VectorBuilder[Elem, Repr, A]:
    def init(): Repr
    def add(repr: Repr, elem: Elem): Repr
    def finish(repr: Repr): A

  final case class Field[A](name: String, decoder: TaggedSchema[A]):
    def schema: Schema = decoder.schema

  final case class SumCase[A](name: String, decoder: TaggedSchema[A])
