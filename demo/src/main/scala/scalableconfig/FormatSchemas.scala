package scalableconfig

import org.virtuslab.yaml.ConstructError
import org.virtuslab.yaml.LoadSettings
import org.virtuslab.yaml.Node
import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.YamlDecoder
import org.virtuslab.yaml.YamlEncoder
import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader as SonReader
import scalanotation.ReadWriter as SonReadWriter
import scalanotation.RouterSchema
import scalanotation.Writer as SonWriter
import scalanotation.Writers
import steps.result.Result
import ujson.Arr
import ujson.Bool
import ujson.Null
import ujson.Num
import ujson.Obj
import ujson.Str
import ujson.Value

import scala.collection.immutable.ListMap
import scala.collection.mutable

object FormatSchemas:
  private type JsonObjectState = mutable.LinkedHashMap[String, Value]
  private type JsonArrayState  = mutable.ArrayBuffer[Value]
  private type YamlMapState    = mutable.Builder[(Node, Node), ListMap[Node, Node]]
  private type YamlSeqState    = mutable.Builder[Node, Vector[Node]]

  private object JsonObjectBuilder extends SonReader.DictBuilder[Value, JsonObjectState, Value]:
    def init(): JsonObjectState =
      mutable.LinkedHashMap.empty[String, Value]

    def add(repr: JsonObjectState, key: String, elem: Value): JsonObjectState =
      repr.addOne(key -> elem)

    def finish(repr: JsonObjectState): Value =
      Obj.from(repr)

  private object JsonArrayBuilder extends SonReader.VectorBuilder[Value, JsonArrayState, Value]:
    def init(): JsonArrayState =
      mutable.ArrayBuffer.empty[Value]

    def add(repr: JsonArrayState, elem: Value): JsonArrayState =
      repr.addOne(elem)

    def finish(repr: JsonArrayState): Value =
      Arr.from(repr)

  private object JsonSelect extends RouterSchema.Write[Value]:
    def caseIndex(value: Value): Int =
      value match
        case _: Obj                                                                        => 0
        case _: Arr                                                                        => 2
        case _: Str                                                                        => 3
        case Num(value) if value.isWhole && value >= Int.MinValue && value <= Int.MaxValue => 5
        case _: Num                                                                        => 8
        case _: Bool                                                                       => 9
        case Null                                                                          => 10

  given jsonValueReadWriter: SonReadWriter[Value] =
    SonReadWriter.router[Value]("ujson.Value", "JSON value")(
      cases = self =>
        List(
          RouterSchema.RouterConstruct.Record -> RouterSchema.Case(
            "Object",
            SonReadWriter.dict[Value, Value, JsonObjectState](
              self,
              JsonObjectBuilder,
              {
                case obj: Obj => obj.value.size
                case other    => invalidJson("Object", other)
              },
              {
                case obj: Obj => obj.value.iterator
                case other    => invalidJson("Object", other)
              }
            )
          ),
          RouterSchema.RouterConstruct.Tuple -> RouterSchema.Case(
            "TupleArray",
            SonReadWriter.tupleOf[Value, Value, JsonArrayState](
              self,
              JsonArrayBuilder,
              {
                case arr: Arr => arr.value.length
                case other    => invalidJson("Array", other)
              },
              {
                case arr: Arr => arr.value.iterator
                case other    => invalidJson("Array", other)
              }
            )
          ),
          RouterSchema.RouterConstruct.Vector -> RouterSchema.Case(
            "Array",
            SonReadWriter.vector[Value, Value, JsonArrayState](
              self,
              JsonArrayBuilder,
              {
                case arr: Arr => arr.value.length
                case other    => invalidJson("Array", other)
              },
              {
                case arr: Arr => arr.value.iterator
                case other    => invalidJson("Array", other)
              }
            )
          ),
          RouterSchema.RouterConstruct.String -> RouterSchema.Case(
            "String",
            summon[SonReadWriter[String]].bimap[Value](Str(_)) {
              case Str(value) => value
              case other      => invalidJson("String", other)
            }
          ),
          RouterSchema.RouterConstruct.Char -> RouterSchema.Case(
            "Char",
            summon[SonReadWriter[Char]].bimap[Value](ch => Str(ch.toString)) {
              case Str(value) if value.length == 1 => value.charAt(0)
              case other                           => invalidJson("single-character string", other)
            }
          ),
          RouterSchema.RouterConstruct.Int -> RouterSchema.Case(
            "Int",
            summon[SonReadWriter[Int]].bimap[Value](value => Num(value.toDouble)) {
              case Num(value) => value.toInt
              case other      => invalidJson("number", other)
            }
          ),
          RouterSchema.RouterConstruct.Long -> RouterSchema.Case(
            "Long",
            summon[SonReadWriter[Long]].bimap[Value](value => Num(value.toDouble)) {
              case Num(value) => value.toLong
              case other      => invalidJson("number", other)
            }
          ),
          RouterSchema.RouterConstruct.Float -> RouterSchema.Case(
            "Float",
            summon[SonReadWriter[Float]].bimap[Value](value => Num(value.toDouble)) {
              case Num(value) => value.toFloat
              case other      => invalidJson("number", other)
            }
          ),
          RouterSchema.RouterConstruct.Double -> RouterSchema.Case(
            "Double",
            summon[SonReadWriter[Double]].bimap[Value](Num(_)) {
              case Num(value) => value
              case other      => invalidJson("number", other)
            }
          ),
          RouterSchema.RouterConstruct.Boolean -> RouterSchema.Case(
            "Boolean",
            summon[SonReadWriter[Boolean]].bimap[Value](Bool(_)) {
              case Bool(value) => value
              case other       => invalidJson("boolean", other)
            }
          ),
          RouterSchema.RouterConstruct.Null -> RouterSchema.Case(
            "Null",
            SonReadWriter.forNull[Value](Null)
          )
        ),
      write = JsonSelect
    )

  private object YamlMapBuilder extends SonReader.DictBuilder[Node, YamlMapState, Node]:
    def init(): YamlMapState =
      ListMap.newBuilder[Node, Node]

    def add(repr: YamlMapState, key: String, elem: Node): YamlMapState =
      repr.addOne(scalar(key, Tag.str) -> elem)

    def finish(repr: YamlMapState): Node =
      Node.MappingNode(repr.result())

  private object YamlSeqBuilder extends SonReader.VectorBuilder[Node, YamlSeqState, Node]:
    def init(): YamlSeqState =
      Vector.newBuilder[Node]

    def add(repr: YamlSeqState, elem: Node): YamlSeqState =
      repr.addOne(elem)

    def finish(repr: YamlSeqState): Node =
      Node.SequenceNode(repr.result()*)

  private object YamlSelect extends RouterSchema.Write[Node]:
    def caseIndex(value: Node): Int =
      value match
        case _: Node.MappingNode                                  => 0
        case _: Node.SequenceNode                                 => 2
        case scalar: Node.ScalarNode if scalar.tag == Tag.int     => yamlIntegerCase(scalar.value)
        case scalar: Node.ScalarNode if scalar.tag == Tag.float   => 8
        case scalar: Node.ScalarNode if scalar.tag == Tag.boolean => 9
        case scalar: Node.ScalarNode if scalar.tag == Tag.nullTag => 10
        case _: Node.ScalarNode                                   => 3

  given yamlNodeReadWriter: SonReadWriter[Node] =
    SonReadWriter.router[Node]("yaml.Node", "YAML node")(
      cases = self =>
        List(
          RouterSchema.RouterConstruct.Record -> RouterSchema.Case(
            "Mapping",
            SonReadWriter.dict[Node, Node, YamlMapState](
              self,
              YamlMapBuilder,
              {
                case mapping: Node.MappingNode => mapping.mappings.size
                case other                     => invalidYaml("mapping", other)
              },
              {
                case mapping: Node.MappingNode =>
                  mapping.mappings.iterator.map {
                    case (key: Node.ScalarNode, value) => key.value -> value
                    case (key, _)                      => invalidYaml("string key", key)
                  }
                case other => invalidYaml("mapping", other)
              }
            )
          ),
          RouterSchema.RouterConstruct.Tuple -> RouterSchema.Case(
            "TupleSequence",
            SonReadWriter.tupleOf[Node, Node, YamlSeqState](
              self,
              YamlSeqBuilder,
              {
                case sequence: Node.SequenceNode => sequence.nodes.length
                case other                       => invalidYaml("sequence", other)
              },
              {
                case sequence: Node.SequenceNode => sequence.nodes.iterator
                case other                       => invalidYaml("sequence", other)
              }
            )
          ),
          RouterSchema.RouterConstruct.Vector -> RouterSchema.Case(
            "Sequence",
            SonReadWriter.vector[Node, Node, YamlSeqState](
              self,
              YamlSeqBuilder,
              {
                case sequence: Node.SequenceNode => sequence.nodes.length
                case other                       => invalidYaml("sequence", other)
              },
              {
                case sequence: Node.SequenceNode => sequence.nodes.iterator
                case other                       => invalidYaml("sequence", other)
              }
            )
          ),
          RouterSchema.RouterConstruct.String -> RouterSchema.Case("String", yamlStringScalar),
          RouterSchema.RouterConstruct.Char   -> RouterSchema.Case(
            "Char",
            yamlCharScalar
          ),
          RouterSchema.RouterConstruct.Int -> RouterSchema.Case(
            "Int",
            yamlIntScalar
          ),
          RouterSchema.RouterConstruct.Long -> RouterSchema.Case(
            "Long",
            yamlLongScalar
          ),
          RouterSchema.RouterConstruct.Float -> RouterSchema.Case(
            "Float",
            yamlFloatScalar
          ),
          RouterSchema.RouterConstruct.Double -> RouterSchema.Case(
            "Double",
            yamlDoubleScalar
          ),
          RouterSchema.RouterConstruct.Boolean -> RouterSchema.Case(
            "Boolean",
            yamlBooleanScalar
          ),
          RouterSchema.RouterConstruct.Null -> RouterSchema.Case(
            "Null",
            SonReadWriter.forNull[Node](scalar("null", Tag.nullTag))
          )
        ),
      write = YamlSelect
    )

  def toUjson[T: SonWriter](value: T): Result[Value, DecodeError] =
    Writers.writeExpr(value).decodeAs[Value]

  def fromUjson[T: SonReader](value: Value): Result[T, DecodeError] =
    Writers.writeExpr(value).decodeAs[T]

  def upickleReadWriter[T](using SonReadWriter[T]): upickle.default.ReadWriter[T] =
    UpickleSchemaAdapter.readWriter(summon[SonReadWriter[T]])

  def fromJsonText[T](json: String)(using readWriter: SonReadWriter[T]): T =
    given upickle.default.ReadWriter[T] = upickleReadWriter[T]
    upickle.default.read[T](json)

  def jsonTextToScalaObjectNotation[T](json: String)(using readWriter: SonReadWriter[T]): String =
    val value = fromJsonText[T](json)
    Writers.write(value)(using readWriter.writer)

  def toYamlNode[T: SonWriter](value: T): Result[Node, DecodeError] =
    Writers.writeExpr(value).decodeAs[Node]

  def fromYamlNode[T: SonReader](node: Node): Result[T, DecodeError] =
    Writers.writeExpr(node).decodeAs[T]

  def yamlEncoder[T](using SonWriter[T]): YamlEncoder[T] =
    new:
      def asNode(obj: T): Node =
        resultOrThrow(toYamlNode(obj))

  def yamlDecoder[T](using SonReader[T]): YamlDecoder[T] =
    new:
      def construct(node: Node)(using settings: LoadSettings): Either[ConstructError, T] =
        fromYamlNode[T](node) match
          case Result.Ok(value) =>
            Right(value)
          case Result.Err(error) =>
            Left(ConstructError(error.format, Some(node), None))

  private val yamlStringScalar: SonReadWriter[Node] =
    summon[SonReadWriter[String]].bimap[Node](value => scalar(value, Tag.str)) {
      case scalar: Node.ScalarNode => scalar.value
      case other                   => invalidYaml("scalar", other)
    }

  private val yamlCharScalar: SonReadWriter[Node] =
    summon[SonReadWriter[Char]].bimap[Node](value => scalar(value.toString, Tag.str)) {
      case scalar: Node.ScalarNode if scalar.value.length == 1 => scalar.value.charAt(0)
      case other => invalidYaml("single-character scalar", other)
    }

  private val yamlIntScalar: SonReadWriter[Node] =
    summon[SonReadWriter[Int]].bimap[Node](value => scalar(value.toString, Tag.int)) {
      case scalar: Node.ScalarNode => scalar.value.toInt
      case other                   => invalidYaml("integer scalar", other)
    }

  private val yamlLongScalar: SonReadWriter[Node] =
    summon[SonReadWriter[Long]].bimap[Node](value => scalar(value.toString, Tag.int)) {
      case scalar: Node.ScalarNode => scalar.value.toLong
      case other                   => invalidYaml("integer scalar", other)
    }

  private val yamlFloatScalar: SonReadWriter[Node] =
    summon[SonReadWriter[Float]].bimap[Node](value => scalar(value.toString, Tag.float)) {
      case scalar: Node.ScalarNode => scalar.value.toFloat
      case other                   => invalidYaml("float scalar", other)
    }

  private val yamlDoubleScalar: SonReadWriter[Node] =
    summon[SonReadWriter[Double]].bimap[Node](value => scalar(value.toString, Tag.float)) {
      case scalar: Node.ScalarNode => scalar.value.toDouble
      case other                   => invalidYaml("float scalar", other)
    }

  private val yamlBooleanScalar: SonReadWriter[Node] =
    summon[SonReadWriter[Boolean]].bimap[Node](value => scalar(value.toString, Tag.boolean)) {
      case scalar: Node.ScalarNode => scalar.value.toBoolean
      case other                   => invalidYaml("boolean scalar", other)
    }

  private def yamlIntegerCase(value: String): Int =
    if value.toIntOption.isDefined then 5 else 6

  private def scalar(value: String, tag: Tag): Node.ScalarNode =
    Node.ScalarNode(value)

  private def resultOrThrow[T](result: Result[T, DecodeError]): T =
    result match
      case Result.Ok(value)  => value
      case Result.Err(error) => throw IllegalArgumentException(error.format)

  private def invalidJson(expected: String, value: Value): Nothing =
    throw IllegalArgumentException(s"Expected ujson $expected but found $value")

  private def invalidYaml(expected: String, value: Node): Nothing =
    throw IllegalArgumentException(s"Expected YAML $expected but found $value")
