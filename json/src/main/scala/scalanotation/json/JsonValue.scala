package scalanotation.json

import scalanotation.ReadWriter
import scalanotation.Reader
import scalanotation.RouterSchema
import scalanotation.Writer
import scalanotation.internal.json.JsonText
import scalanotation.schema.RawSchema

import scala.collection.mutable

/** A dynamic JSON value. Numbers carry their exact source text — no precision is assumed, so any
  * JSON number round-trips bit-for-bit; interpret via [[JsonValue.Num.toBigDecimal]] or the
  * narrowing accessors.
  */
enum JsonValue:
  case Obj(fields: IndexedSeq[(String, JsonValue)])
  case Arr(elements: IndexedSeq[JsonValue])
  case Str(value: String)
  case Num(raw: String)
  case Bool(value: Boolean)
  case Null

object JsonValue:
  extension (num: Num)
    def toBigDecimal: BigDecimal = BigDecimal(num.raw)
    def toDoubleValue: Double    = java.lang.Double.parseDouble(num.raw)

  /** a [[Num]] from a numeric value, spelled with its natural literal text */
  def num(value: Int): Num        = Num(value.toString)
  def num(value: Long): Num       = Num(value.toString)
  def num(value: BigDecimal): Num = Num(value.bigDecimal.toString)
  def num(value: Double): Num     =
    if !java.lang.Double.isFinite(value) then
      throw IllegalArgumentException(s"Cannot represent non-finite Double value $value as JSON")
    Num(value.toString)

  /** a [[Num]] from raw JSON number text, validated eagerly */
  def numFromText(raw: String): Num =
    if !JsonText.isValidNumber(raw) then
      throw IllegalArgumentException(s"'$raw' is not a valid JSON number")
    Num(raw)

  def obj(fields: (String, JsonValue)*): Obj = Obj(fields.toIndexedSeq)
  def arr(elements: JsonValue*): Arr         = Arr(elements.toIndexedSeq)

  given readWriter: ReadWriter[JsonValue] = ReadWriter.fromSchema(JsonValueRouterSchema)
  given reader: Reader[JsonValue]         = readWriter.reader
  given writer: Writer[JsonValue]         = readWriter.writer

  /** Router-based schema over the JSON constructs: objects, arrays, strings, raw numbers (via
    * [[RouterSchema.NumberMode.Raw]], so no precision is assumed), booleans, and null. Case order
    * matches the enum ordinals, so the write side routes by ordinal.
    */
  private[json] val JsonValueRouterSchema: RawSchema[JsonValue] =
    val self: RawSchema[JsonValue] = RawSchema.Ref("JsonValue", () => JsonValueRouterSchema)
    RawSchema.Router(
      name = "JsonValue",
      selfKind = "any JSON value",
      IArray(
        RawSchema.RouterCase("Obj", RawSchema.Dict(self, ObjRead, ObjWrite)),
        RawSchema.RouterCase("Arr", RawSchema.Vector(self, ArrRead, ArrWrite)),
        RawSchema.RouterCase(
          "Str",
          RawSchema.mapPureAndInput(RawSchema.String)(
            resultMap0 = value => JsonValue.Str(value.asInstanceOf[String]),
            inputMap0 = value =>
              value.asInstanceOf[JsonValue] match
                case JsonValue.Str(text) => text
                case other               => invalidRouterInput("Str", other)
          )
        ),
        RawSchema.RouterCase(
          "Num",
          RawSchema.mapPureAndInput(RawSchema.String)(
            resultMap0 = value => JsonValue.Num(value.asInstanceOf[String]),
            inputMap0 = value =>
              value.asInstanceOf[JsonValue] match
                case JsonValue.Num(raw) => raw
                case other              => invalidRouterInput("Num", other)
          )
        ),
        RawSchema.RouterCase(
          "Bool",
          RawSchema.mapPureAndInput(RawSchema.Boolean)(
            resultMap0 = value => JsonValue.Bool(value.asInstanceOf[Boolean]),
            inputMap0 = value =>
              value.asInstanceOf[JsonValue] match
                case JsonValue.Bool(flag) => flag
                case other                => invalidRouterInput("Bool", other)
          )
        ),
        RawSchema.RouterCase(
          "Null",
          RawSchema.mapPureAndInput(RawSchema.Null)(
            resultMap0 = _ => JsonValue.Null,
            inputMap0 = value =>
              value.asInstanceOf[JsonValue] match
                case JsonValue.Null => null
                case other          => invalidRouterInput("Null", other)
          )
        )
      ),
      RouterSchema.Router(
        JsonValueRouter.ObjCase,
        RawSchema.UnsupportedRouterCase, // tuples: JSON arrays route to Arr
        JsonValueRouter.ArrCase,
        JsonValueRouter.StrCase,
        RawSchema.UnsupportedRouterCase, // chars: JSON strings route to Str
        RawSchema.UnsupportedRouterCase, // bounded numbers never arise in raw mode
        RawSchema.UnsupportedRouterCase,
        RawSchema.UnsupportedRouterCase,
        RawSchema.UnsupportedRouterCase,
        JsonValueRouter.BoolCase,
        JsonValueRouter.NullCase,
        JsonValueRouter.NumCase,
        RawSchema.UnsupportedRouterCase
      ),
      JsonValueRouterWrite,
      RouterSchema.NumberMode.Raw
    )
  end JsonValueRouterSchema

  private def invalidRouterInput(expected: String, value: JsonValue): Nothing =
    throw IllegalArgumentException(s"Expected JsonValue.$expected but found $value")

  private object JsonValueRouter:
    inline val ObjCase  = 0
    inline val ArrCase  = 1
    inline val StrCase  = 2
    inline val NumCase  = 3
    inline val BoolCase = 4
    inline val NullCase = 5

  private object JsonValueRouterWrite extends RouterSchema.Write[JsonValue]:
    def caseIndex(router: RouterSchema.Router, value: JsonValue): RouterSchema.Index =
      if value == null then router.unsupportedIndex
      else RouterSchema.Index.fromInt(value.ordinal)

  private object ObjRead
      extends Reader.DictBuilder[
        JsonValue,
        mutable.Builder[(String, JsonValue), IArray[(String, JsonValue)]],
        JsonValue.Obj
      ]:
    def init(): mutable.Builder[(String, JsonValue), IArray[(String, JsonValue)]] =
      IArray.newBuilder[(String, JsonValue)]

    def add(
        state: mutable.Builder[(String, JsonValue), IArray[(String, JsonValue)]],
        key: String,
        elem: JsonValue
    ): mutable.Builder[(String, JsonValue), IArray[(String, JsonValue)]] =
      state.addOne((key, elem))

    def finish(
        state: mutable.Builder[(String, JsonValue), IArray[(String, JsonValue)]]
    ): JsonValue.Obj =
      JsonValue.Obj(state.result())

  private object ObjWrite extends RawSchema.DictWrite:
    def size(value: Any): Int =
      value.asInstanceOf[JsonValue] match
        case JsonValue.Obj(fields) => fields.length
        case other                 => invalidRouterInput("Obj", other)

    def iterator(value: Any): Iterator[(String, Any)] =
      value.asInstanceOf[JsonValue] match
        case JsonValue.Obj(fields) => fields.iterator.map((key, json) => key -> json)
        case other                 => invalidRouterInput("Obj", other)

  private object ArrRead
      extends Reader.VectorBuilder[
        JsonValue,
        mutable.Builder[JsonValue, IArray[JsonValue]],
        JsonValue.Arr
      ]:
    def init(): mutable.Builder[JsonValue, IArray[JsonValue]] =
      IArray.newBuilder[JsonValue]

    def add(
        state: mutable.Builder[JsonValue, IArray[JsonValue]],
        elem: JsonValue
    ): mutable.Builder[JsonValue, IArray[JsonValue]] =
      state.addOne(elem)

    def finish(state: mutable.Builder[JsonValue, IArray[JsonValue]]): JsonValue.Arr =
      JsonValue.Arr(state.result())

  private object ArrWrite extends RawSchema.VectorWrite:
    def size(value: Any): Int =
      value.asInstanceOf[JsonValue] match
        case JsonValue.Arr(elements) => elements.length
        case other                   => invalidRouterInput("Arr", other)

    def iterator(value: Any): Iterator[Any] =
      value.asInstanceOf[JsonValue] match
        case JsonValue.Arr(elements) => elements.iterator
        case other                   => invalidRouterInput("Arr", other)
