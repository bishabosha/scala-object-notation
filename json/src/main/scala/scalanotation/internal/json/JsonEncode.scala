package scalanotation.internal.json

import scalanotation.RouterSchema
import scalanotation.TextFormat
import scalanotation.schema
import scalanotation.schema.RawSchema

import scala.{Option => ScalaOption}

/** Renders values as JSON straight from their [[RawSchema]] — the JSON mirror of the core renderer,
  * with the same unboxed transfers: primitive fields pull through the write's typed accessors,
  * mapped primitives through the mapping's specialized input dispatch.
  */
private[scalanotation] object JsonEncode:

  final class Output:
    // the underlying java builder's typed append overloads format primitives in place, so no
    // intermediate String is allocated for numeric and boolean literals
    private val builder = new java.lang.StringBuilder

    def append(ch: Char): Unit       = builder.append(ch)
    def append(str: String): Unit    = builder.append(str)
    def append(value: Int): Unit     = builder.append(value)
    def append(value: Long): Unit    = builder.append(value)
    def append(value: Boolean): Unit = builder.append(value)

    def appendDouble(value: Double): Unit =
      if !java.lang.Double.isFinite(value) then
        throw IllegalArgumentException(s"Cannot render non-finite Double value $value as JSON")
      builder.append(value)

    def appendFloat(value: Float): Unit =
      if !java.lang.Float.isFinite(value) then
        throw IllegalArgumentException(s"Cannot render non-finite Float value $value as JSON")
      builder.append(value)

    def appendString(value: String): Unit =
      builder.append('"')
      JsonText.appendEscaped(value, builder)
      builder.append('"')

    def appendChar(value: Char): Unit =
      builder.append('"')
      if value < 0x20 || value == '"' || value == '\\' then
        JsonText.appendEscaped(value.toString, builder)
      else builder.append(value)
      builder.append('"')

    /** appends a pre-encoded `"name":` header plus the configured value spacing */
    def appendHeader(headerText: String)(using format: TextFormat): Unit =
      builder.append(headerText)
      valueSpacing()

    def appendName(name: String)(using format: TextFormat): Unit =
      appendString(name)
      builder.append(':')
      valueSpacing()

    def valueSpacing()(using format: TextFormat): Unit =
      var i = 0
      while i < format.spacing do
        builder.append(' ')
        i += 1

    def newlineAndIndent(depth: Int)(using format: TextFormat): Unit =
      if format.pretty then
        builder.append('\n')
        var i = 0
        val n = depth * format.indent
        while i < n do
          builder.append(' ')
          i += 1

    def result(): String = builder.toString

  private def missingWriteCapability(schema: RawSchema[?]): Nothing =
    throw IllegalStateException(s"write is not available for schema ${schema.describeSelf}")

  private def mappedInput(mapping: schema.SchemaMapping[?, ?], value: Any): Any =
    mapping.asInstanceOf[schema.SchemaMapping[Any, Any]].mapInput(value)

  private def anyMapping(mapping: schema.SchemaMapping[?, ?]): schema.SchemaMapping[Any, Any] =
    mapping.asInstanceOf[schema.SchemaMapping[Any, Any]]

  def renderText(
      schema: RawSchema[?],
      value: Any,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit = schema match
    case mapped: RawSchema.Mapped[?, ?] =>
      // primitive bases pull the input through the mapping's specialized dispatch, the
      // write-side dual of the decoder's typed slots
      mapped.base match
        case RawSchema.Int =>
          out.append(anyMapping(mapped.mapping).mapInputInt(value))
        case RawSchema.Long =>
          out.append(anyMapping(mapped.mapping).mapInputLong(value))
        case RawSchema.Float =>
          out.appendFloat(anyMapping(mapped.mapping).mapInputFloat(value))
        case RawSchema.Double =>
          out.appendDouble(anyMapping(mapped.mapping).mapInputDouble(value))
        case base =>
          renderText(base, mappedInput(mapped.mapping, value), out, depth)
    case RawSchema.Ref(_, target) =>
      renderText(target(), value, out, depth)
    case router: RawSchema.Router[?] =>
      renderRouter(router, value, out, depth)
    case namedTuple: RawSchema.NamedTuple[?] =>
      val write = namedTuple.write
      if write == null then missingWriteCapability(schema)
      val fields = namedTuple.fields
      val plans  = JsonFieldPlans.of(namedTuple)
      renderObject(out, depth, fields.length) { index =>
        out.appendHeader(plans.headerText(index))
        renderFieldText(fields(index).schema, write, value, index, out, depth + 1)
      }
    case tuple: RawSchema.Tuple[?] =>
      val write = tuple.write
      if write == null then missingWriteCapability(schema)
      val slots = tuple.slots
      renderArray(out, depth, write.size(value)) { index =>
        renderTupleSlotText(slots(index), write, value, index, out, depth + 1)
      }
    case RawSchema.PartialNamedTuple(base, _) =>
      renderText(base, value, out, depth)
    case sum: RawSchema.Sum[?] =>
      val write = sum.write
      if write == null then missingWriteCapability(schema)
      val caseIndex = write.caseIndex(value)
      val sumCase   = sum.cases(caseIndex)
      val plans     = JsonFieldPlans.of(sum)
      renderObject(out, depth, 1) { _ =>
        out.appendHeader(plans.headerText(caseIndex))
        renderText(sumCase.schema, value, out, depth + 1)
      }
    case sum: RawSchema.DiscriminatorSum[?] =>
      val write = sum.write
      if write == null then missingWriteCapability(schema)
      val caseIndex = write.caseIndex(value)
      val sumCase   = sum.cases(caseIndex)
      val plans     = JsonFieldPlans.of(sum)
      renderDiscriminatedObject(sum, plans, sumCase, caseIndex, value, out, depth)
    case vector: RawSchema.Vector[?, ?] =>
      renderVectorLike(vector, vector.element, vector.write, value, out, depth)
    case tupleOf: RawSchema.TupleOf[?, ?] =>
      renderVectorLike(tupleOf, tupleOf.element, tupleOf.write, value, out, depth)
    case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
      val write = pairSeq.write
      if write == null then missingWriteCapability(schema)
      val values = write.iterator(value)
      renderArray(out, depth, write.size(value)) { _ =>
        val (key, elem) = values.next()
        renderArray(out, depth + 1, 2) { index =>
          if index == 0 then renderText(pairSeq.key, key, out, depth + 2)
          else renderText(pairSeq.value, elem, out, depth + 2)
        }
      }
    case dict: RawSchema.Dict[?, ?] =>
      val write = dict.write
      if write == null then missingWriteCapability(schema)
      val values = write.iterator(value)
      renderObject(out, depth, write.size(value)) { _ =>
        val (key, elem) = values.next()
        out.appendName(key)
        renderText(dict.element, elem, out, depth + 1)
      }
    case option: RawSchema.Option[?] =>
      value.asInstanceOf[ScalaOption[Any]] match
        case Some(innerValue) => renderText(option.inner, innerValue, out, depth)
        case None             => out.append("null")
    case RawSchema.String =>
      out.appendString(value.asInstanceOf[String])
    case RawSchema.Char =>
      out.appendChar(value.asInstanceOf[Char])
    case RawSchema.Int =>
      out.append(value.asInstanceOf[Int])
    case RawSchema.Long =>
      out.append(value.asInstanceOf[Long])
    case RawSchema.Float =>
      out.appendFloat(value.asInstanceOf[Float])
    case RawSchema.Double =>
      out.appendDouble(value.asInstanceOf[Double])
    case RawSchema.Boolean =>
      out.append(value.asInstanceOf[Boolean])
    case RawSchema.Null =>
      out.append("null")

  private def renderRouter(
      router: RawSchema.Router[?],
      value: Any,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    if router.write == null then missingWriteCapability(router)
    val index = router.write.asInstanceOf[RouterSchema.Write[Any]].caseIndex(router.router, value)
    val selected = RawSchema.routerCase(router, index)
    if selected == null then
      throw IllegalArgumentException(
        s"router ${router.describeSelf} cannot select a case for value $value"
      )
    if RouterSchema.Index.toInt(index) == RouterSchema.Index.toInt(router.router.rawNumberIndex)
    then renderRawNumber(selected.schema, value, out)
    else renderText(selected.schema, value, out, depth)

  /** Renders a raw-number case: the value maps through the case's input chain to its raw text,
    * which must spell exactly one JSON number.
    */
  private def renderRawNumber(schema: RawSchema[?], value: Any, out: Output): Unit =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        renderRawNumber(mapped.base, mappedInput(mapped.mapping, value), out)
      case RawSchema.Ref(_, target) =>
        renderRawNumber(target(), value, out)
      case RawSchema.String =>
        val text = value.asInstanceOf[String]
        if !JsonText.isValidNumber(text) then
          throw IllegalArgumentException(s"'$text' is not a valid JSON number")
        out.append(text)
      case other =>
        throw IllegalStateException(
          s"raw number case must be backed by a String schema, but found ${other.describeSelf}"
        )

  private def renderDiscriminatedObject(
      sum: RawSchema.DiscriminatorSum[?],
      plans: JsonFieldPlans,
      sumCase: RawSchema.SumCase,
      caseIndex: Int,
      value: Any,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    out.append('{')
    out.newlineAndIndent(depth + 1)
    out.appendHeader(plans.headerText(0))
    out.appendString(sumCase.name)
    renderDiscriminatorPayload(sumCase.schema, value, out, depth)
    out.newlineAndIndent(depth)
    out.append('}')

  /** renders the case's fields after the discriminator, prefixed by separators */
  private def renderDiscriminatorPayload(
      schema: RawSchema[?],
      value: Any,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case RawSchema.PartialNamedTuple(base, _) =>
        renderDiscriminatorPayload(base, value, out, depth)
      case mapped: RawSchema.Mapped[?, ?] =>
        renderDiscriminatorPayload(mapped.base, mappedInput(mapped.mapping, value), out, depth)
      case RawSchema.Ref(_, target) =>
        renderDiscriminatorPayload(target(), value, out, depth)
      case namedTuple: RawSchema.NamedTuple[?] =>
        val write = namedTuple.write
        if write == null then missingWriteCapability(namedTuple)
        val fields = namedTuple.fields
        val plans  = JsonFieldPlans.of(namedTuple)
        var index  = 0
        while index < fields.length do
          out.append(',')
          if format.pretty then out.newlineAndIndent(depth + 1) else out.valueSpacing()
          out.appendHeader(plans.headerText(index))
          renderFieldText(fields(index).schema, write, value, index, out, depth + 1)
          index += 1
      case RawSchema.Null =>
        ()
      case other =>
        throw IllegalStateException(
          s"discriminator sum case must be a named tuple, but found ${other.describeSelf}"
        )

  private def renderVectorLike(
      schema: RawSchema[?],
      element: RawSchema[?],
      write: RawSchema.VectorWrite | Null,
      value: Any,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    if write == null then missingWriteCapability(schema)
    write match
      case indexed: RawSchema.IndexedVectorWrite =>
        renderArray(out, depth, indexed.size(value)) { index =>
          renderElementText(element, indexed, value, index, out, depth + 1)
        }
      case _ =>
        val values = write.iterator(value)
        renderArray(out, depth, write.size(value)) { _ =>
          renderText(element, values.next(), out, depth + 1)
        }

  private def renderObject(
      out: Output,
      depth: Int,
      size: Int
  )(
      renderField: Int => Unit
  )(using format: TextFormat): Unit =
    renderComposite(out, depth, size, open = '{', close = '}')(renderField)

  private def renderArray(
      out: Output,
      depth: Int,
      size: Int
  )(
      renderValue: Int => Unit
  )(using format: TextFormat): Unit =
    renderComposite(out, depth, size, open = '[', close = ']')(renderValue)

  private def renderComposite(
      out: Output,
      depth: Int,
      size: Int,
      open: Char,
      close: Char
  )(
      renderValue: Int => Unit
  )(using format: TextFormat): Unit =
    out.append(open)
    if size == 0 then out.append(close)
    else if !format.pretty then
      var i = 0
      while i < size do
        if i > 0 then
          out.append(',')
          out.valueSpacing()
        renderValue(i)
        i += 1
      out.append(close)
    else
      var i = 0
      while i < size do
        out.newlineAndIndent(depth + 1)
        renderValue(i)
        if i + 1 < size then out.append(',')
        i += 1
      out.newlineAndIndent(depth)
      out.append(close)

  /** Renders field `index` of `value`, pulling it through the write's typed accessor when the field
    * schema pins it to a primitive — no box is allocated for the transfer.
    */
  private def renderFieldText(
      schema: RawSchema[?],
      write: RawSchema.NamedTupleWrite,
      value: Any,
      index: Int,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case RawSchema.Int =>
        out.append(write.intFieldValue(value, index))
      case RawSchema.String =>
        out.appendString(write.stringFieldValue(value, index))
      case RawSchema.Boolean =>
        out.append(write.booleanFieldValue(value, index))
      case RawSchema.Long =>
        out.append(write.longFieldValue(value, index))
      case RawSchema.Double =>
        out.appendDouble(write.doubleFieldValue(value, index))
      case RawSchema.Float =>
        out.appendFloat(write.floatFieldValue(value, index))
      case RawSchema.Char =>
        out.appendChar(write.charFieldValue(value, index))
      case other =>
        renderText(other, write.fieldValue(value, index), out, depth)

  /** renders tuple slot `index` of `value` — see [[renderFieldText]] */
  private def renderTupleSlotText(
      schema: RawSchema[?],
      write: RawSchema.TupleWrite,
      value: Any,
      index: Int,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case RawSchema.Int =>
        out.append(write.intElementValue(value, index))
      case RawSchema.String =>
        out.appendString(write.stringElementValue(value, index))
      case RawSchema.Boolean =>
        out.append(write.booleanElementValue(value, index))
      case RawSchema.Long =>
        out.append(write.longElementValue(value, index))
      case RawSchema.Double =>
        out.appendDouble(write.doubleElementValue(value, index))
      case RawSchema.Float =>
        out.appendFloat(write.floatElementValue(value, index))
      case RawSchema.Char =>
        out.appendChar(write.charElementValue(value, index))
      case other =>
        renderText(other, write.elementValue(value, index), out, depth)

  /** renders element `index` of an indexed vector `value` — see [[renderFieldText]] */
  private def renderElementText(
      schema: RawSchema[?],
      write: RawSchema.IndexedVectorWrite,
      value: Any,
      index: Int,
      out: Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case RawSchema.Int =>
        out.append(write.intElementValue(value, index))
      case RawSchema.String =>
        out.appendString(write.stringElementValue(value, index))
      case RawSchema.Boolean =>
        out.append(write.booleanElementValue(value, index))
      case RawSchema.Long =>
        out.append(write.longElementValue(value, index))
      case RawSchema.Double =>
        out.appendDouble(write.doubleElementValue(value, index))
      case RawSchema.Float =>
        out.appendFloat(write.floatElementValue(value, index))
      case RawSchema.Char =>
        out.appendChar(write.charElementValue(value, index))
      case other =>
        renderText(other, write.elementValue(value, index), out, depth)
