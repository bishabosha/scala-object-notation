package scalanotation.internal.json

import scalanotation.schema.RawSchema

import java.nio.charset.StandardCharsets

/** Flat per-field JSON decode plan, the JSON counterpart of [[RawSchema.FieldPlans]]: value
  * dispatch codes, the field names pre-encoded as the exact UTF-8 bytes the happy path expects
  * (`"name":` — quote, escaped name, quote, colon), the escape-free name content bytes for the
  * out-of-order slice compare (null when the name needs escapes), and the shared fill values.
  *
  * Cached once per schema instance in [[RawSchema.externalPlans]], so a record decode pays one
  * volatile read for the whole plan.
  */
private[json] final class JsonFieldPlans(
    val kinds: Array[Byte],
    /** the fused header bytes `"name":` per field (case names for sums) */
    val headerBytes: Array[Array[Byte]],
    /** escape-free name content bytes for slice comparison, or null when the JSON encoding of the
      * name contains escapes (then only a decoded-string compare is valid)
      */
    val contentBytes: Array[Array[Byte] | Null],
    val names: Array[String],
    /** decode-time fill values shared with the core plans (None in skippable mode, installed
      * defaults in defaults mode), or null when fields may not be omitted
      */
    val fills: Array[AnyRef | Null] | Null
):
  /** the fused header as text (`"name":`) for the encoder — derived once from the bytes */
  val headerText: Array[String] =
    headerBytes.map(bytes => new String(bytes, java.nio.charset.StandardCharsets.UTF_8))

private[json] object JsonFieldPlans:
  val Empty: JsonFieldPlans =
    JsonFieldPlans(Array.emptyByteArray, Array.empty, Array.empty, Array.empty, null)

  def of(schema: RawSchema[?]): JsonFieldPlans =
    schema.externalPlans(compute)

  private val compute: RawSchema[?] => JsonFieldPlans =
    case namedTuple: RawSchema.NamedTuple[?] =>
      val fields  = namedTuple.fields
      val kinds   = new Array[Byte](fields.length)
      val headers = new Array[Array[Byte]](fields.length)
      val content = new Array[Array[Byte] | Null](fields.length)
      val names   = new Array[String](fields.length)
      var index   = 0
      while index < fields.length do
        val field = fields(index)
        kinds(index) = RawSchema.valuePlanOf(field.schema)
        headers(index) = headerBytesOf(field.name)
        content(index) = contentBytesOf(field.name)
        names(index) = field.name
        index += 1
      // the fill values (skippable Nones or installed defaults) are computed once by the core
      // plans; sharing the array keeps the two formats' omission semantics identical
      JsonFieldPlans(kinds, headers, content, names, namedTuple.fieldPlans.fills)
    case sum: RawSchema.Sum[?] =>
      val cases   = sum.cases
      val kinds   = new Array[Byte](cases.length)
      val headers = new Array[Array[Byte]](cases.length)
      val content = new Array[Array[Byte] | Null](cases.length)
      val names   = new Array[String](cases.length)
      var index   = 0
      while index < cases.length do
        val sumCase = cases(index)
        kinds(index) = RawSchema.valuePlanOf(sumCase.schema)
        headers(index) = headerBytesOf(sumCase.name)
        content(index) = contentBytesOf(sumCase.name)
        names(index) = sumCase.name
        index += 1
      JsonFieldPlans(kinds, headers, content, names, null)
    case sum: RawSchema.DiscriminatorSum[?] =>
      // entry 0 is the discriminator header; entries 1..n carry the case names, whose values
      // arrive as JSON strings — the header decode slice-matches the string content against
      // contentBytes without materializing it
      val cases   = sum.cases
      val kinds   = new Array[Byte](cases.length + 1)
      val headers = new Array[Array[Byte]](cases.length + 1)
      val content = new Array[Array[Byte] | Null](cases.length + 1)
      val names   = new Array[String](cases.length + 1)
      kinds(0) = RawSchema.FieldPlan.StringV
      headers(0) = headerBytesOf(sum.discriminatorField)
      content(0) = contentBytesOf(sum.discriminatorField)
      names(0) = sum.discriminatorField
      var index = 0
      while index < cases.length do
        kinds(index + 1) = RawSchema.FieldPlan.Other
        headers(index + 1) = headerBytesOf(cases(index).name)
        content(index + 1) = contentBytesOf(cases(index).name)
        names(index + 1) = cases(index).name
        index += 1
      JsonFieldPlans(kinds, headers, content, names, null)
    case _ => JsonFieldPlans.Empty

  /** the exact bytes the fused header probe expects: `"` + JSON-escaped name + `":` */
  private def headerBytesOf(name: String): Array[Byte] =
    val builder = new java.lang.StringBuilder(name.length + 3)
    builder.append('"')
    JsonText.appendEscaped(name, builder)
    builder.append('"')
    builder.append(':')
    builder.toString.getBytes(StandardCharsets.UTF_8)

  /** the raw content bytes for slice comparison, or null when the name needs JSON escapes (a slice
    * compare over escaped input would be incorrect, so those names take the decoded path)
    */
  private def contentBytesOf(name: String): Array[Byte] | Null =
    if JsonText.needsEscaping(name) then null
    else name.getBytes(StandardCharsets.UTF_8)

/** Shared JSON text helpers used by the plans, the encoder, and the public number validators. */
private[scalanotation] object JsonText:
  def needsEscaping(value: String): Boolean =
    var i = 0
    while i < value.length do
      val ch = value.charAt(i)
      if ch < 0x20 || ch == '"' || ch == '\\' then return true
      i += 1
    false

  /** appends `value` with JSON escapes into `builder` (no surrounding quotes) */
  def appendEscaped(value: String, builder: java.lang.StringBuilder): Unit =
    val length = value.length
    var start  = 0
    var i      = 0
    while i < length do
      val ch = value.charAt(i)
      if ch < 0x20 || ch == '"' || ch == '\\' then
        if i > start then builder.append(value, start, i)
        appendEscape(ch, builder)
        start = i + 1
      i += 1
    if start == 0 then builder.append(value)
    else if length > start then builder.append(value, start, length)

  private def appendEscape(ch: Char, builder: java.lang.StringBuilder): Unit =
    (ch: @annotation.switch) match
      case '"'  => builder.append("\\\"")
      case '\\' => builder.append("\\\\")
      case '\b' => builder.append("\\b")
      case '\f' => builder.append("\\f")
      case '\n' => builder.append("\\n")
      case '\r' => builder.append("\\r")
      case '\t' => builder.append("\\t")
      case _    =>
        builder.append("\\u00")
        val hi = (ch >> 4) & 0xf
        val lo = ch & 0xf
        builder.append(hexDigit(hi))
        builder.append(hexDigit(lo))

  private def hexDigit(value: Int): Char =
    if value < 10 then ('0' + value).toChar else ('a' + value - 10).toChar

  /** Validates that `text` is exactly one JSON number (the grammar of RFC 8259 §6). Used by the
    * encoder before emitting raw number text from a router's raw-number case.
    */
  def isValidNumber(text: String): Boolean =
    val length = text.length
    if length == 0 then return false
    var i = 0
    if text.charAt(i) == '-' then i += 1
    if i >= length then return false
    // int part: 0 | [1-9][0-9]*
    if text.charAt(i) == '0' then i += 1
    else if text.charAt(i) >= '1' && text.charAt(i) <= '9' then
      i += 1
      while i < length && isDigit(text.charAt(i)) do i += 1
    else return false
    // frac
    if i < length && text.charAt(i) == '.' then
      i += 1
      if i >= length || !isDigit(text.charAt(i)) then return false
      while i < length && isDigit(text.charAt(i)) do i += 1
    // exp
    if i < length && (text.charAt(i) == 'e' || text.charAt(i) == 'E') then
      i += 1
      if i < length && (text.charAt(i) == '+' || text.charAt(i) == '-') then i += 1
      if i >= length || !isDigit(text.charAt(i)) then return false
      while i < length && isDigit(text.charAt(i)) do i += 1
    i == length

  private inline def isDigit(ch: Char): Boolean = ch >= '0' && ch <= '9'
