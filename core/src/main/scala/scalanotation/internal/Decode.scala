package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.internal.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.ok
import steps.result.Result.eval.raise

import scala.compiletime.uninitialized

import TokenDecoder.describe

private[scalanotation] object ExprDecoder:
  def decodeExpr[A: Reader as reader](expr: Expr): Result[A, DecodeError] =
    ExprDecoder().decodeInto(reader, expr)

private[scalanotation] object NumericPromotions:
  private[scalanotation] def isExactFloat(value: Int): Boolean =
    value.toFloat.toInt == value

private[scalanotation] class ExprDecoder extends Internal.PoolHolder:
  private def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private def isExactFloat(value: Int): Boolean =
    NumericPromotions.isExactFloat(value)

  private def describeExpr(expr: Expr): String =
    TokenDecoder.describe(expr)

  private def expectedType(schema: RawSchema, expr: Expr): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeExpr(expr))

  private def fillSkippedNullableFields(
      fields: IArray[Field],
      values: Array[AnyRef],
      startIndex: Int,
      actualName: String
  ): Int =
    var index = startIndex
    while index < fields.length
      && fields(index).name != actualName
      && TokenDecoder.isNullable(fields(index).schema)
    do
      values(index) = None.asInstanceOf[AnyRef]
      index += 1
    index

  def decodeInto[A](reader: Reader[A], expr: Expr): Result[A, DecodeError] =
    decodeBase(reader.schema, expr).asInstanceOf[Result[A, DecodeError]]

  private[scalanotation] def decodeRaw(
      schema: RawSchema,
      expr: Expr
  ): Result[Any, DecodeError] =
    decodeBase(schema, expr)

  private def decodeBase(
      schema: RawSchema,
      expr: Expr
  ): Result[Any, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapped.mapping.mapResult(decodeBase(mapped.base, expr))
      case sc: RawSchema.NamedTuple =>
        decodeNamedTuple(sc, expr)
      case sc: RawSchema.Tuple =>
        decodeTuple(sc, expr)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        decodePartialNamedTuple(base, alreadySeenField, expr)
      case sc: RawSchema.Sum =>
        decodeSum(sc, expr)
      case sc: RawSchema.DiscriminatorSum =>
        decodeDiscriminatorSum(sc, expr)
      case sc: RawSchema.Vector =>
        decodeVector(sc, expr)
      case sc: RawSchema.Dict =>
        decodeDict(sc, expr)
      case sc: RawSchema.Option =>
        expr match
          case Expr.NullConstant => Result.Ok(None)
          case other             => decodeBase(sc.inner, other).map(Some(_))
      case RawSchema.AnyExpr =>
        Result.Ok(expr)
      case RawSchema.String =>
        Result:
          expr match
            case Expr.StringConstant(value) => value
            case other                      => raise(expectedType(RawSchema.String, other))
      case RawSchema.Char =>
        Result:
          expr match
            case Expr.CharConstant(value) => value
            case other                    => raise(expectedType(RawSchema.Char, other))
      case RawSchema.Int =>
        Result:
          expr match
            case Expr.IntConstant(value) => value
            case other                   => raise(expectedType(RawSchema.Int, other))
      case RawSchema.Long =>
        Result:
          expr match
            case Expr.LongConstant(value) => value
            case Expr.IntConstant(value)  => value.toLong
            case other                    => raise(expectedType(RawSchema.Long, other))
      case RawSchema.Float =>
        Result:
          expr match
            case Expr.FloatConstant(value)                      => value
            case Expr.IntConstant(value) if isExactFloat(value) =>
              value.toFloat
            case other => raise(expectedType(RawSchema.Float, other))
      case RawSchema.Double =>
        Result:
          expr match
            case Expr.DoubleConstant(value) => value
            case Expr.IntConstant(value)    => value.toDouble
            case other                      => raise(expectedType(RawSchema.Double, other))
      case RawSchema.Boolean =>
        Result:
          expr match
            case Expr.BooleanConstant(value) => value
            case other                       => raise(expectedType(RawSchema.Boolean, other))
      case RawSchema.Null =>
        expr match
          case Expr.NullConstant => Result.Ok(null)
          case other             => Result.Err(expectedType(schema, other))

  private def decodeVector(
      schema: RawSchema.Vector,
      expr: Expr
  ): Result[Any, DecodeError] = Result:
    expr match
      case Expr.VectorExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read   = schema.read.nn
        var values = read.init()
        var index  = 0
        while index < elements.length do
          val value = decodeBase(schema.element, elements(index))
            .mapErr(_.atPath(s"[$index]"))
            .ok
          values = read.add(values, value)
          index += 1
        read.finish(values)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeTuple(
      schema: RawSchema.Tuple,
      expr: Expr
  ): Result[Any, DecodeError] = Result:
    expr match
      case Expr.TupleExpr(elements) =>
        val read = schema.read
        if read == null then missingReadCapability(schema)
        val slots = schema.slots
        if elements.length != slots.length then
          raise(DecodeError.FieldCountMismatch(slots.length, elements.length))
        var state = read.init(slots.length)
        var index = 0
        while index < slots.length do
          val value = decodeBase(slots(index), elements(index))
            .mapErr(_.atPath(s"[$index]"))
            .ok
          state = read.add(state, index, value)
          index += 1
        read.finish(state)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple,
      expr: Expr
  ): Result[Any, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result:
      val read = schema.read
      if read == null then missingReadCapability(schema)
      schema.isValidNamedTuple(namesPool).check
      expr match
        case Expr.NamedTupleExpr(fieldExprs) =>
          val fields = schema.fields
          val values = new Array[AnyRef](fields.length)
          if schema.allowSkippedNullableFields then
            if fieldExprs.isEmpty && fields.nonEmpty then raise(DecodeError.UnitValueNotAllowed())
            var fieldExprIndex = 0
            var fieldIndex     = 0
            while fieldExprIndex < fieldExprs.length do
              val fieldExpr          = fieldExprs(fieldExprIndex)
              val fieldName          = fieldExpr.name
              val expectedBeforeSkip =
                if fieldIndex < fields.length then fields(fieldIndex) else null
              fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, fieldName)

              if fieldIndex >= fields.length then
                if expectedBeforeSkip == null then
                  raise(DecodeError.FieldCountMismatch(fields.length, fieldExprIndex + 1))
                else raise(DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, fieldName))

              val field = fields(fieldIndex)
              if fieldName != field.name then
                raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

              val value = decodeBase(field.schema, fieldExpr.value)
                .mapErr(_.atPath(s".${field.name}"))
                .ok
              values(fieldIndex) = value.asInstanceOf[AnyRef]
              fieldIndex += 1
              fieldExprIndex += 1

            fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, "")
            if fieldIndex != fields.length then
              raise(DecodeError.FieldCountMismatch(fields.length, fieldExprs.length))
          else
            if fieldExprs.length != fields.length then
              raise(DecodeError.FieldCountMismatch(fields.length, fieldExprs.length))
            var index = 0
            while index < fields.length do
              val field     = fields(index)
              val fieldExpr = fieldExprs(index)
              val fieldName = fieldExpr.name
              if fieldName != field.name then
                raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

              val value = decodeBase(field.schema, fieldExpr.value)
                .mapErr(_.atPath(s".${field.name}"))
                .ok
              values(index) = value.asInstanceOf[AnyRef]
              index += 1

          read.build(values)
        case other =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
  }

  private def decodeDict(
      schema: RawSchema.Dict,
      expr: Expr
  ): Result[Any, DecodeError] = Result:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        val read = schema.read
        if read == null then missingReadCapability(schema)
        var index = 0
        var state = read.init()
        while index < fieldExprs.length do
          val fieldExpr = fieldExprs(index)
          val fieldName = fieldExpr.name
          val value     = decodeBase(schema.element, fieldExpr.value)
            .mapErr(_.atPath(s".${fieldName}"))
            .ok
          state = read.add(state, fieldName, value)
          index += 1

        read.finish(state)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeSum(
      schema: RawSchema.Sum,
      expr: Expr
  ): Result[Any, DecodeError] =
    Result:
      expr match
        case Expr.NamedTupleExpr(fieldExprs) =>
          if fieldExprs.length != 1 then raise(DecodeError.FieldCountMismatch(1, fieldExprs.length))
          val fieldExpr = fieldExprs(0)
          val caseName  = fieldExpr.name
          val value     = fieldExpr.value
          val sumCase   = schema.cases.iterator.find(_.name == caseName) match
            case Some(c) => c
            case _       => raise(DecodeError.UnexpectedField(caseName).atPath(s".$caseName"))
          Result.eval.break(decodeBase(sumCase.schema, value).mapErr(_.atPath(s".$caseName")))
        case other =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeDiscriminatorSum(
      schema: RawSchema.DiscriminatorSum,
      expr: Expr
  ): Result[Any, DecodeError] =
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        Result:
          val discriminatorField = schema.discriminatorField
          if fieldExprs.isEmpty then raise(DecodeError.FieldCountMismatch(1, 0))

          val discriminatorExpr = fieldExprs(0)
          if discriminatorExpr.name != discriminatorField then
            raise(DecodeError.FieldOrderMismatch(discriminatorField, discriminatorExpr.name))

          val caseName = decodeBase(RawSchema.String, discriminatorExpr.value)
            .mapErr(_.atPath(s".$discriminatorField"))
            .ok
            .asInstanceOf[String]
          val sumCase = schema.cases.iterator.find(_.name == caseName) match
            case Some(c) => c
            case _ => raise(DecodeError.UnexpectedField(caseName).atPath(s".$discriminatorField"))
          Result.eval.break(decodeBase(sumCase.schema, expr))
      case other =>
        Result.Err(expectedType(schema, other))

  private def decodePartialNamedTuple(
      schema: RawSchema,
      alreadySeenField: String,
      expr: Expr
  ): Result[Any, DecodeError] =
    schema match
      case RawSchema.PartialNamedTuple(base, _) =>
        decodePartialNamedTuple(base, alreadySeenField, expr)
      case mapped: RawSchema.Mapped =>
        mapped.mapping.mapResult(decodePartialNamedTuple(mapped.base, alreadySeenField, expr))
      case namedTuple: RawSchema.NamedTuple =>
        decodePartialNamedTuple(namedTuple, alreadySeenField, expr)
      case RawSchema.Null =>
        expr match
          case Expr.NamedTupleExpr(fieldExprs) =>
            validatePartialNamedTupleStart(fieldExprs, alreadySeenField).flatMap { _ =>
              val payloadFieldCount = fieldExprs.length - 1
              if payloadFieldCount == 0 then Result.Ok(null)
              else Result.Err(DecodeError.FieldCountMismatch(0, payloadFieldCount))
            }
          case other =>
            Result.Err(DecodeError.ExpectedType(RawSchema.Null.describeSelf, describeExpr(other)))
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeExpr(expr)))

  private def validatePartialNamedTupleStart(
      fieldExprs: IndexedSeq[(name: String, value: Expr)],
      alreadySeenField: String
  ): Result[Unit, DecodeError] =
    Result:
      if fieldExprs.isEmpty then raise(DecodeError.FieldCountMismatch(1, 0))
      val actualName = fieldExprs(0).name
      if actualName != alreadySeenField then
        raise(DecodeError.FieldOrderMismatch(alreadySeenField, actualName))

  private def decodePartialNamedTuple(
      schema: RawSchema.NamedTuple,
      alreadySeenField: String,
      expr: Expr
  ): Result[Any, DecodeError] = Result:
    val read = schema.read
    if read == null then missingReadCapability(schema)
    schema.isValidNamedTuple(namesPool).check
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        validatePartialNamedTupleStart(fieldExprs, alreadySeenField).check
        val fields     = schema.fields
        val values     = new Array[AnyRef](fields.length)
        var fieldIndex = 0
        val offset     = 1
        var index      = offset
        while index < fieldExprs.length do
          val parsedFieldIndex               = index - offset
          val fieldExpr                      = fieldExprs(index)
          val actualName                     = fieldExpr.name
          val validated: DecodeError | Field = {
            if schema.allowSkippedNullableFields then
              val expectedBeforeSkip =
                if fieldIndex < fields.length then fields(fieldIndex) else null
              fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, actualName)

              if fieldIndex >= fields.length then
                if expectedBeforeSkip == null then
                  DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                else DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
              else
                val expectedField = fields(fieldIndex)
                if actualName != expectedField.name then
                  DecodeError.FieldOrderMismatch(expectedField.name, actualName)
                else expectedField
            else if parsedFieldIndex >= fields.length then
              DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
            else
              val expectedField = fields(parsedFieldIndex)
              if actualName != expectedField.name then
                DecodeError.FieldOrderMismatch(expectedField.name, actualName)
              else expectedField
          }
          validated match
            case expectedField: Field =>
              val value = decodeBase(expectedField.schema, fieldExpr.value)
                .mapErr(_.atPath(s".${expectedField.name}"))
                .ok
              values(fieldIndex) = value.asInstanceOf[AnyRef]
              fieldIndex += 1
            case err: DecodeError => raise(err.atPath(s".$actualName"))
          index += 1

        if schema.allowSkippedNullableFields then
          fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, "")

        val payloadFieldCount = fieldExprs.length - offset
        val decodedFieldCount =
          if schema.allowSkippedNullableFields then fieldIndex else payloadFieldCount
        if decodedFieldCount != fields.length then
          raise(DecodeError.FieldCountMismatch(fields.length, payloadFieldCount))

        read.build(values)
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

private[scalanotation] object TokenDecoder:

  private[scalanotation] def decode[T](
      input: String,
      debugTokens: Boolean,
      rootName: String,
      packageName: String,
      decoder: Reader[T]
  ): Result[T, DecodeError] =
    catchingTokenErrors(input):
      TokenDecoder(input, debugTokens).decodeRoot(decoder, rootName, packageName)

  private[scalanotation] def decodeAnyRoot[T](
      input: String,
      debugTokens: Boolean,
      packageName: String,
      decoder: Reader[T]
  ): Result[Expr.SourceFile[T], DecodeError] =
    catchingTokenErrors(input):
      TokenDecoder(input, debugTokens).decodeAnyRoot(decoder, packageName)

  private[scalanotation] def decodeExpression[T](
      input: String,
      debugTokens: Boolean,
      decoder: Reader[T]
  ): Result[T, DecodeError] =
    catchingTokenErrors(input):
      TokenDecoder(input, debugTokens).decodeExpression(decoder)

  /** tokens are scanned lazily while decoding, so malformed input can surface anywhere in the
    * decode as a [[TokenizeException]] — converted to a [[DecodeError]] here.
    */
  private inline def catchingTokenErrors[A](input: String)(
      inline body: => Result[A, DecodeError]
  ): Result[A, DecodeError] =
    try body
    catch
      case e: TokenizeException =>
        Result.Err(DecodeError.TokenFormat(e.message).atToken(Tokenizer.spanAt(input, e.offset)))

  private[scalanotation] def isNullable(schema: RawSchema): Boolean =
    schema match
      case RawSchema.Option(_)       => true
      case RawSchema.Mapped(base, _) => isNullable(base)
      case _                         => false

  private[scalanotation] def describe(expr: Expr): String =
    expr match
      case Expr.VectorExpr(_)       => "Vector(...)"
      case Expr.TupleExpr(elements) =>
        RawSchema.describeTupleSlots(elements.length)
      case Expr.NamedTupleExpr(fieldExprs) =>
        if fieldExprs.isEmpty then "NamedTuple.Empty" else s"(${fieldExprs.head.name} = ...)"
      case Expr.StringConstant(value)  => s"""("$value": String)"""
      case Expr.CharConstant(value)    => s"('$value': Char)"
      case Expr.IntConstant(value)     => s"($value: Int)"
      case Expr.LongConstant(value)    => s"($value: Long)"
      case Expr.FloatConstant(value)   => s"($value: Float)"
      case Expr.DoubleConstant(value)  => s"($value: Double)"
      case Expr.BooleanConstant(value) => s"($value: Boolean)"
      case Expr.NullConstant           => "null"

/** Reusable buffer for named-tuple parse results: the closing token is recorded as an unboxed Int
  * offset; a Span is only materialized if an error is constructed.
  */
private final class NamedTupleParseResult() {
  var fieldCount: Int          = uninitialized
  var fieldName: String | Null = uninitialized
  var closingOffset: Int       = uninitialized

  def push(fieldCount: Int, fieldName: String | Null, closingOffset: Int): this.type =
    this.fieldCount = fieldCount
    this.fieldName = fieldName
    this.closingOffset = closingOffset
    this
}

private final class TokenDecoder(input: String, debug: Boolean) extends TokenStream(input, debug) {

  import scala.util.boundary.Label

  type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  private def expectedTypeAtCurrent(schema: RawSchema): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  private def fillSkippedNullableFields(
      fields: IArray[Field],
      values: Array[AnyRef],
      startIndex: Int,
      actualName: String
  ): Int =
    var index = startIndex
    while index < fields.length
      && fields(index).name != actualName
      && TokenDecoder.isNullable(fields(index).schema)
    do
      values(index) = None.asInstanceOf[AnyRef]
      index += 1
    index

  def decodeRoot[T](
      schema: Reader[T],
      rootName: String,
      packageName: String
  ): Result[T, DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      expectVal().check
      val declaredName = expectIdentifier().ok
      if declaredName != rootName then raise(DecodeError.UnexpectedRoot(declaredName))
      expectEquals().check
      val value = decodeTaggedAs(schema).ok
      expectEof().check
      value

  def decodeAnyRoot[T](
      schema: Reader[T],
      packageName: String
  ): Result[Expr.SourceFile[T], DecodeError] =
    Result:
      expectPackageStatement(packageName).check
      expectVal().check
      val declaredName = expectIdentifier().ok
      expectEquals().check
      val value = decodeTaggedAs(schema).ok
      expectEof().check
      Expr.SourceFile(Map(declaredName -> value))

  def decodeExpression[T](schema: Reader[T]): Result[T, DecodeError] =
    Result:
      val value = decodeTaggedAs(schema).ok
      expectEof().check
      value

  private def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  private[scalanotation] def decodeTaggedAs[T](reader: Reader[T]): Result[T, DecodeError] =
    decodeBase(reader.schema).asInstanceOf[Result[T, DecodeError]]

  private def decodeBase(schema: RawSchema): Result[Any, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapped.mapping.mapResult(decodeBase(mapped.base))
      case sc: RawSchema.NamedTuple =>
        decodeNamedTuple(sc)
      case sc: RawSchema.Tuple =>
        decodeTuple(sc)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        decodePartialNamedTuple(base, alreadySeenField)
      case sc: RawSchema.Sum =>
        decodeSum(sc)
      case sc: RawSchema.DiscriminatorSum =>
        decodeDiscriminatorSum(sc)
      case sc: RawSchema.Vector =>
        decodeVector(sc)
      case sc: RawSchema.Dict =>
        decodeDict(sc)
      case sc: RawSchema.Option =>
        decodeOption(sc)
      case RawSchema.AnyExpr =>
        decodeAnyExpr()
      case RawSchema.String =>
        decodeString(identity)
      case RawSchema.Char =>
        decodeChar(identity)
      case RawSchema.Int =>
        decodeInt(identity)
      case RawSchema.Long =>
        decodeLong(identity)
      case RawSchema.Float =>
        decodeFloat(identity)
      case RawSchema.Double =>
        decodeDouble(identity)
      case RawSchema.Boolean =>
        decodeBoolean(identity)
      case RawSchema.Null =>
        decodeNull(identity)

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple
  ): Result[Any, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result {
      val read = schema.read
      if read == null then missingReadCapability(schema)
      schema.isValidNamedTuple(namesPool).check
      val fields     = schema.fields
      val values     = new Array[AnyRef](fields.length)
      var fieldIndex = 0

      val allowEmpty =
        fields.isEmpty // FIXME: must be hoisted to allow inlining parseNamedTupleStructure!

      val parsed = parseNamedTupleStructure(schema, allowEmpty = allowEmpty) {
        (actualName, nameOffset, parsedFieldIndex) =>
          def actualFieldErr(err: DecodeError): DecodeError =
            err.atPath(s".${actualName}").atToken(spanAt(nameOffset))
          val validated: DecodeError | Field = eval {
            if seenNames.alreadySeen(actualName) then
              actualFieldErr(DecodeError.DuplicateField(actualName))
            else if schema.allowSkippedNullableFields then
              val expectedBeforeSkip =
                if fieldIndex < fields.length then fields(fieldIndex) else null
              fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, actualName)

              if fieldIndex >= fields.length then
                if expectedBeforeSkip == null then
                  actualFieldErr(
                    DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                  )
                else
                  actualFieldErr(
                    DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
                  )
              else
                val expectedField = fields(fieldIndex)
                if actualName != expectedField.name then
                  actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                else expectedField
            else if parsedFieldIndex >= fields.length then
              actualFieldErr(DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1))
            else
              val expectedField = fields(parsedFieldIndex)
              if actualName != expectedField.name then
                actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
              else expectedField
          }
          validated match
            case expectedField: Field =>
              def decoded = decodeBase(expectedField.schema).mapErr(actualFieldErr)
              val value   = decoded.ok
              values(fieldIndex) = value.asInstanceOf[AnyRef]
              fieldIndex += 1
            case err: DecodeError => raise(err)
      }

      if schema.allowSkippedNullableFields && fields.nonEmpty && parsed.fieldCount == 0 then
        raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))

      if schema.allowSkippedNullableFields then
        fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, "")

      val decodedFieldCount =
        if schema.allowSkippedNullableFields then fieldIndex else parsed.fieldCount
      if decodedFieldCount != fields.length then
        def err =
          var err0 = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
          if parsed.fieldName != null then err0 = err0.atPath(s".${parsed.fieldName}")
          err0.atToken(spanAt(parsed.closingOffset))
        raise(err)

      read.build(values)
    }
  }

  private def decodeTuple(schema: RawSchema.Tuple): Result[Any, DecodeError] =
    Result {
      val read = schema.read
      if read == null then missingReadCapability(schema)
      val slots = schema.slots
      var state = read.init(slots.length)
      currentKind() match
        case TokenKind.EmptyTupleId =>
          val emptyTupleOffset = currentOffset()
          advance()
          if slots.nonEmpty then
            raise(DecodeError.FieldCountMismatch(slots.length, 0).atToken(spanAt(emptyTupleOffset)))
        case TokenKind.LParen =>
          state = decodeParenthesizedTuple(read)(schema, slots, state).ok
        case _ =>
          if slots.isEmpty then
            raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()))
          val value = decodeTupleSlotValue(slots, index = 0, allowStringConcat = false).ok
          state = read.add(state, 0, value)
          state = decodeTupleConsTail(read)(slots, state, startIndex = 1).ok
      read.finish(state)
    }

  private def decodeParenthesizedTuple(
      read: RawSchema.TupleRead
  )(
      schema: RawSchema.Tuple,
      slots: IArray[RawSchema],
      state: read.State
  ): Result[read.State, DecodeError] =
    Result:
      var result                   = state
      val (hasComma, hasStarColon) = parenthesizedTupleSeparators()
      advanceTupleOpen(schema).check
      currentKind() match
        case TokenKind.RParen =>
          raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))
        case _ =>
          if slots.isEmpty then raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))
          val firstValue      = decodeTupleSlotValue(slots, index = 0, hasComma || !hasStarColon).ok
          val stateAfterFirst = read.add(state, 0, firstValue)
          currentKind() match
            case TokenKind.Comma =>
              result = decodeTupleCommaTail(read)(slots, stateAfterFirst, startCount = 1).ok
            case TokenKind.StarColon =>
              val stateAfterTail =
                decodeTupleConsTail(read)(slots, stateAfterFirst, startIndex = 1).ok
              currentKind() match
                case TokenKind.RParen =>
                  advanceTupleClose().check
                  result = stateAfterTail
                case _ =>
                  raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
            case TokenKind.RParen =>
              val rparenOffset = currentOffset()
              advanceTupleClose().check
              currentKind() match
                case TokenKind.StarColon =>
                  result = decodeTupleConsTail(read)(slots, stateAfterFirst, startIndex = 1).ok
                case _ =>
                  raise(
                    DecodeError
                      .ExpectedType(schema.describeSelf, "(...)")
                      .atToken(spanAt(rparenOffset))
                  )
            case _ =>
              raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
      result

  private def decodeTupleCommaTail(
      read: RawSchema.TupleRead
  )(
      slots: IArray[RawSchema],
      state0: read.State,
      startCount: Int
  ): Result[read.State, DecodeError] =
    Result:
      var state              = state0
      var count              = startCount
      var done               = false
      var closingOffset: Int = currentOffset()
      while !done do
        currentKind() match
          case TokenKind.Comma =>
            advanceTupleComma().check
            currentKind() match
              case TokenKind.RParen =>
                closingOffset = currentOffset()
                if count == 1 then
                  raise(DecodeError.FieldCountMismatch(2, 1).atToken(spanAt(closingOffset)))
                done = true
              case _ =>
                if count >= slots.length then
                  raise(
                    DecodeError
                      .FieldCountMismatch(slots.length, count + 1)
                      .atToken(currentSpan())
                  )
                val value = decodeTupleSlotValue(slots, count, allowStringConcat = true).ok
                state = read.add(state, count, value)
                count += 1
          case TokenKind.RParen =>
            closingOffset = currentOffset()
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      advanceTupleClose().check
      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1).atToken(spanAt(closingOffset)))
      if count != slots.length then
        raise(DecodeError.FieldCountMismatch(slots.length, count).atToken(spanAt(closingOffset)))
      state

  private def decodeTupleConsTail(
      read: RawSchema.TupleRead
  )(
      slots: IArray[RawSchema],
      state0: read.State,
      startIndex: Int
  ): Result[read.State, DecodeError] =
    Result:
      var state = state0
      var index = startIndex
      var done  = false
      while !done do
        advanceTupleConsSeparator().check
        currentKind() match
          case TokenKind.EmptyTupleId =>
            advanceTupleEmptyTail(slots.length, index).check
            done = true
          case _ =>
            if index >= slots.length then
              raise(
                DecodeError
                  .FieldCountMismatch(slots.length, index + 1)
                  .atToken(currentSpan())
              )
            val value = decodeTupleSlotValue(slots, index, allowStringConcat = false).ok
            state = read.add(state, index, value)
            index += 1
      state

  private def decodeTupleSlotValue(
      slots: IArray[RawSchema],
      index: Int,
      allowStringConcat: Boolean
  ): Result[Any, DecodeError] =
    decodeTupleElement(slots(index), allowStringConcat)
      .mapErr(_.atPath(s"[$index]"))

  private def advanceTupleOpen(schema: RawSchema.Tuple): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )

  private def advanceTupleClose(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def advanceTupleComma(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.Comma then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

  private def advanceTupleConsSeparator(): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.StarColon then advance()
      else raise(DecodeError.ExpectedType("'*:'", describeCurrent()).atToken(currentSpan()))

  private def advanceTupleEmptyTail(
      expectedSlots: Int,
      actualSlots: Int
  ): Result[Unit, DecodeError] =
    Result.task:
      if currentKind() == TokenKind.EmptyTupleId then
        val emptyTupleOffset = currentOffset()
        advance()
        if actualSlots != expectedSlots then
          raise(
            DecodeError
              .FieldCountMismatch(expectedSlots, actualSlots)
              .atToken(spanAt(emptyTupleOffset))
          )
      else raise(DecodeError.ExpectedType("'EmptyTuple'", describeCurrent()).atToken(currentSpan()))

  private def decodeTupleElement(
      schema: RawSchema,
      allowStringConcat: Boolean
  ): Result[Any, DecodeError] =
    schema match
      case RawSchema.Mapped(base, mapping) =>
        mapping.mapResult(decodeTupleElement(base, allowStringConcat))
      case opt @ RawSchema.Option(inner) =>
        if currentKind() == TokenKind.NullKw then decodeOption(opt)
        else decodeTupleElement(inner, allowStringConcat).map(Some(_))
      case RawSchema.String if !allowStringConcat =>
        decodeStringAtom()
      case _ if currentKind() == TokenKind.LParen && !canDecodeFromLParen(schema) =>
        decodeGroupedTupleElement(schema)
      case _ =>
        decodeBase(schema)

  private def decodeGroupedTupleElement(schema: RawSchema): Result[Any, DecodeError] =
    Result {
      if currentKind() == TokenKind.LParen then advance()
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      val bufValue = decodeTupleElement(schema, allowStringConcat = true)
      if currentKind() == TokenKind.RParen then
        advance()
        Result.eval.break(bufValue)
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
    }

  private def canDecodeFromLParen(schema: RawSchema): Boolean =
    schema match
      case RawSchema.Mapped(base, _)      => canDecodeFromLParen(base)
      case RawSchema.Option(inner)        => canDecodeFromLParen(inner)
      case _: RawSchema.NamedTuple        => true
      case _: RawSchema.Tuple             => true
      case _: RawSchema.PartialNamedTuple => true
      case _: RawSchema.Sum               => true
      case _: RawSchema.DiscriminatorSum  => true
      case _: RawSchema.Dict              => true
      case RawSchema.AnyExpr              => true
      case _                              => false

  /** Scans ahead (without buffering) from the current '(' to its matching ')' to discover which
    * separators the parenthesized tuple uses. Uses a scout scanner so the bounded token buffer of
    * the stream is preserved; no tokens are materialized.
    */
  private def parenthesizedTupleSeparators(): (hasComma: Boolean, hasStarColon: Boolean) =
    var depth        = 0
    var sawOpen      = false
    var done         = false
    var hasComma     = false
    var hasStarColon = false
    val scout        = scoutFromCurrent()
    while !done do
      scout.scanNext()
      scout.kind match
        case TokenKind.LParen =>
          depth += 1
          sawOpen = true
        case TokenKind.RParen if sawOpen =>
          depth -= 1
          if depth == 0 then done = true
        case TokenKind.Comma if depth == 1 =>
          hasComma = true
        case TokenKind.StarColon if depth == 1 =>
          hasStarColon = true
        case TokenKind.Eof =>
          done = true
        case _ => ()
    (hasComma, hasStarColon)

  private def decodeSum(schema: RawSchema.Sum): Result[Any, DecodeError] =
    Result {
      var decoded: Any = null
      val parsed       = parseNamedTupleStructure(schema, allowEmpty = false) {
        (actualName, nameOffset, fieldIndex) =>
          if fieldIndex >= 1 then
            raise(
              DecodeError
                .FieldCountMismatch(1, fieldIndex + 1)
                .atPath(s".${actualName}")
                .atToken(spanAt(nameOffset))
            )
          else
            val sumCase = schema.cases.iterator.find(_.name == actualName) match
              case Some(c) => c
              case _       =>
                raise(
                  DecodeError
                    .UnexpectedField(actualName)
                    .atPath(s".${actualName}")
                    .atToken(spanAt(nameOffset))
                )
            decoded = decodeBase(sumCase.schema)
              .mapErr(_.atPath(s".${actualName}"))
              .ok
      }
      if parsed.fieldCount != 1 then
        var err = DecodeError.FieldCountMismatch(1, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(spanAt(parsed.closingOffset)))
      decoded
    }

  private def decodeDiscriminatorSum(schema: RawSchema.DiscriminatorSum): Result[Any, DecodeError] =
    Result {
      val discriminatorField = schema.discriminatorField
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )
      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      val nameOffset = currentOffset()
      val actualName = parseNamedFieldStart().ok
      if actualName != discriminatorField then
        raise(
          DecodeError
            .FieldOrderMismatch(discriminatorField, actualName)
            .atPath(s".$actualName")
            .atToken(spanAt(nameOffset))
        )

      val caseName = decodeString(identity).mapErr(_.atPath(s".$actualName")).ok
      val sumCase  = schema.cases.iterator.find(_.name == caseName) match
        case Some(c) => c
        case _       =>
          raise(
            DecodeError
              .UnexpectedField(caseName)
              .atPath(s".$actualName")
              .atToken(spanAt(nameOffset))
          )

      currentKind() match
        case TokenKind.Comma  => advance()
        case TokenKind.RParen =>
        case _                =>
          raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      Result.eval.break(decodeBase(sumCase.schema))
    }

  private def decodePartialNamedTuple(
      schema: RawSchema,
      alreadySeenField: String
  ): Result[Any, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapped.mapping.mapResult(decodePartialNamedTuple(mapped.base, alreadySeenField))
      case namedTuple: RawSchema.NamedTuple =>
        decodePartialNamedTuple(namedTuple, alreadySeenField)
      case RawSchema.Null =>
        decodeEmptyPartialNamedTuple()
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeCurrent()))

  private def decodeEmptyPartialNamedTuple(): Result[Any, DecodeError] =
    Result:
      if currentKind() == TokenKind.RParen then
        advance()
        null
      else raise(DecodeError.FieldCountMismatch(0, 1).atToken(currentSpan()))

  private def decodePartialNamedTuple(
      schema: RawSchema.NamedTuple,
      alreadySeenField: String
  ): Result[Any, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result:
      val read = schema.read
      if read == null then missingReadCapability(schema)
      schema.isValidNamedTuple(namesPool).check
      seenNames.alreadySeen(alreadySeenField)
      val fields     = schema.fields
      val values     = new Array[AnyRef](fields.length)
      var fieldIndex = 0

      val parsed = parsePartialNamedTupleStructure(schema) {
        (actualName, nameOffset, parsedFieldIndex) =>
          def actualFieldErr(err: DecodeError): DecodeError =
            err.atPath(s".${actualName}").atToken(spanAt(nameOffset))
          val validated: DecodeError | Field = eval {
            if seenNames.alreadySeen(actualName) then
              actualFieldErr(DecodeError.DuplicateField(actualName))
            else if schema.allowSkippedNullableFields then
              val expectedBeforeSkip =
                if fieldIndex < fields.length then fields(fieldIndex) else null
              fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, actualName)

              if fieldIndex >= fields.length then
                if expectedBeforeSkip == null then
                  actualFieldErr(
                    DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                  )
                else
                  actualFieldErr(
                    DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
                  )
              else
                val expectedField = fields(fieldIndex)
                if actualName != expectedField.name then
                  actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                else expectedField
            else if parsedFieldIndex >= fields.length then
              actualFieldErr(DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1))
            else
              val expectedField = fields(parsedFieldIndex)
              if actualName != expectedField.name then
                actualFieldErr(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
              else expectedField
          }
          validated match
            case expectedField: Field =>
              val value = decodeBase(expectedField.schema).mapErr(actualFieldErr).ok
              values(fieldIndex) = value.asInstanceOf[AnyRef]
              fieldIndex += 1
            case err: DecodeError => raise(err)
      }

      if schema.allowSkippedNullableFields then
        fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, "")

      val decodedFieldCount =
        if schema.allowSkippedNullableFields then fieldIndex else parsed.fieldCount
      if decodedFieldCount != fields.length then
        var err = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(spanAt(parsed.closingOffset)))

      read.build(values)
  }

  private def decodeVector(schema: RawSchema.Vector): Result[Any, DecodeError] =
    Result {
      val read = schema.read
      if read == null then missingReadCapability(schema)
      var values = read.init()
      parseVectorStructure(schema) { indexInVector =>
        val value = decodeBase(schema.element)
          .mapErr(_.atPath(s"[$indexInVector]"))
          .ok
        values = read.add(values, value)
      }
      read.finish(values)
    }

  private def decodeDict(schema: RawSchema.Dict): Result[Any, DecodeError] =
    namesPool.withBorrowed { seenNames =>
      Result {
        val read = schema.read
        if read == null then missingReadCapability(schema)
        var state  = read.init()
        val parsed = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameOffset, _) =>
          if seenNames.alreadySeen(name) then
            raise(DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset)))
          val elem = decodeBase(schema.element).mapErr(_.atPath(s".${name}")).ok
          state = read.add(state, name, elem)
        }
        val _ = parsed.closingOffset
        val _ = parsed.fieldName
        val _ = parsed.fieldCount
        read.finish(state)
      }
    }

  private def decodeOption(schema: RawSchema.Option): Result[Any, DecodeError] =
    Result {
      if currentKind() == TokenKind.NullKw then
        advance()
        None
      else Some(decodeBase(schema.inner).ok)
    }

  private object exprVisitor:
    private val AnyNamedTupleSchema: RawSchema.NamedTuple =
      RawSchema.NamedTuple(IArray.empty[RawSchema.Field], read = null, write = null)
    private val AnyTupleSchema: RawSchema.Tuple =
      RawSchema.Tuple(IArray.empty[RawSchema], read = null, write = null)
    private val AnyVectorSchema: RawSchema.Vector =
      RawSchema.Vector(
        RawSchema.AnyExpr,
        read = null,
        write = null
      )
    private val EmptyTupleExpr: Expr =
      Expr.TupleExpr(Vector.empty)

    def inferExpr(): Result[Expr, DecodeError] =
      onStringConcat()

    private def onStringConcat(): Result[Expr, DecodeError] = Result {
      val first = onTupleCons().ok
      if currentKind() == TokenKind.Plus then
        val builder = first match
          case Expr.StringConstant(value) => new StringBuilder ++= value
          case other                      =>
            raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
        while currentKind() == TokenKind.Plus do
          advance()
          onTupleCons().ok match
            case Expr.StringConstant(value) => builder ++= value
            case other                      =>
              raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
        Expr.StringConstant(builder.result())
      else first
    }

    private def onTupleCons(): Result[Expr, DecodeError] = Result {
      val head = onPrimary().ok
      if currentKind() == TokenKind.StarColon then
        advance()
        onTupleCons().ok match
          case Expr.TupleExpr(elements) =>
            Expr.TupleExpr(head +: elements)
          case other =>
            raise(DecodeError.ExpectedType("Tuple", describe(other)))
      else head
    }

    private def onPrimary(): Result[Expr, DecodeError] =
      currentKind() match
        case TokenKind.LParen                     => onParenthesized()
        case TokenKind.VectorId                   => onVector(AnyVectorSchema)
        case TokenKind.EmptyTupleId               => onEmptyTuple()
        case TokenKind.StringLit                  => onString()
        case TokenKind.CharLit                    => onChar()
        case TokenKind.IntLit                     => onInt()
        case TokenKind.LongLit                    => onLong()
        case TokenKind.FloatLit                   => onFloat()
        case TokenKind.DoubleLit                  => onDouble()
        case TokenKind.TrueKw | TokenKind.FalseKw => onBoolean()
        case TokenKind.NullKw                     => onNull()
        case TokenKind.Minus                      =>
          peekKind() match
            case TokenKind.IntLit    => onInt()
            case TokenKind.LongLit   => onLong()
            case TokenKind.FloatLit  => onFloat()
            case TokenKind.DoubleLit => onDouble()
            case _                   =>
              Result.Err(DecodeError.ExpectedType("Number", describePeek()).atToken(peekSpan()))
        case _ =>
          Result.Err(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

    def onParenthesized(): Result[Expr, DecodeError] = Result {
      if currentKind() == TokenKind.LParen then advance()
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then
        raise(DecodeError.UnitValueNotAllowed().atToken(currentSpan()))

      if peekKind() == TokenKind.Equals then onNamedTupleAfterOpen(AnyNamedTupleSchema).ok
      else
        val first = inferExpr().ok
        currentKind() match
          case TokenKind.Comma =>
            onTupleAfterGroupedHead(first).ok
          case TokenKind.RParen =>
            advance()
            first
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
    }

    def onTupleAfterGroupedHead(first: Expr): Result[Expr, DecodeError] = Result {
      val elements = IArray.newBuilder[Expr]
      elements += first
      var count = 1
      var done  = false
      while !done do
        currentKind() match
          case TokenKind.Comma =>
            advance()
            currentKind() match
              case TokenKind.RParen =>
                if count == 1 then
                  raise(DecodeError.FieldCountMismatch(2, 1).atToken(currentSpan()))
                done = true
              case _ =>
                elements += inferExpr().ok
                count += 1
          case TokenKind.RParen =>
            done = true
          case _ =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1))
      Expr.TupleExpr(elements.result())
    }

    def onNamedTuple(schema: RawSchema.NamedTuple): Result[Expr, DecodeError] = Result {
      if currentKind() == TokenKind.LParen then advance()
      else
        raise(
          DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())
        )
      Result.eval.break(onNamedTupleAfterOpen(schema))
    }

    def onNamedTupleAfterOpen(schema: RawSchema.NamedTuple): Result[Expr, DecodeError] =
      namesPool.withBorrowed { seenNames =>
        Result {
          val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
          val allowEmpty = false
          val parsed     =
            parseNamedTupleStructureAfterOpen(schema, allowEmpty) { (name, nameOffset, _) =>
              if seenNames.alreadySeen(name) then
                raise(
                  DecodeError.DuplicateField(name).atPath(s".${name}").atToken(spanAt(nameOffset))
                )
              val elem = inferExpr().mapErr(_.atPath(s".${name}")).ok
              fieldExprs += ((name, elem))
            }
          val _ = parsed.closingOffset
          val _ = parsed.fieldName
          val _ = parsed.fieldCount
          Expr.NamedTupleExpr(fieldExprs.result())
        }
      }

    def onVector(schema: RawSchema.Vector): Result[Expr, DecodeError] = Result {
      val elements = IArray.newBuilder[Expr]
      parseVectorStructure(schema) { _ =>
        elements += inferExpr().ok
      }
      Expr.VectorExpr(elements.result())
    }

    def onString(): Result[Expr, DecodeError] = decodeString(Expr.StringConstant.apply)

    def onChar(): Result[Expr, DecodeError] = decodeChar(Expr.CharConstant.apply)

    def onInt(): Result[Expr, DecodeError] = decodeInt(Expr.IntConstant.apply)

    def onLong(): Result[Expr, DecodeError] = decodeLong(Expr.LongConstant.apply)

    def onFloat(): Result[Expr, DecodeError] = decodeFloat(Expr.FloatConstant.apply)

    def onDouble(): Result[Expr, DecodeError] = decodeDouble(Expr.DoubleConstant.apply)

    def onBoolean(): Result[Expr, DecodeError] = decodeBoolean(Expr.BooleanConstant.apply)

    def onNull(): Result[Expr, DecodeError] = decodeNull(_ => Expr.NullConstant)

    def onEmptyTuple(): Result[Expr, DecodeError] = Result:
      if currentKind() == TokenKind.EmptyTupleId then
        advance()
        EmptyTupleExpr
      else raise(DecodeError.ExpectedExpression(describeCurrent()).atToken(currentSpan()))

  private[scalanotation] def decodeAnyExpr(): Result[Expr, DecodeError] =
    exprVisitor.inferExpr()

  // reusable (per-decoder) buffer for named-tuple parse results — no allocation in the happy path
  private val namedTupleParseResult = new NamedTupleParseResult()

  @deprecated("Kept for binary compatibility; will be removed in a future version", "0.3.6")
  private class NamedTupleParseResultBuf() {
    // retained only for binary compatibility — superseded by NamedTupleParseResult above
    var fieldCount: Int               = uninitialized
    var fieldName: String | Null      = uninitialized
    var closingSpan: DecodeError.Span = uninitialized
  }

  @deprecated("Kept for binary compatibility; will be removed in a future version", "0.3.6")
  private object NamedTupleParseResultBuf extends NamedTupleParseResultBuf() {
    def push(fieldCount: Int, fieldName: String | Null, closingSpan: DecodeError.Span): this.type =
      this.fieldCount = fieldCount
      this.fieldName = fieldName
      this.closingSpan = closingSpan
      this
  }

  private inline def eval[T](inline op: T): T =
    def exprToEval(): T = op
    exprToEval()

  private def parseNamedFieldStart(): Result[String, DecodeError] =
    Result:
      val actualName = currentKind() match
        case TokenKind.Identifier   => currentName()
        case TokenKind.VectorId     => "Vector"
        case TokenKind.EmptyTupleId => "EmptyTuple"
        case TokenKind.Plus         => "+"
        case TokenKind.Minus        => "-"
        case TokenKind.StarColon    => "*:"
        case _                      =>
          raise(DecodeError.ExpectedFieldName(describeCurrent()).atToken(currentSpan()))
      advance()
      if currentKind() == TokenKind.Equals then
        advance()
        actualName
      else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  private inline def parseNamedTupleStructure(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    if currentKind() == TokenKind.LParen then advance()
    else
      raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan()))

    // parseNamedTupleStructureAfterOpen(schema, allowEmpty)(consumeFieldValue)
    val parsed: NamedTupleParseResult =
      parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
        case parsed: NamedTupleParseResult => parsed
        case err: Result.Err[DecodeError]  =>
          scala.util.boundary.break(err) // TODO: replace with Result.breakErr

    if !allowEmpty && parsed.fieldCount == 0 then
      raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))
    parsed
  }

  private inline def parseNamedTupleStructureAfterOpen(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    val parsed: NamedTupleParseResult =
      parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
        case parsed: NamedTupleParseResult => parsed
        case err: Result.Err[DecodeError]  =>
          scala.util.boundary.break(err) // TODO: replace with Result.breakErr

    if !allowEmpty && parsed.fieldCount == 0 then
      raise(DecodeError.UnitValueNotAllowed().atToken(spanAt(parsed.closingOffset)))
    parsed
  }

  private inline def parsePartialNamedTupleStructure(
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = {
    parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
      case parsed: NamedTupleParseResult => parsed
      case err: Result.Err[DecodeError]  =>
        scala.util.boundary.break(err) // TODO: replace with Result.breakErr
  }

  private inline def parsePartialNamedTupleStructureInner(
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, Int, Int) => Unit, DecodeError]
  ): NamedTupleParseResult | Result.Err[DecodeError] =
    // to share the logic without breaking the label optimisation, we need to cache the result and
    // then redispatch the break at the call-site. i.e. nested inline calls dont seem to compose
    // well enough to pass along the label. i would like to investigate why.
    eval {
      scala.util.boundary {
        import Internal.loop

        currentKind() match {
          case TokenKind.RParen =>
            val closingOffset = currentOffset()
            advance()
            namedTupleParseResult.push(0, null, closingOffset)
          case _ =>
            var fieldIndex: Int              = 0
            var lastFieldName: String | Null = null
            val closingOffset: Int           = loop {
              val nameOffset = currentOffset()
              val actualName = parseNamedFieldStart().ok
              consumeFieldValue(actualName, nameOffset, fieldIndex)
              lastFieldName = actualName
              fieldIndex += 1

              currentKind() match
                case TokenKind.Comma =>
                  advance()
                  if currentKind() == TokenKind.RParen then loop.break(currentOffset())
                case TokenKind.RParen =>
                  loop.break(currentOffset())
                case _ =>
                  raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
            }
            advance()
            namedTupleParseResult.push(fieldIndex, lastFieldName, closingOffset)
        }
      }
    }

  private inline def parseVectorStructure(schema: RawSchema)(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    if currentKind() == TokenKind.VectorId && peekKind() == TokenKind.LParen then
      advance()
      advance()
    else
      raise(DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan()))

    var indexInVector = 0

    if currentKind() == TokenKind.RParen then advance()
    else
      var done = false
      while !done do
        consumeElementValue(indexInVector)
        indexInVector += 1

        currentKind() match
          case TokenKind.Comma =>
            advance()
            if currentKind() == TokenKind.RParen then done = true
          case TokenKind.RParen => done = true
          case _                =>
            raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))

      if currentKind() == TokenKind.RParen then advance()
      else raise(DecodeError.ExpectedRParen(describeCurrent()).atToken(currentSpan()))
  }

  private[scalanotation] def decodeString[A](wrap: String => A): Result[A, DecodeError] =
    Result {
      val first  = decodeStringAtom().ok
      var isPlus = currentKind() == TokenKind.Plus
      if !isPlus then wrap(first)
      else
        val builder = StringBuilder() ++= first
        while isPlus do
          advance()
          builder ++= decodeStringAtom().ok
          isPlus = currentKind() == TokenKind.Plus
        wrap(builder.toString())
    }

  private def decodeStringAtom(): Result[String, DecodeError] = Result:
    if currentKind() == TokenKind.StringLit then
      val value = currentStringValue()
      advance()
      value
    else raise(expectedTypeAtCurrent(RawSchema.String))

  private[scalanotation] def decodeChar[A](wrap: Char => A): Result[A, DecodeError] = Result:
    if currentKind() == TokenKind.CharLit then
      val value = currentCharValue()
      advance()
      wrap(value)
    else raise(expectedTypeAtCurrent(RawSchema.Char))

  private[scalanotation] def decodeInt[A](wrap: Int => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = () =>
        currentKind() match
          case TokenKind.IntLit => currentIntValue()
          case _                => raise(expectedTypeAtCurrent(RawSchema.Int)),
      negator = -1,
      one = 1,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeLong[A](wrap: Long => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = () =>
        currentKind() match
          case TokenKind.LongLit => currentLongValue()
          case TokenKind.IntLit  => currentIntValue().toLong
          case _                 => raise(expectedTypeAtCurrent(RawSchema.Long)),
      negator = -1L,
      one = 1L,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeFloat[A](wrap: Float => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = () =>
        currentKind() match
          case TokenKind.FloatLit => currentFloatValue()
          case TokenKind.IntLit if NumericPromotions.isExactFloat(currentIntValue()) =>
            currentIntValue().toFloat
          case _ => raise(expectedTypeAtCurrent(RawSchema.Float)),
      negator = -1.0f,
      one = 1.0f,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeDouble[A](wrap: Double => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = () =>
        currentKind() match
          case TokenKind.DoubleLit => currentDoubleValue()
          case TokenKind.IntLit    => currentIntValue().toDouble
          case _                   => raise(expectedTypeAtCurrent(RawSchema.Double)),
      negator = -1.0d,
      one = 1.0d,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeBoolean[A](wrap: Boolean => A): Result[A, DecodeError] =
    Result:
      currentKind() match
        case TokenKind.TrueKw =>
          advance()
          wrap(true)
        case TokenKind.FalseKw =>
          advance()
          wrap(false)
        case _ =>
          raise(expectedTypeAtCurrent(RawSchema.Boolean))

  private[scalanotation] def decodeNull[A](wrap: Null => A): Result[A, DecodeError] = Result:
    if currentKind() == TokenKind.NullKw then
      advance()
      wrap(null)
    else raise(expectedTypeAtCurrent(RawSchema.Null))

  private inline def decodeSigned[N, A](
      inline literal: () => N,
      negator: N,
      one: N,
      prod: (N, N) => N,
      wrap: N => A
  ): A =
    val sign =
      if currentKind() == TokenKind.Minus then
        advance()
        negator
      else one
    val value = literal()
    advance()
    wrap(prod(sign, value))

  private def expectVal(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.ValKw then advance()
    else raise(DecodeError.ExpectedVal(describeCurrent()).atToken(currentSpan()))

  private def expectPackageStatement(packageName: String): Result[Unit, DecodeError] =
    Result.task:
      if packageName.nonEmpty then
        expectPackage().check
        val declaredName = expectQualifiedIdentifier().ok
        if declaredName != packageName then raise(DecodeError.UnexpectedPackage(declaredName))
        acceptStatementSeparator()

  private def acceptStatementSeparator(): Unit =
    if currentKind() == TokenKind.Semicolon then advance()

  private def expectPackage(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.PackageKw then advance()
    else raise(DecodeError.ExpectedPackage(describeCurrent()).atToken(currentSpan()))

  private def expectQualifiedIdentifier(): Result[String, DecodeError] = Result:
    val builder = new StringBuilder(expectIdentifier().ok)
    while currentKind() == TokenKind.Dot do
      advance()
      builder += '.'
      builder ++= expectIdentifier().ok
    builder.result()

  private def expectIdentifier(): Result[String, DecodeError] = Result:
    val name = currentKind() match
      case TokenKind.Identifier   => currentName()
      case TokenKind.VectorId     => "Vector"
      case TokenKind.EmptyTupleId => "EmptyTuple"
      case TokenKind.Plus         => "+"
      case TokenKind.Minus        => "-"
      case TokenKind.StarColon    => "*:"
      case _                      =>
        raise(DecodeError.ExpectedIdentifier(describeCurrent()).atToken(currentSpan()))
    advance()
    name

  private def expectEquals(): Result[Unit, DecodeError] = Result.task:
    if currentKind() == TokenKind.Equals then advance()
    else raise(DecodeError.ExpectedEquals(describeCurrent()).atToken(currentSpan()))

  private def expectEof(): Result[Unit, DecodeError] = Result.task:
    if currentKind() != TokenKind.Eof then
      raise(DecodeError.ExpectedEof(describeCurrent()).atToken(currentSpan()))
}
