package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.internal.RawSchema.Field
import scalanotation.internal.Token
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.ok
import steps.result.Result.eval.raise

import scala.annotation.constructorOnly
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
          decodeBase(sumCase.schema, value).mapErr(_.atPath(s".$caseName")).ok
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
          decodeBase(sumCase.schema, expr).ok
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
      tokens: List[Token],
      rootName: String,
      packageName: String,
      decoder: Reader[T]
  ): Result[T, DecodeError] =
    TokenDecoder(tokens).decodeRoot(decoder, rootName, packageName)

  private[scalanotation] def decodeAnyRoot[T](
      tokens: List[Token],
      packageName: String,
      decoder: Reader[T]
  ): Result[Expr.SourceFile[T], DecodeError] =
    TokenDecoder(tokens).decodeAnyRoot(decoder, packageName)

  private[scalanotation] def decodeExpression[T](
      tokens: List[Token],
      decoder: Reader[T]
  ): Result[T, DecodeError] =
    TokenDecoder(tokens).decodeExpression(decoder)

  private[scalanotation] def isNullable(schema: RawSchema): Boolean =
    schema match
      case RawSchema.Option(_)       => true
      case RawSchema.Mapped(base, _) => isNullable(base)
      case _                         => false

  private[scalanotation] def describe(token: Token | Expr): String =
    token match
      case t: Token => describe(t)
      case e: Expr  => describe(e)

  private[scalanotation] def describe(token: Token): String =
    token match
      case Token.PackageKw(_)         => "'package'"
      case Token.ValKw(_)             => "'val'"
      case Token.VectorId(_)          => "'Vector'"
      case Token.TrueKw(_)            => "'true'"
      case Token.FalseKw(_)           => "'false'"
      case Token.NullKw(_)            => "'null'"
      case Token.EmptyTupleId(_)      => "'EmptyTuple'"
      case Token.Keyword(raw, _)      => s"'$raw'"
      case Token.Identifier(name, _)  => s"identifier '$name'"
      case Token.IntLit(raw, _, _)    => s"integer literal '$raw'"
      case Token.LongLit(raw, _, _)   => s"long literal '$raw'"
      case Token.FloatLit(raw, _, _)  => s"float literal '$raw'"
      case Token.DoubleLit(raw, _, _) => s"double literal '$raw'"
      case Token.StringLit(raw, _, _) => s"string literal $raw"
      case Token.CharLit(raw, _, _)   => s"character literal '$raw'"
      case Token.Equals(_)            => "'='"
      case Token.Dot(_)               => "'.'"
      case Token.Plus(_)              => "'+'"
      case Token.Minus(_)             => "'-'"
      case Token.StarColon(_)         => "'*:'"
      case Token.Comma(_)             => "','"
      case Token.Semicolon(_)         => "';'"
      case Token.LParen(_)            => "'('"
      case Token.RParen(_)            => "')'"
      case Token.Eof(_)               => "end of input"

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

private class NamedTupleParseResult() {
  var fieldCount: Int               = uninitialized
  var fieldName: String | Null      = uninitialized
  var closingSpan: DecodeError.Span = uninitialized
}

private object NamedTupleParseResult {
  def push(
      fieldCount: Int,
      fieldName: String | Null,
      closingSpan: DecodeError.Span
  ): NamedTupleParseResult =
    val result = new NamedTupleParseResult()
    result.fieldCount = fieldCount
    result.fieldName = fieldName
    result.closingSpan = closingSpan
    result
}

private class TupleParseResult() {
  var elementCount: Int             = uninitialized
  var closingSpan: DecodeError.Span = uninitialized
}

private object TupleParseResult extends TupleParseResult {
  def push(
      elementCount: Int,
      closingSpan: DecodeError.Span
  ): this.type =
    this.elementCount = elementCount
    this.closingSpan = closingSpan
    this
}

private final class TokenDecoder(@constructorOnly tokens: List[Token])
    extends Internal.TokenStream(tokens) {

  import scala.util.boundary.Label

  type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  private def expectedType(schema: RawSchema, token: Token): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describe(token)).atToken(token.span)

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
        (actualName, nameSpan, parsedFieldIndex) =>
          def actualFieldErr(err: DecodeError): DecodeError =
            err.atPath(s".${actualName}").atToken(nameSpan)
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
        raise(DecodeError.UnitValueNotAllowed().atToken(parsed.closingSpan))

      if schema.allowSkippedNullableFields then
        fieldIndex = fillSkippedNullableFields(fields, values, fieldIndex, "")

      val decodedFieldCount =
        if schema.allowSkippedNullableFields then fieldIndex else parsed.fieldCount
      if decodedFieldCount != fields.length then
        def err =
          var err0 = DecodeError.FieldCountMismatch(fields.length, parsed.fieldCount)
          if parsed.fieldName != null then err0 = err0.atPath(s".${parsed.fieldName}")
          err0.atToken(parsed.closingSpan)
        raise(err)

      read.build(values)
    }
  }

  private def decodeTuple(schema: RawSchema.Tuple): Result[Any, DecodeError] =
    Result {
      val expr = exprVisitor.inferExpr().ok
      ExprDecoder().decodeRaw(schema, expr).ok
    }

  private def decodeSum(schema: RawSchema.Sum): Result[Any, DecodeError] =
    Result {
      var decoded: Any = null
      val parsed       = parseNamedTupleStructure(schema, allowEmpty = false) {
        (actualName, nameSpan, fieldIndex) =>
          if fieldIndex >= 1 then
            raise(
              DecodeError
                .FieldCountMismatch(1, fieldIndex + 1)
                .atPath(s".${actualName}")
                .atToken(nameSpan)
            )
          else
            val sumCase = schema.cases.iterator.find(_.name == actualName) match
              case Some(c) => c
              case _       =>
                raise(
                  DecodeError
                    .UnexpectedField(actualName)
                    .atPath(s".${actualName}")
                    .atToken(nameSpan)
                )
            decoded = decodeBase(sumCase.schema)
              .mapErr(_.atPath(s".${actualName}"))
              .ok
      }
      if parsed.fieldCount != 1 then
        var err = DecodeError.FieldCountMismatch(1, parsed.fieldCount)
        if parsed.fieldName != null then err = err.atPath(s".${parsed.fieldName}")
        raise(err.atToken(parsed.closingSpan))
      decoded
    }

  private def decodeDiscriminatorSum(schema: RawSchema.DiscriminatorSum): Result[Any, DecodeError] =
    Result {
      val discriminatorField = schema.discriminatorField
      currentToken() match
        case Token.LParen(_) => advance()
        case other           =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span))
      currentToken() match
        case rparen: Token.RParen => raise(DecodeError.UnitValueNotAllowed().atToken(rparen.span))
        case _                    => ()

      val (actualName, nameSpan) = parseNamedFieldStart().ok
      if actualName != discriminatorField then
        raise(
          DecodeError
            .FieldOrderMismatch(discriminatorField, actualName)
            .atPath(s".$actualName")
            .atToken(nameSpan)
        )

      val caseName = decodeString(identity).mapErr(_.atPath(s".$actualName")).ok
      val sumCase  = schema.cases.iterator.find(_.name == caseName) match
        case Some(c) => c
        case _       =>
          raise(
            DecodeError
              .UnexpectedField(caseName)
              .atPath(s".$actualName")
              .atToken(nameSpan)
          )

      currentToken() match
        case Token.Comma(_)  => advance()
        case _: Token.RParen =>
        case other           =>
          raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))

      decodeBase(sumCase.schema).ok
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
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describe(currentToken())))

  private def decodeEmptyPartialNamedTuple(): Result[Any, DecodeError] =
    Result:
      currentToken() match
        case _: Token.RParen =>
          advance()
          null
        case other =>
          raise(DecodeError.FieldCountMismatch(0, 1).atToken(other.span))

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
        (actualName, nameSpan, parsedFieldIndex) =>
          def actualFieldErr(err: DecodeError): DecodeError =
            err.atPath(s".${actualName}").atToken(nameSpan)
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
        raise(err.atToken(parsed.closingSpan))

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
        val parsed = parseNamedTupleStructure(schema, allowEmpty = false) { (name, nameSpan, _) =>
          if seenNames.alreadySeen(name) then
            raise(DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan))
          val elem = decodeBase(schema.element).mapErr(_.atPath(s".${name}")).ok
          state = read.add(state, name, elem)
        }
        val _ = parsed.closingSpan
        val _ = parsed.fieldName
        val _ = parsed.fieldCount
        read.finish(state)
      }
    }

  private def decodeOption(schema: RawSchema.Option): Result[Any, DecodeError] =
    Result {
      currentToken() match
        case Token.NullKw(_) =>
          advance()
          None
        case _ =>
          Some(decodeBase(schema.inner).ok)
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

    def inferExpr(): Result[Expr, DecodeError] =
      onStringConcat()

    private def onStringConcat(): Result[Expr, DecodeError] = Result {
      val first = onTupleCons().ok
      currentToken() match
        case Token.Plus(_) =>
          val builder = new StringBuilder
          first match
            case Expr.StringConstant(value) => builder ++= value
            case other                      =>
              raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
          while currentToken().isInstanceOf[Token.Plus] do
            advance()
            onTupleCons().ok match
              case Expr.StringConstant(value) => builder ++= value
              case other                      =>
                raise(DecodeError.ExpectedType(RawSchema.String.describeSelf, describe(other)))
          Expr.StringConstant(builder.result())
        case _ =>
          first
    }

    private def onTupleCons(): Result[Expr, DecodeError] = Result {
      val head = onPrimary().ok
      currentToken() match
        case Token.StarColon(_) =>
          advance()
          onTupleCons().ok match
            case Expr.TupleExpr(elements) =>
              Expr.TupleExpr(head +: elements)
            case other =>
              raise(DecodeError.ExpectedType("Tuple", describe(other)))
        case _ =>
          head
    }

    private def onPrimary(): Result[Expr, DecodeError] =
      currentToken() match
        case Token.LParen(_)                    => onParenthesized()
        case Token.VectorId(_)                  => onVector(AnyVectorSchema)
        case Token.EmptyTupleId(_)              => onEmptyTuple()
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
              Result.Err(DecodeError.ExpectedType("Number", describe(token)).atToken(token.span))
        case other =>
          Result.Err(DecodeError.ExpectedExpression(describe(other)).atToken(other.span))

    def onParenthesized(): Result[Expr, DecodeError] = Result {
      currentToken() match
        case Token.LParen(_) => advance()
        case other           =>
          raise(DecodeError.ExpectedExpression(describe(other)).atToken(other.span))

      currentToken() match
        case rparen @ Token.RParen(_) =>
          raise(DecodeError.UnitValueNotAllowed().atToken(rparen.span))
        case _ => ()

      if peekToken().isInstanceOf[Token.Equals] then onNamedTupleAfterOpen(AnyNamedTupleSchema).ok
      else
        val first = inferExpr().ok
        currentToken() match
          case Token.Comma(_) =>
            onTupleAfterGroupedHead(first).ok
          case Token.RParen(_) =>
            advance()
            first
          case other =>
            raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))
    }

    def onTupleAfterGroupedHead(first: Expr): Result[Expr, DecodeError] = Result {
      val elements = IArray.newBuilder[Expr]
      elements += first
      var count = 1
      var done  = false
      while !done do
        currentToken() match
          case Token.Comma(_) =>
            advance()
            currentToken() match
              case Token.RParen(_) =>
                if count == 1 then
                  raise(DecodeError.FieldCountMismatch(2, 1).atToken(currentToken().span))
                done = true
              case _ =>
                elements += inferExpr().ok
                count += 1
          case Token.RParen(_) =>
            done = true
          case other =>
            raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))

      currentToken() match
        case Token.RParen(_) =>
          advance()
        case other =>
          raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))

      if count == 1 then raise(DecodeError.FieldCountMismatch(2, 1))
      Expr.TupleExpr(elements.result())
    }

    def onNamedTuple(schema: RawSchema.NamedTuple): Result[Expr, DecodeError] =
      Result {
        currentToken() match
          case Token.LParen(_) => advance()
          case other           =>
            raise(
              DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span)
            )
        onNamedTupleAfterOpen(schema).ok
      }

    def onNamedTupleAfterOpen(schema: RawSchema.NamedTuple): Result[Expr, DecodeError] =
      namesPool.withBorrowed { seenNames =>
        Result {
          val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
          val parsed     =
            parseNamedTupleStructureAfterOpen(schema, allowEmpty = false) { (name, nameSpan, _) =>
              if seenNames.alreadySeen(name) then
                raise(DecodeError.DuplicateField(name).atPath(s".${name}").atToken(nameSpan))
              val elem = inferExpr().mapErr(_.atPath(s".${name}")).ok
              fieldExprs += ((name, elem))
            }
          val _ = parsed.closingSpan
          val _ = parsed.fieldName
          val _ = parsed.fieldCount
          Expr.NamedTupleExpr(fieldExprs.result())
        }
      }

    def onTupleAfterOpen(schema: RawSchema.Tuple): Result[Expr, DecodeError] = Result {
      val elements = IArray.newBuilder[Expr]
      val parsed   = parseTupleStructureAfterOpen(schema) { _ =>
        elements += inferExpr().ok
      }
      if parsed.elementCount == 1 then
        raise(DecodeError.FieldCountMismatch(2, 1).atToken(parsed.closingSpan))
      Expr.TupleExpr(elements.result())
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
      currentToken() match
        case Token.EmptyTupleId(_) =>
          advance()
          Expr.TupleExpr(IndexedSeq.empty)
        case other =>
          raise(DecodeError.ExpectedExpression(describe(other)).atToken(other.span))

  private[scalanotation] def decodeAnyExpr(): Result[Expr, DecodeError] =
    exprVisitor.inferExpr()

  private class NamedTupleParseResultBuf() {
    var fieldCount: Int               = uninitialized
    var fieldName: String | Null      = uninitialized
    var closingSpan: DecodeError.Span = uninitialized
  }

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

  private def parseNamedFieldStart(): Result[(String, DecodeError.Span), DecodeError] =
    Result:
      val (actualName, nameSpan) = currentToken() match
        case Token.Identifier(actualName, nameSpan) => (actualName, nameSpan)
        case Token.VectorId(nameSpan)               => ("Vector", nameSpan)
        case Token.EmptyTupleId(nameSpan)           => ("EmptyTuple", nameSpan)
        case Token.Plus(nameSpan)                   => ("+", nameSpan)
        case Token.Minus(nameSpan)                  => ("-", nameSpan)
        case Token.StarColon(nameSpan)              => ("*:", nameSpan)
        case other                                  =>
          raise(DecodeError.ExpectedFieldName(describe(other)).atToken(other.span))
      advance()
      currentToken() match
        case Token.Equals(_) =>
          advance()
          (actualName, nameSpan)
        case other =>
          raise(DecodeError.ExpectedEquals(describe(other)).atToken(other.span))

  private inline def parseNamedTupleStructure(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, DecodeError.Span, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    currentToken() match
      case Token.LParen(_) =>
        advance()
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span))

    parseNamedTupleStructureAfterOpen(schema, allowEmpty)(consumeFieldValue)
  }

  private inline def parseNamedTupleStructureAfterOpen(
      schema: RawSchema,
      allowEmpty: Boolean
  )(
      inline consumeFieldValue: Resulting[(String, DecodeError.Span, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = { lbl ?=>
    val parsed: NamedTupleParseResult =
      parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
        case parsed: NamedTupleParseResult => parsed
        case err: Result.Err[DecodeError]  =>
          scala.util.boundary.break(err) // TODO: replace with Result.breakErr

    if !allowEmpty && parsed.fieldCount == 0 then
      raise(DecodeError.UnitValueNotAllowed().atToken(parsed.closingSpan))
    parsed
  }

  private inline def parsePartialNamedTupleStructure(
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, DecodeError.Span, Int) => Unit, DecodeError]
  ): Resulting[NamedTupleParseResult, DecodeError] = {
    parsePartialNamedTupleStructureInner(schema)(consumeFieldValue) match
      case parsed: NamedTupleParseResult => parsed
      case err: Result.Err[DecodeError]  =>
        scala.util.boundary.break(err) // TODO: replace with Result.breakErr
  }

  private inline def parsePartialNamedTupleStructureInner(
      schema: RawSchema
  )(
      inline consumeFieldValue: Resulting[(String, DecodeError.Span, Int) => Unit, DecodeError]
  ): NamedTupleParseResult | Result.Err[DecodeError] =
    // to share the logic without breaking the label optimisation, we need to cache the result and
    // then redispatch the break at the call-site. i.e. nested inline calls dont seem to compose
    // well enough to pass along the label. i would like to investigate why.
    eval {
      scala.util.boundary {
        import Internal.loop

        currentToken() match {
          case rparen @ Token.RParen(_) =>
            advance()
            NamedTupleParseResult.push(0, null, rparen.span)
          case _ =>
            var fieldIndex: Int              = 0
            var lastFieldName: String | Null = null
            val rparen: Token.RParen         = loop {
              val (actualName, nameSpan) = parseNamedFieldStart().ok
              consumeFieldValue(actualName, nameSpan, fieldIndex)
              lastFieldName = actualName
              fieldIndex += 1

              val expectCommaOrRParen: Token | DecodeError = eval {
                currentToken() match
                  case Token.Comma(_) =>
                    advance()
                    currentToken() match
                      case rparen @ Token.RParen(_) => rparen
                      case nextToken                => nextToken
                  case rparen @ Token.RParen(_) => rparen
                  case other                    =>
                    DecodeError.ExpectedRParen(describe(other)).atToken(other.span)
              }
              expectCommaOrRParen match
                case rparen: Token.RParen => loop.break(rparen)
                case err: DecodeError     => raise(err)
                case _                    => ()
            }
            advance()
            NamedTupleParseResult.push(fieldIndex, lastFieldName, rparen.span)
        }
      }
    }

  private inline def parseTupleStructure(schema: RawSchema.Tuple)(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[TupleParseResult, DecodeError] = {
    currentToken() match
      case Token.LParen(_) =>
        advance()
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span))

    parseTupleStructureAfterOpen(schema)(consumeElementValue)
  }

  private inline def parseTupleStructureAfterOpen(schema: RawSchema)(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[TupleParseResult, DecodeError] = {
    import Internal.loop

    currentToken() match
      case rparen @ Token.RParen(_) =>
        advance()
        TupleParseResult.push(0, rparen.span)
      case _ =>
        var elementIndex         = 0
        val rparen: Token.RParen = loop {
          consumeElementValue(elementIndex)
          elementIndex += 1

          currentToken() match
            case Token.Comma(_) =>
              advance()
              currentToken() match
                case rparen @ Token.RParen(_) => loop.break(rparen)
                case _                        => ()
            case rparen @ Token.RParen(_) =>
              loop.break(rparen)
            case other =>
              raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))
        }
        advance()
        TupleParseResult.push(elementIndex, rparen.span)
  }

  private inline def parseVectorStructure(schema: RawSchema)(
      inline consumeElementValue: Resulting[Int => Unit, DecodeError]
  ): Resulting[Unit, DecodeError] = {
    (currentToken(), peekToken()) match
      case (Token.VectorId(_), Token.LParen(_)) =>
        advance()
        advance()
      case (other, _) =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)).atToken(other.span))

    var indexInVector = 0

    if currentToken().isInstanceOf[Token.RParen] then advance()
    else
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
            raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))

      currentToken() match
        case Token.RParen(_) =>
          advance()
        case other =>
          raise(DecodeError.ExpectedRParen(describe(other)).atToken(other.span))
  }

  private[scalanotation] def decodeString[A](wrap: String => A): Result[A, DecodeError] =
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
        raise(expectedType(RawSchema.String, other))

  private[scalanotation] def decodeChar[A](wrap: Char => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.CharLit(value = value) =>
        advance()
        wrap(value)
      case other =>
        raise(expectedType(RawSchema.Char, other))

  private[scalanotation] def decodeInt[A](wrap: Int => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = token =>
        token match
          case Token.IntLit(value = value) => value
          case other                       => raise(expectedType(RawSchema.Int, other)),
      negator = -1,
      one = 1,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeLong[A](wrap: Long => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = token =>
        token match
          case Token.LongLit(value = value) => value
          case Token.IntLit(value = value)  => value.toLong
          case other                        => raise(expectedType(RawSchema.Long, other)),
      negator = -1L,
      one = 1L,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeFloat[A](wrap: Float => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = token =>
        token match
          case Token.FloatLit(value = value)                                        => value
          case Token.IntLit(value = value) if NumericPromotions.isExactFloat(value) =>
            value.toFloat
          case other => raise(expectedType(RawSchema.Float, other)),
      negator = -1.0f,
      one = 1.0f,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeDouble[A](wrap: Double => A): Result[A, DecodeError] = Result:
    decodeSigned(
      literal = token =>
        token match
          case Token.DoubleLit(value = value) => value
          case Token.IntLit(value = value)    => value.toDouble
          case other                          => raise(expectedType(RawSchema.Double, other)),
      negator = -1.0d,
      one = 1.0d,
      prod = _ * _,
      wrap = wrap
    )

  private[scalanotation] def decodeBoolean[A](wrap: Boolean => A): Result[A, DecodeError] =
    Result:
      currentToken() match
        case Token.TrueKw(_) =>
          advance()
          wrap(true)
        case Token.FalseKw(_) =>
          advance()
          wrap(false)
        case other =>
          raise(expectedType(RawSchema.Boolean, other))

  private[scalanotation] def decodeNull[A](wrap: Null => A): Result[A, DecodeError] = Result:
    currentToken() match
      case Token.NullKw(_) =>
        advance()
        wrap(null)
      case other =>
        raise(expectedType(RawSchema.Null, other))

  private inline def decodeSigned[N, A](
      inline literal: Token => N,
      negator: N,
      one: N,
      prod: (N, N) => N,
      wrap: N => A
  ): A =
    val sign =
      currentToken() match
        case Token.Minus(_) =>
          advance()
          negator
        case _ =>
          one
    val value = literal(currentToken())
    advance()
    wrap(prod(sign, value))

  private def expectVal(): Result[Unit, DecodeError] = Result.task:
    currentToken() match
      case Token.ValKw(_) =>
        advance()
      case other =>
        raise(DecodeError.ExpectedVal(describe(other)).atToken(other.span))

  private def expectPackageStatement(packageName: String): Result[Unit, DecodeError] =
    Result.task:
      if packageName.nonEmpty then
        expectPackage().check
        val declaredName = expectQualifiedIdentifier().ok
        if declaredName != packageName then raise(DecodeError.UnexpectedPackage(declaredName))
        acceptStatementSeparator()

  private def acceptStatementSeparator(): Unit =
    currentToken() match
      case Token.Semicolon(_) => advance()
      case _                  => ()

  private def expectPackage(): Result[Unit, DecodeError] = Result.task:
    currentToken() match
      case Token.PackageKw(_) =>
        advance()
      case other =>
        raise(DecodeError.ExpectedPackage(describe(other)).atToken(other.span))

  private def expectQualifiedIdentifier(): Result[String, DecodeError] = Result:
    val builder = new StringBuilder(expectIdentifier().ok)
    while currentToken().isInstanceOf[Token.Dot] do
      advance()
      builder += '.'
      builder ++= expectIdentifier().ok
    builder.result()

  private def expectIdentifier(): Result[String, DecodeError] = Result:
    currentToken() match
      case Token.Identifier(name, _) =>
        advance()
        name
      case Token.VectorId(_) =>
        advance()
        "Vector"
      case Token.EmptyTupleId(_) =>
        advance()
        "EmptyTuple"
      case Token.Plus(_) =>
        advance()
        "+"
      case Token.Minus(_) =>
        advance()
        "-"
      case Token.StarColon(_) =>
        advance()
        "*:"
      case other =>
        raise(DecodeError.ExpectedIdentifier(describe(other)).atToken(other.span))

  private def expectEquals(): Result[Unit, DecodeError] = Result.task:
    currentToken() match
      case Token.Equals(_) =>
        advance()
      case other =>
        raise(DecodeError.ExpectedEquals(describe(other)).atToken(other.span))

  private def expectEof(): Result[Unit, DecodeError] = Result.task:
    currentToken() match
      case Token.Eof(_) => ()
      case other        => raise(DecodeError.ExpectedEof(describe(other)).atToken(other.span))
}
