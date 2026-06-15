package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.internal.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

import TokenDecoder.describe

private[scalanotation] object ExprDecoder:
  def decodeExpr[A: Reader as reader](expr: Expr): Result[A, DecodeError] =
    ExprDecoder().decodeInto(reader, expr)

private[scalanotation] class ExprDecoder extends PushSlots:
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

  def decodeInto[A](reader: Reader[A], expr: Expr): Result[A, DecodeError] =
    Result:
      decodeBase(reader.schema, expr).check
      pullAny().asInstanceOf[A]

  private[scalanotation] def decodeRaw(
      schema: RawSchema,
      expr: Expr
  ): Result[Any, DecodeError] =
    Result:
      decodeBase(schema, expr).check
      pullAny()

  private def decodeBase(
      schema: RawSchema,
      expr: Expr
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        mapSlot(mapped.mapping, decodeBase(mapped.base, expr))
      case RawSchema.Ref(_, target) =>
        decodeBase(target(), expr)
      case router: RawSchema.Router =>
        if router eq RawSchema.ExprRouterSchema then
          pushRef(expr)
          Result.done
        else decodeRouter(router, expr)
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
      case sc: RawSchema.TupleOf =>
        decodeTupleOf(sc, expr)
      case sc: RawSchema.Dict =>
        decodeDict(sc, expr)
      case sc: RawSchema.Option =>
        expr match
          case Expr.NullConstant =>
            pushRef(None)
            Result.done
          case other =>
            val r = decodeBase(sc.inner, other)
            if r.isOk then pushRef(Some(pullAny()))
            r
      case RawSchema.AnyExpr =>
        pushRef(expr)
        Result.done
      case RawSchema.String =>
        expr match
          case Expr.StringConstant(value) =>
            pushString(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.String, other))
      case RawSchema.Char =>
        expr match
          case Expr.CharConstant(value) =>
            pushChar(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Char, other))
      case RawSchema.Int =>
        expr match
          case Expr.IntConstant(value) =>
            pushInt(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Int, other))
      case RawSchema.Long =>
        expr match
          case Expr.LongConstant(value) =>
            pushLong(value)
            Result.done
          case Expr.IntConstant(value) =>
            pushLong(value.toLong)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Long, other))
      case RawSchema.Float =>
        expr match
          case Expr.FloatConstant(value) =>
            pushFloat(value)
            Result.done
          case Expr.IntConstant(value) if isExactFloat(value) =>
            pushFloat(value.toFloat)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Float, other))
      case RawSchema.Double =>
        expr match
          case Expr.DoubleConstant(value) =>
            pushDouble(value)
            Result.done
          case Expr.IntConstant(value) =>
            pushDouble(value.toDouble)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Double, other))
      case RawSchema.Boolean =>
        expr match
          case Expr.BooleanConstant(value) =>
            pushBoolean(value)
            Result.done
          case other => Result.Err(expectedType(RawSchema.Boolean, other))
      case RawSchema.Null =>
        expr match
          case Expr.NullConstant =>
            pushRef(null)
            Result.done
          case other => Result.Err(expectedType(schema, other))

  private def decodeRouter(
      schema: RawSchema.Router,
      expr: Expr
  ): Result[Unit, DecodeError] =
    Result.task:
      if schema.read == null then missingReadCapability(schema)
      val index = schema.read.nn.route(routerConstruct(expr))
      if index < 0 || index >= schema.cases.length then
        raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(expr)))
      decodeBase(schema.cases(index).schema, expr).check

  private def routerConstruct(expr: Expr): RawSchema.RouterConstruct =
    expr match
      case Expr.NamedTupleExpr(_)  => RawSchema.RouterConstruct.Record
      case Expr.TupleExpr(_)       => RawSchema.RouterConstruct.Tuple
      case Expr.VectorExpr(_)      => RawSchema.RouterConstruct.Vector
      case Expr.StringConstant(_)  => RawSchema.RouterConstruct.String
      case Expr.CharConstant(_)    => RawSchema.RouterConstruct.Char
      case Expr.IntConstant(_)     => RawSchema.RouterConstruct.Int
      case Expr.LongConstant(_)    => RawSchema.RouterConstruct.Long
      case Expr.FloatConstant(_)   => RawSchema.RouterConstruct.Float
      case Expr.DoubleConstant(_)  => RawSchema.RouterConstruct.Double
      case Expr.BooleanConstant(_) => RawSchema.RouterConstruct.Boolean
      case Expr.NullConstant       => RawSchema.RouterConstruct.Null

  private def decodeVector(
      schema: RawSchema.Vector,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.VectorExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read   = schema.read.nn
        var values = read.init()
        var index  = 0
        while index < elements.length do
          checkOrRaise(decodeBase(schema.element, elements(index)))(_.atPath(s"[$index]"))
          values = addSlot(read)(values)
          index += 1
        pushRef(read.finish(values))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeTupleOf(
      schema: RawSchema.TupleOf,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.TupleExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read   = schema.read.nn
        var values = read.init()
        var index  = 0
        while index < elements.length do
          checkOrRaise(decodeBase(schema.element, elements(index)))(_.atPath(s"[$index]"))
          values = addSlot(read)(values)
          index += 1
        pushRef(read.finish(values))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeTuple(
      schema: RawSchema.Tuple,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.TupleExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read  = schema.read.nn
        val slots = schema.slots
        if elements.length != slots.length then
          raise(DecodeError.FieldCountMismatch(slots.length, elements.length))
        var state = read.init(slots.length)
        var index = 0
        while index < slots.length do
          checkOrRaise(decodeBase(slots(index), elements(index)))(_.atPath(s"[$index]"))
          state = addSlot(read)(state, index)
          index += 1
        pushRef(read.finish(state))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple,
      expr: Expr
  ): Result[Unit, DecodeError] = namesPool.withBorrowed { seenNames =>
    Result.task:
      if schema.read == null then missingReadCapability(schema)
      val read = schema.read.nn
      schema.isValidNamedTuple(namesPool).check
      expr match
        case Expr.NamedTupleExpr(fieldExprs) =>
          val fields = schema.fields
          var state  = read.init(fields.length, slots = null)
          if schema.allowSkippedNullableFields then
            if fieldExprs.isEmpty && fields.nonEmpty then raise(DecodeError.UnitValueNotAllowed())
            var fieldExprIndex = 0
            var fieldIndex     = 0
            while fieldExprIndex < fieldExprs.length do
              val fieldExpr          = fieldExprs(fieldExprIndex)
              val fieldName          = fieldExpr.name
              val expectedBeforeSkip =
                if fieldIndex < fields.length then fields(fieldIndex) else null
              state = fillSkippedNullableFields(read)(fields, state, fieldIndex, fieldName)
              fieldIndex = pullSkipFillIndex()

              if fieldIndex >= fields.length then
                if expectedBeforeSkip == null then
                  raise(DecodeError.FieldCountMismatch(fields.length, fieldExprIndex + 1))
                else raise(DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, fieldName))

              val field = fields(fieldIndex)
              if fieldName != field.name then
                raise(DecodeError.FieldOrderMismatch(field.name, fieldName))

              checkOrRaise(decodeBase(field.schema, fieldExpr.value))(
                _.atPath(s".${field.name}")
              )
              state = addSlot(read)(state, fieldIndex)
              fieldIndex += 1
              fieldExprIndex += 1

            state = fillSkippedNullableFields(read)(fields, state, fieldIndex, "")
            fieldIndex = pullSkipFillIndex()
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

              checkOrRaise(decodeBase(field.schema, fieldExpr.value))(
                _.atPath(s".${field.name}")
              )
              state = addSlot(read)(state, index)
              index += 1

          pushRef(read.finish(state))
        case other =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
  }

  private def decodeDict(
      schema: RawSchema.Dict,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        if schema.read == null then missingReadCapability(schema)
        val read  = schema.read.nn
        var index = 0
        var state = read.init()
        while index < fieldExprs.length do
          val fieldExpr = fieldExprs(index)
          val fieldName = fieldExpr.name
          checkOrRaise(decodeBase(schema.element, fieldExpr.value))(
            _.atPath(s".${fieldName}")
          )
          state = addSlot(read)(state, fieldName)
          index += 1

        pushRef(read.finish(state))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeSum(
      schema: RawSchema.Sum,
      expr: Expr
  ): Result[Unit, DecodeError] =
    Result.task:
      expr match
        case Expr.NamedTupleExpr(fieldExprs) =>
          if fieldExprs.length != 1 then raise(DecodeError.FieldCountMismatch(1, fieldExprs.length))
          val fieldExpr = fieldExprs(0)
          val caseName  = fieldExpr.name
          val value     = fieldExpr.value
          val sumCase   = RawSchema.findCase(schema.cases, caseName) match
            case null => raise(DecodeError.UnexpectedField(caseName).atPath(s".$caseName"))
            case c    => c
          checkOrRaise(decodeBase(sumCase.schema, value))(_.atPath(s".$caseName"))
        case other =>
          raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeDiscriminatorSum(
      schema: RawSchema.DiscriminatorSum,
      expr: Expr
  ): Result[Unit, DecodeError] =
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        Result.task:
          val discriminatorField = schema.discriminatorField
          if fieldExprs.isEmpty then raise(DecodeError.FieldCountMismatch(1, 0))

          val discriminatorExpr = fieldExprs(0)
          if discriminatorExpr.name != discriminatorField then
            raise(DecodeError.FieldOrderMismatch(discriminatorField, discriminatorExpr.name))

          checkOrRaise(decodeBase(RawSchema.String, discriminatorExpr.value))(
            _.atPath(s".$discriminatorField")
          )
          val caseName = pullStringStrict()
          val sumCase  = RawSchema.findCase(schema.cases, caseName) match
            case null =>
              raise(DecodeError.UnexpectedField(caseName).atPath(s".$discriminatorField"))
            case c => c
          decodeBase(sumCase.schema, expr).check
      case other =>
        Result.Err(expectedType(schema, other))

  private def decodePartialNamedTuple(
      schema: RawSchema,
      alreadySeenField: String,
      expr: Expr
  ): Result[Unit, DecodeError] =
    schema match
      case RawSchema.PartialNamedTuple(base, _) =>
        decodePartialNamedTuple(base, alreadySeenField, expr)
      case mapped: RawSchema.Mapped =>
        mapSlot(mapped.mapping, decodePartialNamedTuple(mapped.base, alreadySeenField, expr))
      case namedTuple: RawSchema.NamedTuple =>
        decodePartialNamedTuple(namedTuple, alreadySeenField, expr)
      case RawSchema.Null =>
        expr match
          case Expr.NamedTupleExpr(fieldExprs) =>
            Result.task:
              validatePartialNamedTupleStart(fieldExprs, alreadySeenField).check
              val payloadFieldCount = fieldExprs.length - 1
              if payloadFieldCount == 0 then pushRef(null)
              else raise(DecodeError.FieldCountMismatch(0, payloadFieldCount))
          case other =>
            Result.Err(DecodeError.ExpectedType(RawSchema.Null.describeSelf, describeExpr(other)))
      case other =>
        Result.Err(DecodeError.ExpectedType(other.describeSelf, describeExpr(expr)))

  private def validatePartialNamedTupleStart(
      fieldExprs: IndexedSeq[(name: String, value: Expr)],
      alreadySeenField: String
  ): Result[Unit, DecodeError] =
    Result.task:
      if fieldExprs.isEmpty then raise(DecodeError.FieldCountMismatch(1, 0))
      val actualName = fieldExprs(0).name
      if actualName != alreadySeenField then
        raise(DecodeError.FieldOrderMismatch(alreadySeenField, actualName))

  private def decodePartialNamedTuple(
      schema: RawSchema.NamedTuple,
      alreadySeenField: String,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    if schema.read == null then missingReadCapability(schema)
    val read = schema.read.nn
    schema.isValidNamedTuple(namesPool).check
    expr match
      case Expr.NamedTupleExpr(fieldExprs) =>
        validatePartialNamedTupleStart(fieldExprs, alreadySeenField).check
        val fields     = schema.fields
        var state      = read.init(fields.length, slots = null)
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
              state = fillSkippedNullableFields(read)(fields, state, fieldIndex, actualName)
              fieldIndex = pullSkipFillIndex()

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
              checkOrRaise(decodeBase(expectedField.schema, fieldExpr.value))(
                _.atPath(s".${expectedField.name}")
              )
              state = addSlot(read)(state, fieldIndex)
              fieldIndex += 1
            case err: DecodeError => raise(err.atPath(s".$actualName"))
          index += 1

        if schema.allowSkippedNullableFields then
          state = fillSkippedNullableFields(read)(fields, state, fieldIndex, "")
          fieldIndex = pullSkipFillIndex()

        val payloadFieldCount = fieldExprs.length - offset
        val decodedFieldCount =
          if schema.allowSkippedNullableFields then fieldIndex else payloadFieldCount
        if decodedFieldCount != fields.length then
          raise(DecodeError.FieldCountMismatch(fields.length, payloadFieldCount))

        pushRef(read.finish(state))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))
