package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.internal.RawSchema.Field
import steps.result.Result
import steps.result.Result.eval.check
import steps.result.Result.eval.raise

private[scalanotation] object ExprDecoder:
  def decodeExpr[A: Reader as reader](expr: Expr): Result[A, DecodeError] =
    ExprDecoder().decodeInto(reader, expr)

private[scalanotation] class ExprDecoder() extends PushSlots with SharedHelpers:
  private[internal] def slotsPooling: Boolean = false // TODO: batched Expr decode?

  private def describeExpr(expr: Expr): String = expr match
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

  private def expectedType(schema: RawSchema, expr: Expr): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeExpr(expr))

  def decodeInto[A](reader: Reader[A], expr: Expr): Result[A, DecodeError] =
    Result:
      decodeBase(reader.schema, expr).check
      pullAny().asInstanceOf[A]

  private def decodeBase(
      schema: RawSchema,
      expr: Expr
  ): Result[Unit, DecodeError] =
    schema match
      case mapped: RawSchema.Mapped =>
        Result.task {
          decodeBase(mapped.base, expr).check
          mapSlot(mapped.mapping).check
        }
      case RawSchema.Ref(_, target) =>
        decodeBase(target(), expr)
      case router: RawSchema.Router =>
        if router eq RawSchema.ExprRouterSchema then
          Result.task {
            pushRef(expr)
          }
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
      case sc: RawSchema.PairSeq =>
        decodePairSeq(sc, expr)
      case sc: RawSchema.Dict =>
        decodeDict(sc, expr)
      case sc: RawSchema.Option =>
        expr match
          case Expr.NullConstant =>
            Result.task {
              pushRef(None)
            }
          case other =>
            Result.task {
              decodeBase(sc.inner, other).check
              pushRef(Some(pullAny()))
            }
      case RawSchema.String =>
        Result.task {
          expr match
            case Expr.StringConstant(value) =>
              pushString(value)
            case other => raise(expectedType(RawSchema.String, other))
        }
      case RawSchema.Char =>
        Result.task {
          expr match
            case Expr.CharConstant(value) =>
              pushChar(value)
            case other => raise(expectedType(RawSchema.Char, other))
        }
      case RawSchema.Int =>
        Result.task {
          expr match
            case Expr.IntConstant(value) =>
              pushInt(value)
            case other => raise(expectedType(RawSchema.Int, other))
        }
      case RawSchema.Long =>
        Result.task {
          expr match
            case Expr.LongConstant(value) =>
              pushLong(value)
            case Expr.IntConstant(value) =>
              pushLong(value.toLong)
            case other => raise(expectedType(RawSchema.Long, other))
        }
      case RawSchema.Float =>
        Result.task {
          expr match
            case Expr.FloatConstant(value) =>
              pushFloat(value)
            case Expr.IntConstant(value) if NumericPromotions.isExactFloat(value) =>
              pushFloat(value.toFloat)
            case other => raise(expectedType(RawSchema.Float, other))
        }
      case RawSchema.Double =>
        Result.task {
          expr match
            case Expr.DoubleConstant(value) =>
              pushDouble(value)
            case Expr.IntConstant(value) =>
              pushDouble(value.toDouble)
            case other => raise(expectedType(RawSchema.Double, other))
        }
      case RawSchema.Boolean =>
        Result.task {
          expr match
            case Expr.BooleanConstant(value) =>
              pushBoolean(value)
            case other => raise(expectedType(RawSchema.Boolean, other))
        }
      case RawSchema.Null =>
        Result.task {
          expr match
            case Expr.NullConstant =>
              pushRef(null)
            case other => raise(expectedType(RawSchema.Null, other))
        }

  private def decodeRouter(
      schema: RawSchema.Router,
      expr: Expr
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read): read =>
      Result.task:
        val index = read.route(routerConstruct(expr))
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
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read): read =>
      Result.task:
        expr match
          case Expr.VectorExpr(elements) =>
            var values = read.init()
            var index  = 0
            while index < elements.length do
              checkOrRaise(decodeBase(schema.element, elements(index)))(_.atPath(s"[$index]"))
              values = addSlot(read)(values)
              index += 1
            pushRef(read.finish(values))
          case other =>
            raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(other)))

  private def decodeTupleOf(
      schema: RawSchema.TupleOf,
      expr: Expr
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read): read =>
      Result.task:
        expr match
          case Expr.TupleExpr(elements) =>
            var values = read.init()
            var index  = 0
            while index < elements.length do
              checkOrRaise(decodeBase(schema.element, elements(index)))(_.atPath(s"[$index]"))
              values = addSlot(read)(values)
              index += 1
            pushRef(read.finish(values))
          case other =>
            raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(other)))

  private def decodePairSeq(
      schema: RawSchema.PairSeq,
      expr: Expr
  ): Result[Unit, DecodeError] = Result.task:
    expr match
      case Expr.VectorExpr(elements) =>
        if schema.read == null then missingReadCapability(schema)
        val read  = schema.read.nn
        var state = read.init()
        var index = 0
        while index < elements.length do
          elements(index) match
            case Expr.TupleExpr(pair) =>
              if pair.length != 2 then
                raise(DecodeError.FieldCountMismatch(2, pair.length).atPath(s"[$index]"))
              checkOrRaise(decodeBase(schema.key, pair(0)))(_.atPath(s"[$index][0]"))
              val key = pullAny()
              checkOrRaise(decodeBase(schema.value, pair(1)))(_.atPath(s"[$index][1]"))
              state = addSlot(read)(state, key)
            case other =>
              raise(
                DecodeError
                  .ExpectedType(RawSchema.describeTupleSlots(2), describe(other))
                  .atPath(s"[$index]")
              )
          index += 1
        pushRef(read.finish(state))
      case other =>
        raise(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

  private def decodeTuple(
      schema: RawSchema.Tuple,
      expr: Expr
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read): read =>
      Result.task:
        expr match
          case Expr.TupleExpr(elements) =>
            val slots = schema.slots
            if elements.length != slots.length then
              raise(DecodeError.FieldCountMismatch(slots.length, elements.length))
            var state = read.initPooled(slots.length, pooled = null)
            var index = 0
            while index < slots.length do
              checkOrRaise(decodeBase(slots(index), elements(index)))(_.atPath(s"[$index]"))
              state = addSlot(read)(state, index)
              index += 1
            pushRef(read.finish(state))
          case other =>
            raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(other)))

  private def decodeNamedTuple(
      schema: RawSchema.NamedTuple,
      expr: Expr
  ): Result[Unit, DecodeError] = namesPool.withBorrowed { seenNames =>
    withRead(schema, _.read): read =>
      Result.task:
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

                checkOrRaise(decodeBase(field.schema, fieldExpr.value))(_.atPath(s".${field.name}"))
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

                checkOrRaise(decodeBase(field.schema, fieldExpr.value))(_.atPath(s".${field.name}"))
                state = addSlot(read)(state, index)
                index += 1

            pushRef(read.finish(state))
          case other =>
            raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(other)))
  }

  private def decodeDict(
      schema: RawSchema.Dict,
      expr: Expr
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read): read =>
      Result.task:
        expr match
          case Expr.NamedTupleExpr(fieldExprs) =>
            var index = 0
            var state = read.init()
            while index < fieldExprs.length do
              val fieldExpr = fieldExprs(index)
              val fieldName = fieldExpr.name
              checkOrRaise(decodeBase(schema.element, fieldExpr.value))(_.atPath(s".${fieldName}"))
              state = addSlot(read)(state, fieldName)
              index += 1

            pushRef(read.finish(state))
          case other =>
            raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(other)))

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
          raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(other)))

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
        Result.task {
          decodePartialNamedTuple(mapped.base, alreadySeenField, expr).check
          mapSlot(mapped.mapping).check
        }
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
  ): Result[Unit, DecodeError] =
    withRead(schema, _.read): read =>
      Result.task: abortTask ?=>
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
              val parsedFieldIndex                          = index - offset
              val fieldExpr                                 = fieldExprs(index)
              val actualName                                = fieldExpr.name
              def fieldError(err: DecodeError): DecodeError = err.atPath(s".${actualName}")
              val state0                                    = state
              val fieldIndex0                               = fieldIndex
              val expectedField: Field                      = {
                if schema.allowSkippedNullableFields then
                  val expectedBeforeSkip =
                    if fieldIndex0 < fields.length then fields(fieldIndex0)
                    else null
                  val state1 = fillSkippedNullableFields(read)(
                    fields,
                    state0,
                    fieldIndex0,
                    actualName
                  )
                  val fieldIndex1   = pullSkipFillIndex()
                  val expectedField = {
                    if fieldIndex1 >= fields.length then
                      if expectedBeforeSkip == null then
                        raise(
                          fieldError(
                            DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1)
                          )
                        )
                      else
                        raise(
                          fieldError(
                            DecodeError.FieldOrderMismatch(expectedBeforeSkip.name, actualName)
                          )
                        )
                    else
                      val expectedField = fields(fieldIndex1)
                      if actualName != expectedField.name then
                        raise(
                          fieldError(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                        )
                      else expectedField
                  }
                  state = state1
                  fieldIndex = fieldIndex1
                  expectedField
                else if parsedFieldIndex >= fields.length then
                  raise(DecodeError.FieldCountMismatch(fields.length, parsedFieldIndex + 1))
                else
                  val expectedField = fields(parsedFieldIndex)
                  if actualName != expectedField.name then
                    raise(DecodeError.FieldOrderMismatch(expectedField.name, actualName))
                  else expectedField
              }
              checkOrRaise(decodeBase(expectedField.schema, fieldExpr.value))(
                _.atPath(s".${expectedField.name}")
              )
              state = addSlot(read)(state, fieldIndex)
              fieldIndex += 1
              index += 1
            end while

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
            raise(DecodeError.ExpectedType(schema.describeSelf, describeExpr(other)))
