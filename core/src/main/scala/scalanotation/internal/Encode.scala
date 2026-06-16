package scalanotation.internal

import scalanotation.Expr
import scalanotation.TextFormat

import scala.{Option => ScalaOption}

private[scalanotation] object Encode:
  private def missingWriteCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"write is not available for schema ${schema.describeSelf}"
    )

  private def selectedRouterCase(schema: RawSchema.Router, value: Any): RawSchema.RouterCase =
    if schema.write == null then missingWriteCapability(schema)
    val index = schema.write.nn.caseIndex(value)
    if index < 0 || index >= schema.cases.length then
      throw IllegalArgumentException(
        s"router ${schema.describeSelf} cannot select a case for value $value"
      )
    schema.cases(index)

  def writeExpr(schema: RawSchema, value: Any): Expr =
    schema match
      case mapped: RawSchema.Mapped =>
        writeExpr(mapped.base, mapped.mapping.mapInput(value))
      case RawSchema.Ref(_, target) =>
        writeExpr(target(), value)
      case router: RawSchema.Router =>
        writeExpr(selectedRouterCase(router, value).schema, value)
      case namedTuple: RawSchema.NamedTuple =>
        if namedTuple.write == null then missingWriteCapability(schema)
        val access     = namedTuple.write.nn
        val fields     = namedTuple.fields
        val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
        var index      = 0
        while index < fields.length do
          val field = fields(index)
          fieldExprs += ((field.name, writeExpr(field.schema, access.fieldValue(value, index))))
          index += 1
        Expr.NamedTupleExpr(fieldExprs.result())
      case tuple: RawSchema.Tuple =>
        if tuple.write == null then missingWriteCapability(schema)
        val access = tuple.write.nn
        val slots  = tuple.slots
        Expr.TupleExpr(
          slots.indices.map(index => writeExpr(slots(index), access.elementValue(value, index)))
        )
      case RawSchema.PartialNamedTuple(base, _) =>
        writeExpr(base, value)
      case sum: RawSchema.Sum =>
        if sum.write == null then missingWriteCapability(schema)
        val sumCase = sum.cases(sum.write.nn.caseIndex(value))
        Expr.NamedTupleExpr(IndexedSeq(sumCase.name -> writeExpr(sumCase.schema, value)))
      case sum: RawSchema.DiscriminatorSum =>
        if sum.write == null then missingWriteCapability(schema)
        val sumCase = sum.cases(sum.write.nn.caseIndex(value))
        Expr.NamedTupleExpr(
          (sum.discriminatorField -> Expr.StringConstant(sumCase.name)) +:
            writeDiscriminatorPayload(sumCase.schema, value)
        )
      case vector: RawSchema.Vector =>
        if vector.write == null then missingWriteCapability(schema)
        Expr.VectorExpr(
          vector.write.nn.iterator(value).map(writeExpr(vector.element, _)).toIndexedSeq
        )
      case tupleOf: RawSchema.TupleOf =>
        if tupleOf.write == null then missingWriteCapability(schema)
        Expr.TupleExpr(
          tupleOf.write.nn.iterator(value).map(writeExpr(tupleOf.element, _)).toIndexedSeq
        )
      case dict: RawSchema.Dict =>
        if dict.write == null then missingWriteCapability(schema)
        Expr.NamedTupleExpr(
          dict.write.nn
            .iterator(value)
            .map((key, elem) => key -> writeExpr(dict.element, elem))
            .toIndexedSeq
        )
      case option: RawSchema.Option =>
        value.asInstanceOf[ScalaOption[Any]] match
          case Some(innerValue) => writeExpr(option.inner, innerValue)
          case None             => Expr.NullConstant
      case RawSchema.String =>
        Expr.StringConstant(value.asInstanceOf[String])
      case RawSchema.Char =>
        Expr.CharConstant(value.asInstanceOf[Char])
      case RawSchema.Int =>
        Expr.IntConstant(value.asInstanceOf[Int])
      case RawSchema.Long =>
        Expr.LongConstant(value.asInstanceOf[Long])
      case RawSchema.Float =>
        Expr.FloatConstant(value.asInstanceOf[Float])
      case RawSchema.Double =>
        Expr.DoubleConstant(value.asInstanceOf[Double])
      case RawSchema.Boolean =>
        Expr.BooleanConstant(value.asInstanceOf[Boolean])
      case RawSchema.Null =>
        Expr.NullConstant

  def renderText(
      schema: RawSchema,
      value: Any,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit = schema match
    case mapped: RawSchema.Mapped =>
      renderText(mapped.base, mapped.mapping.mapInput(value), out, depth)
    case RawSchema.Ref(_, target) =>
      renderText(target(), value, out, depth)
    case router: RawSchema.Router =>
      renderText(selectedRouterCase(router, value).schema, value, out, depth)
    case namedTuple: RawSchema.NamedTuple =>
      val write = namedTuple.write
      if write == null then missingWriteCapability(schema)
      val fields = namedTuple.fields
      ExprRenderer.renderNamedTuple(out, depth, fields.length) { index =>
        val field = fields(index)
        IdentifierSyntax.appendIdentifier(field.name, out)
        out.append(" = ")
        renderText(field.schema, write.fieldValue(value, index), out, depth + 1)
      }
    case tuple: RawSchema.Tuple =>
      val write = tuple.write
      if write == null then missingWriteCapability(schema)
      val slots = tuple.slots
      ExprRenderer.renderTuple(out, depth, write.size(value)) { index =>
        renderTupleElement(slots(index), write.elementValue(value, index), out, depth + 1)
      }
    case RawSchema.PartialNamedTuple(base, _) =>
      renderText(base, value, out, depth)
    case sum: RawSchema.Sum =>
      val write = sum.write
      if write == null then missingWriteCapability(schema)
      val sumCase = sum.cases(write.caseIndex(value))
      ExprRenderer.renderNamedTuple(out, depth, 1) { _ =>
        IdentifierSyntax.appendIdentifier(sumCase.name, out)
        out.append(" = ")
        renderText(sumCase.schema, value, out, depth + 1)
      }
    case sum: RawSchema.DiscriminatorSum =>
      if sum.write == null then missingWriteCapability(schema)
      ExprRenderer.renderExpr(writeExpr(sum, value), out, depth)
    case vector: RawSchema.Vector =>
      val write = vector.write
      if write == null then missingWriteCapability(schema)
      val values = write.iterator(value)
      ExprRenderer.renderVector(out, depth, write.size(value)) { _ =>
        renderText(vector.element, values.next(), out, depth + 1)
      }
    case tupleOf: RawSchema.TupleOf =>
      val write = tupleOf.write
      if write == null then missingWriteCapability(schema)
      val values = write.iterator(value)
      ExprRenderer.renderTuple(out, depth, write.size(value)) { _ =>
        renderTupleElement(tupleOf.element, values.next(), out, depth + 1)
      }
    case dict: RawSchema.Dict =>
      val write = dict.write
      if write == null then missingWriteCapability(schema)
      val values = write.iterator(value)
      ExprRenderer.renderNamedTuple(out, depth, write.size(value)) { _ =>
        val (key, elem) = values.next()
        IdentifierSyntax.appendIdentifier(key, out)
        out.append(" = ")
        renderText(dict.element, elem, out, depth + 1)
      }
    case option: RawSchema.Option =>
      value.asInstanceOf[ScalaOption[Any]] match
        case Some(innerValue) => renderText(option.inner, innerValue, out, depth)
        case None             => out.append("null")
    case RawSchema.String =>
      ExprRenderer.renderStringLiteral(value.asInstanceOf[String], out)
    case RawSchema.Char =>
      ExprRenderer.renderCharLiteral(value.asInstanceOf[Char], out)
    case RawSchema.Int =>
      out.append(value.asInstanceOf[Int].toString)
    case RawSchema.Long =>
      out.append(s"${value.asInstanceOf[Long]}L")
    case RawSchema.Float =>
      ExprRenderer.renderFloatLiteral(value.asInstanceOf[Float], out)
    case RawSchema.Double =>
      ExprRenderer.renderDoubleLiteral(value.asInstanceOf[Double], out)
    case RawSchema.Boolean =>
      out.append(value.asInstanceOf[Boolean].toString)
    case RawSchema.Null =>
      out.append("null")

  private def writeDiscriminatorPayload(schema: RawSchema, value: Any): IndexedSeq[
    (name: String, value: Expr)
  ] =
    schema match
      case RawSchema.PartialNamedTuple(base, _) =>
        writeDiscriminatorPayload(base, value)
      case mapped: RawSchema.Mapped =>
        writeDiscriminatorPayload(mapped.base, mapped.mapping.mapInput(value))
      case RawSchema.Ref(_, target) =>
        writeDiscriminatorPayload(target(), value)
      case namedTuple: RawSchema.NamedTuple =>
        if namedTuple.write == null then missingWriteCapability(namedTuple)
        val access     = namedTuple.write.nn
        val fields     = namedTuple.fields
        val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
        var index      = 0
        while index < fields.length do
          val field = fields(index)
          fieldExprs += ((field.name, writeExpr(field.schema, access.fieldValue(value, index))))
          index += 1
        fieldExprs.result().toIndexedSeq
      case RawSchema.Null =>
        IndexedSeq.empty
      case other =>
        throw IllegalStateException(
          s"discriminator sum case must be a named tuple, but found ${other.describeSelf}"
        )

  private def renderTupleElement(
      schema: RawSchema,
      value: Any,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    renderText(schema, value, out, depth)
