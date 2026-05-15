package scalanotation.internal

import scalanotation.Expr
import scalanotation.TextFormat

import scala.{Option => ScalaOption}

private[scalanotation] object Encode:
  private def missingWriteCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"write is not available for schema ${schema.describeSelf}"
    )

  def writeExpr(schema: RawSchema, value: Any): Expr =
    schema match
      case mapped: RawSchema.Mapped =>
        writeExpr(mapped.base, mapped.mapping.mapInput(value))
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
      case RawSchema.AnyExpr =>
        value.asInstanceOf[Expr]
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
    case RawSchema.AnyExpr =>
      ExprRenderer.renderExpr(value.asInstanceOf[Expr], out, depth)(using format)
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
