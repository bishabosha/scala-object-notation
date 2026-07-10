package scalanotation.internal

import scalanotation.Expr
import scalanotation.RouterSchema
import scalanotation.TextFormat

import scala.{Option => ScalaOption}
import scalanotation.schema
import scalanotation.schema.RawSchema

private[scalanotation] object Encode:
  private def missingWriteCapability(schema: RawSchema[?]): Nothing =
    throw IllegalStateException(
      s"write is not available for schema ${schema.describeSelf}"
    )

  private def selectedRouterCase(
      schema: RawSchema.Router[?],
      value: Any
  ): RawSchema.RouterCase[?] =
    if schema.write == null then missingWriteCapability(schema)
    val selected = RawSchema.routerCase(
      schema,
      schema.write.asInstanceOf[RouterSchema.Write[Any]].caseIndex(schema.router, value)
    )
    if selected == null then
      throw IllegalArgumentException(
        s"router ${schema.describeSelf} cannot select a case for value $value"
      )
    selected

  private def mappedInput(mapping: schema.SchemaMapping[?, ?], value: Any): Any =
    mapping.asInstanceOf[schema.SchemaMapping[Any, Any]].mapInput(value)

  def writeExpr(schema: RawSchema[?], value: Any): Expr =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        writeExpr(mapped.base, mappedInput(mapped.mapping, value))
      case RawSchema.Ref(_, target) =>
        writeExpr(target(), value)
      case router: RawSchema.Router[?] =>
        writeExpr(selectedRouterCase(router, value).schema, value)
      case namedTuple: RawSchema.NamedTuple[?] =>
        if namedTuple.write == null then missingWriteCapability(schema)
        val access     = namedTuple.write
        val fields     = namedTuple.fields
        val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
        var index      = 0
        while index < fields.length do
          val field = fields(index)
          fieldExprs += ((field.name, writeExpr(field.schema, access.fieldValue(value, index))))
          index += 1
        Expr.NamedTupleExpr(fieldExprs.result())
      case tuple: RawSchema.Tuple[?] =>
        if tuple.write == null then missingWriteCapability(schema)
        val access = tuple.write
        val slots  = tuple.slots
        Expr.TupleExpr(
          slots.indices.map(index => writeExpr(slots(index), access.elementValue(value, index)))
        )
      case RawSchema.PartialNamedTuple(base, _) =>
        writeExpr(base, value)
      case sum: RawSchema.Sum[?] =>
        if sum.write == null then missingWriteCapability(schema)
        val sumCase = sum.cases(sum.write.caseIndex(value))
        Expr.NamedTupleExpr(IndexedSeq(sumCase.name -> writeExpr(sumCase.schema, value)))
      case sum: RawSchema.DiscriminatorSum[?] =>
        if sum.write == null then missingWriteCapability(schema)
        val sumCase = sum.cases(sum.write.caseIndex(value))
        Expr.NamedTupleExpr(
          (sum.discriminatorField -> Expr.StringConstant(sumCase.name)) +:
            writeDiscriminatorPayload(sumCase.schema, value)
        )
      case vector: RawSchema.Vector[?, ?] =>
        if vector.write == null then missingWriteCapability(schema)
        Expr.VectorExpr(
          vector.write.iterator(value).map(writeExpr(vector.element, _)).toIndexedSeq
        )
      case tupleOf: RawSchema.TupleOf[?, ?] =>
        if tupleOf.write == null then missingWriteCapability(schema)
        Expr.TupleExpr(
          tupleOf.write.iterator(value).map(writeExpr(tupleOf.element, _)).toIndexedSeq
        )
      case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
        if pairSeq.write == null then missingWriteCapability(schema)
        Expr.VectorExpr(
          pairSeq.write
            .iterator(value)
            .map((key, elem) =>
              Expr.TupleExpr(
                IndexedSeq(
                  writeExpr(pairSeq.key, key),
                  writeExpr(pairSeq.value, elem)
                )
              )
            )
            .toIndexedSeq
        )
      case dict: RawSchema.Dict[?, ?] =>
        if dict.write == null then missingWriteCapability(schema)
        Expr.NamedTupleExpr(
          dict.write
            .iterator(value)
            .map((key, elem) => key -> writeExpr(dict.element, elem))
            .toIndexedSeq
        )
      case option: RawSchema.Option[?] =>
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
      case RawSchema.RawNumber =>
        // Expr has no arbitrary-precision constant; routers over raw numbers must select another
        // case (e.g. String) on the write side.
        throw IllegalStateException("a raw number value cannot be written to an Expr tree")

  def renderText(
      schema: RawSchema[?],
      value: Any,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit = schema match
    case mapped: RawSchema.Mapped[?, ?] =>
      renderText(mapped.base, mappedInput(mapped.mapping, value), out, depth)
    case RawSchema.Ref(_, target) =>
      renderText(target(), value, out, depth)
    case router: RawSchema.Router[?] =>
      renderText(selectedRouterCase(router, value).schema, value, out, depth)
    case namedTuple: RawSchema.NamedTuple[?] =>
      val write = namedTuple.write
      if write == null then missingWriteCapability(schema)
      val fields = namedTuple.fields
      ExprRenderer.renderNamedTuple(out, depth, fields.length) { index =>
        val field = fields(index)
        IdentifierSyntax.appendIdentifier(field.name, out)
        out.appendToken('=')
        renderFieldText(field.schema, write, value, index, out, depth + 1)
      }
    case tuple: RawSchema.Tuple[?] =>
      val write = tuple.write
      if write == null then missingWriteCapability(schema)
      val slots = tuple.slots
      ExprRenderer.renderTuple(out, depth, write.size(value)) { index =>
        renderTupleSlotText(slots(index), write, value, index, out, depth + 1)
      }
    case RawSchema.PartialNamedTuple(base, _) =>
      renderText(base, value, out, depth)
    case sum: RawSchema.Sum[?] =>
      val write = sum.write
      if write == null then missingWriteCapability(schema)
      val sumCase = sum.cases(write.caseIndex(value))
      ExprRenderer.renderNamedTuple(out, depth, 1) { _ =>
        IdentifierSyntax.appendIdentifier(sumCase.name, out)
        out.appendToken('=')
        renderText(sumCase.schema, value, out, depth + 1)
      }
    case sum: RawSchema.DiscriminatorSum[?] =>
      if sum.write == null then missingWriteCapability(schema)
      ExprRenderer.renderExpr(writeExpr(sum, value), out, depth)
    case vector: RawSchema.Vector[?, ?] =>
      val write = vector.write
      if write == null then missingWriteCapability(schema)
      write match
        case indexed: RawSchema.IndexedVectorWrite =>
          ExprRenderer.renderVector(out, depth, indexed.size(value)) { index =>
            renderElementText(vector.element, indexed, value, index, out, depth + 1)
          }
        case _ =>
          val values = write.iterator(value)
          ExprRenderer.renderVector(out, depth, write.size(value)) { _ =>
            renderText(vector.element, values.next(), out, depth + 1)
          }
    case tupleOf: RawSchema.TupleOf[?, ?] =>
      val write = tupleOf.write
      if write == null then missingWriteCapability(schema)
      write match
        case indexed: RawSchema.IndexedVectorWrite =>
          ExprRenderer.renderTuple(out, depth, indexed.size(value)) { index =>
            renderElementText(tupleOf.element, indexed, value, index, out, depth + 1)
          }
        case _ =>
          val values = write.iterator(value)
          ExprRenderer.renderTuple(out, depth, write.size(value)) { _ =>
            renderTupleElement(tupleOf.element, values.next(), out, depth + 1)
          }
    case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
      val write = pairSeq.write
      if write == null then missingWriteCapability(schema)
      val values = write.iterator(value)
      ExprRenderer.renderVector(out, depth, write.size(value)) { _ =>
        val (key, elem) = values.next()
        renderPair(pairSeq, key, elem, out, depth + 1)
      }
    case dict: RawSchema.Dict[?, ?] =>
      val write = dict.write
      if write == null then missingWriteCapability(schema)
      val values = write.iterator(value)
      ExprRenderer.renderNamedTuple(out, depth, write.size(value)) { _ =>
        val (key, elem) = values.next()
        IdentifierSyntax.appendIdentifier(key, out)
        out.appendToken('=')
        renderText(dict.element, elem, out, depth + 1)
      }
    case option: RawSchema.Option[?] =>
      value.asInstanceOf[ScalaOption[Any]] match
        case Some(innerValue) => renderText(option.inner, innerValue, out, depth)
        case None             => out.append("null")
    case RawSchema.String =>
      ExprRenderer.renderStringLiteral(value.asInstanceOf[String], out)
    case RawSchema.Char =>
      ExprRenderer.renderCharLiteral(value.asInstanceOf[Char], out)
    case RawSchema.Int =>
      out.append(value.asInstanceOf[Int])
    case RawSchema.Long =>
      out.append(value.asInstanceOf[Long])
      out.append('L')
    case RawSchema.Float =>
      ExprRenderer.renderFloatLiteral(value.asInstanceOf[Float], out)
    case RawSchema.Double =>
      ExprRenderer.renderDoubleLiteral(value.asInstanceOf[Double], out)
    case RawSchema.Boolean =>
      out.append(value.asInstanceOf[Boolean])
    case RawSchema.Null =>
      out.append("null")
    case RawSchema.RawNumber =>
      // the mapped input is the number's text, rendered bare so it reads back as a number literal
      out.append(value.asInstanceOf[String])

  private def writeDiscriminatorPayload(schema: RawSchema[?], value: Any): IndexedSeq[
    (name: String, value: Expr)
  ] =
    schema match
      case RawSchema.PartialNamedTuple(base, _) =>
        writeDiscriminatorPayload(base, value)
      case mapped: RawSchema.Mapped[?, ?] =>
        writeDiscriminatorPayload(mapped.base, mappedInput(mapped.mapping, value))
      case RawSchema.Ref(_, target) =>
        writeDiscriminatorPayload(target(), value)
      case namedTuple: RawSchema.NamedTuple[?] =>
        if namedTuple.write == null then missingWriteCapability(namedTuple)
        val access     = namedTuple.write
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
      schema: RawSchema[?],
      value: Any,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    renderText(schema, value, out, depth)

  /** Renders field `index` of `value`, pulling it through the write's typed accessor when the field
    * schema pins it to a primitive — no box is allocated for the transfer. Non-primitive fields go
    * through the boxed accessor and the general renderer.
    */
  private def renderFieldText(
      schema: RawSchema[?],
      write: RawSchema.NamedTupleWrite,
      value: Any,
      index: Int,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case RawSchema.Int =>
        out.append(write.intFieldValue(value, index))
      case RawSchema.String =>
        ExprRenderer.renderStringLiteral(write.stringFieldValue(value, index), out)
      case RawSchema.Boolean =>
        out.append(write.booleanFieldValue(value, index))
      case RawSchema.Long =>
        out.append(write.longFieldValue(value, index))
        out.append('L')
      case RawSchema.Double =>
        ExprRenderer.renderDoubleLiteral(write.doubleFieldValue(value, index), out)
      case RawSchema.Float =>
        ExprRenderer.renderFloatLiteral(write.floatFieldValue(value, index), out)
      case RawSchema.Char =>
        ExprRenderer.renderCharLiteral(write.charFieldValue(value, index), out)
      case other =>
        renderText(other, write.fieldValue(value, index), out, depth)

  /** renders tuple slot `index` of `value` — see [[renderFieldText]] */
  private def renderTupleSlotText(
      schema: RawSchema[?],
      write: RawSchema.TupleWrite,
      value: Any,
      index: Int,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case RawSchema.Int =>
        out.append(write.intElementValue(value, index))
      case RawSchema.String =>
        ExprRenderer.renderStringLiteral(write.stringElementValue(value, index), out)
      case RawSchema.Boolean =>
        out.append(write.booleanElementValue(value, index))
      case RawSchema.Long =>
        out.append(write.longElementValue(value, index))
        out.append('L')
      case RawSchema.Double =>
        ExprRenderer.renderDoubleLiteral(write.doubleElementValue(value, index), out)
      case RawSchema.Float =>
        ExprRenderer.renderFloatLiteral(write.floatElementValue(value, index), out)
      case RawSchema.Char =>
        ExprRenderer.renderCharLiteral(write.charElementValue(value, index), out)
      case other =>
        renderText(other, write.elementValue(value, index), out, depth)

  /** renders element `index` of an indexed vector `value` — see [[renderFieldText]] */
  private def renderElementText(
      schema: RawSchema[?],
      write: RawSchema.IndexedVectorWrite,
      value: Any,
      index: Int,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    schema match
      case RawSchema.Int =>
        out.append(write.intElementValue(value, index))
      case RawSchema.String =>
        ExprRenderer.renderStringLiteral(write.stringElementValue(value, index), out)
      case RawSchema.Boolean =>
        out.append(write.booleanElementValue(value, index))
      case RawSchema.Long =>
        out.append(write.longElementValue(value, index))
        out.append('L')
      case RawSchema.Double =>
        ExprRenderer.renderDoubleLiteral(write.doubleElementValue(value, index), out)
      case RawSchema.Float =>
        ExprRenderer.renderFloatLiteral(write.floatElementValue(value, index), out)
      case RawSchema.Char =>
        ExprRenderer.renderCharLiteral(write.charElementValue(value, index), out)
      case other =>
        renderText(other, write.elementValue(value, index), out, depth)

  private def renderPair(
      schema: RawSchema.PairSeq[?, ?, ?],
      key: Any,
      value: Any,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    ExprRenderer.renderTuple(out, depth, 2) { index =>
      if index == 0 then renderTupleElement(schema.key, key, out, depth + 1)
      else renderTupleElement(schema.value, value, out, depth + 1)
    }
