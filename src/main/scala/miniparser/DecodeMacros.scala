package miniparser

import scala.quoted.{Expr as QExpr, Quotes, Type, Varargs}

object DecodeMacros:
  def namedTupleDecoderImpl[Names <: Tuple: Type, Values <: Tuple: Type](using q: Quotes): QExpr[AstDecoder[_root_.scala.NamedTuple.NamedTuple[Names, Values]]] =
    '{
      new AstDecoder[_root_.scala.NamedTuple.NamedTuple[Names, Values]]:
        val schema: Schema = Schema.NamedTuple(IArray(${Varargs(fieldExprs[Names, Values])}*))
    }

  def namedTupleSchemaImpl[Names <: Tuple: Type, Values <: Tuple: Type](using q: Quotes): QExpr[Schema] =
    '{ Schema.NamedTuple(IArray(${Varargs(fieldExprs[Names, Values])}*)) }

  private def fieldExprs[Names <: Tuple: Type, Values <: Tuple: Type](using q: Quotes): List[QExpr[Schema.Field]] =
    import q.reflect.report

    (Type.of[Names], Type.of[Values]) match
      case ('[EmptyTuple], '[EmptyTuple]) =>
        Nil
      case ('[name *: tailNames], '[value *: tailValues]) =>
        val fieldName: String =
          Type.valueOfConstant[name]
            .map(_.asInstanceOf[String])
            .getOrElse(report.errorAndAbort("Named tuple field names must be singleton string literals"))

        val headDecoder =
          QExpr.summon[AstDecoder[value]]
            .getOrElse(report.errorAndAbort(s"No AstDecoder available for field '$fieldName'"))

        val headSchema = '{ $headDecoder.schema }

        '{ Schema.Field(${QExpr(fieldName)}, $headSchema) } :: fieldExprs[tailNames, tailValues]
      case _ =>
        report.errorAndAbort("Named tuple names and values must have the same shape")