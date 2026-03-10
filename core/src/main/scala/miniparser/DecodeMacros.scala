package miniparser

import scala.quoted.{Expr as QExpr, Quotes, quotes, Type, Varargs}
import NamedTuple.NamedTuple
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

object DecodeMacros:
  def namedTupleDecoderImpl[Names <: Tuple: Type, Values <: Tuple: Type](using Quotes): QExpr[AstDecoder[NamedTuple[Names, Values]]] =
    '{
      AstDecoder.RawDecoder(${namedTupleSchemaImpl[Names, Values]})
        .asInstanceOf[AstDecoder[NamedTuple[Names, Values]]]
    }

  def namedTupleSchemaImpl[Names <: Tuple: Type, Values <: Tuple: Type](using Quotes): QExpr[Schema] =
    namedTupleSchemaExpr[Names, Values](Nil)

  private def namedTupleSchemaExpr[Names <: Tuple: Type, Values <: Tuple: Type](path: List[String])(using Quotes): QExpr[Schema] =
    namedTupleSchemaExprFromTypes(Type.of[Names], Type.of[Values], path)

  private def namedTupleSchemaExprFromTypes(namesType: Type[?], valuesType: Type[?], path: List[String])(using Quotes): QExpr[Schema] =
    '{ Schema.NamedTuple(IArray(${Varargs(fieldExprsFromTypes(namesType, valuesType, path))}*)) }

  private def fieldExprsFromTypes(namesType: Type[?], valuesType: Type[?], path: List[String])(using Quotes): List[QExpr[Schema.Field]] =
    import quotes.reflect.report

    @tailrec
    def loop(
        namesType: Type[?], valuesType: Type[?])(
        acc: List[QExpr[Schema.Field]], path: List[String]
    ): List[QExpr[Schema.Field]] =
      (namesType, valuesType) match
        case ('[EmptyTuple], '[EmptyTuple]) =>
          acc.reverse
        case ('[name *: tailNames], '[value *: tailValues]) =>
          val fieldName: String =
            Type.valueOfConstant[name]
              .map(_.asInstanceOf[String])
              .getOrElse(report.errorAndAbort("Named tuple field names must be singleton string literals"))

          val headSchema = fetchFieldDecoder[value](path :+ fieldName)
          loop(Type.of[tailNames], Type.of[tailValues])('{ Schema.Field(${QExpr(fieldName)}, $headSchema) } :: acc, path)
        case _ =>
          report.errorAndAbort("Named tuple names and values must have the same shape")

    loop(namesType, valuesType)(Nil, path)

  private def fetchFieldDecoder[T: Type](path: List[String])(using Quotes): QExpr[Schema] =
    import quotes.reflect.*

    def renderedPath =
      if path.isEmpty then "<root>"
      else path.mkString(".")

    def fetch() =
      val decoder = QExpr.summon[AstDecoder[T]].getOrElse(
        report.errorAndAbort(s"No AstDecoder available at path '$renderedPath' for type ${Type.show[T]}")
      )
      '{ $decoder.schema }

    Type.of[T] match
      case '[Null] => '{ Schema.Null } // REQUIRED **FIRST** OR ELSE VECTOR TYPE WILL MATCH
      case '[Vector[element]] =>
        val inner = fetchFieldDecoder[element](path :+ "[]")
        '{ Schema.Vector($inner) }
      case '[type namedTuple <: NamedTuple.AnyNamedTuple; `namedTuple`] =>
        namedTupleSchemaExprFromTypes(Type.of[NamedTuple.Names[namedTuple]], Type.of[NamedTuple.DropNames[namedTuple]], path)
      case _ => fetch()
