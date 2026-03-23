package scalanotation

import scalanotation.Writer.Builders.AtPath
import scalanotation.internal.CommonDerivationBuilders
import scalanotation.internal.ExprRenderer
import scalanotation.internal.PublicInternal.showType
import scalanotation.internal.RawSchema

import scala.NamedTuple.NamedTuple
import scala.deriving.Mirror
import scala.util.NotGiven

sealed trait Writer[T]:
  private[scalanotation] def schema: RawSchema
  def write(value: T): Expr

  private[scalanotation] def renderText(
      value: T,
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit

  final def contramap[U](f: U => T): Writer[U] =
    Writer.contramapped(this)(f)

  final def writeText(value: T): String =
    Writer.renderText(this, value, TextFormat.compact)

  final def writeText(value: T, format: TextFormat): String =
    Writer.renderText(this, value, format)

  final def writePrettyText(value: T, indent: Int = 2): String =
    writeText(value, TextFormat.pretty(indent))

  final def writeDecl(name: String, value: T): String =
    Writer.renderDecl(this, name, value, TextFormat.compact)

  final def writeDecl(name: String, value: T, format: TextFormat): String =
    Writer.renderDecl(this, name, value, format)

  final def writeDeclPretty(name: String, value: T, indent: Int = 2): String =
    writeDecl(name, value, TextFormat.pretty(indent))

object Writer {
  private type TextEncoder[T] = (T, ExprRenderer.Output, Int, TextFormat) => Unit

  final case class Field(name: String, writer: Writer[Any]):
    def schemaField: RawSchema.Field = RawSchema.Field(name, writer.schema)

  final case class SumCase(name: String, writer: Writer[Any]):
    def schemaCase: RawSchema.SumCase = RawSchema.SumCase(name, writer.schema)

  private final class Instance[T](
      val schema: RawSchema,
      encodeExpr: T => Expr,
      encodeText: TextEncoder[T]
  ) extends Writer[T]:
    def write(value: T): Expr = encodeExpr(value)

    def renderText(
        value: T,
        out: ExprRenderer.Output,
        depth: Int
    )(using format: TextFormat): Unit =
      encodeText(value, out, depth, format)

  private def instance[T](schema: RawSchema)(encode: T => Expr)(render: TextEncoder[T]): Writer[T] =
    Instance(schema, encode, render)

  private[scalanotation] def renderText[T](
      writer: Writer[T],
      value: T,
      format: TextFormat
  ): String =
    val out = ExprRenderer.Output()
    writer.renderText(value, out, 0)(using format)
    out.result()

  private[scalanotation] def renderDecl[T](
      writer: Writer[T],
      name: String,
      value: T,
      format: TextFormat
  ): String =
    val out = ExprRenderer.Output()
    out.append("val ")
    out.append(name)
    out.append(" = ")
    writer.renderText(value, out, 0)(using format)
    out.result()

  private def encodeProduct(
      value: Product,
      fields: IArray[Field]
  ): Expr =
    val fieldExprs = IArray.newBuilder[(name: String, value: Expr)]
    var index      = 0
    while index < fields.length do
      val field = fields(index)
      fieldExprs += ((field.name, field.writer.write(value.productElement(index))))
      index += 1
    Expr.NamedTupleExpr(fieldExprs.result())

  private def renderProductText(
      value: Product,
      fields: IArray[Field],
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    ExprRenderer.renderNamedTuple(out, depth, fields.length) { index =>
      val field = fields(index)
      out.append(field.name)
      out.append(" = ")
      field.writer.renderText(value.productElement(index), out, depth + 1)
    }

  private def renderVectorText[A](
      values: Iterator[A],
      size: Int,
      writer: Writer[A],
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    ExprRenderer.renderVector(out, depth, size) { _ =>
      writer.renderText(values.next(), out, depth + 1)
    }

  private def renderDictText[A](
      values: Iterator[(String, A)],
      size: Int,
      writer: Writer[A],
      out: ExprRenderer.Output,
      depth: Int
  )(using format: TextFormat): Unit =
    ExprRenderer.renderNamedTuple(out, depth, size) { _ =>
      val (key, value) = values.next()
      out.append(key)
      out.append(" = ")
      writer.renderText(value, out, depth + 1)
    }

  def contramapped[A, B](base: Writer[A])(transform: B => A): Writer[B] =
    instance[B](base.schema)(value => base.write(transform(value))) { (value, out, depth, format) =>
      base.renderText(transform(value), out, depth)(using format)
    }

  inline def derived[T](using mirror: Mirror.Of[T]): Writer[T] =
    inline mirror match
      case m: Mirror.ProductOf[T] =>
        compiletime.summonFrom {
          case _: (m.MirroredElemTypes =:= EmptyTuple) =>
            val label = compiletime.constValue[m.MirroredLabel]
            singleton[T](using m)(using ValueOf(label))
          case _ =>
            ofFields[T](using m)(
              using compiletime.summonInline[
                Builders.ProductFieldsAtPath["", m.MirroredElemLabels, m.MirroredElemTypes]
              ]
            )
        }
      case m: Mirror.SumOf[T] =>
        ofCases[T](using m)(
          using compiletime.summonInline[
            Builders.SumCasesAtPath["", T, m.MirroredElemLabels, m.MirroredElemTypes]
          ]
        )

  def ofFields[T](using mirror: Mirror.ProductOf[T])(
      using
      atPath: Builders.ProductFieldsAtPath["", mirror.MirroredElemLabels, mirror.MirroredElemTypes],
      hasFields: NotGiven[mirror.MirroredElemTypes =:= EmptyTuple]
  ): Writer[T] =
    Builders.productTypeClass[T](atPath.fields)

  def singleton[T](using mirror: Mirror.ProductOf[T])(
      using label: ValueOf[mirror.MirroredLabel],
      noFields: mirror.MirroredElemTypes =:= EmptyTuple
  ): Writer[T] =
    Builders.singletonTypeClass[T](label.value)

  def forNull[T]: Writer[T] =
    instance[T](RawSchema.Nullary)(_ => Expr.NullConstant) { (_, out, _, _) =>
      out.append("null")
    }

  def ofCases[T](using mirror: Mirror.SumOf[T])(
      using casesAtPath: Builders.SumCasesAtPath[
        "",
        T,
        mirror.MirroredElemLabels,
        mirror.MirroredElemTypes
      ]
  ): Writer[T] =
    val cases  = IArray.from(casesAtPath.cases)
    val schema = RawSchema.Sum(cases.iterator.map(c => c.name -> c.schemaCase).toMap)
    instance[T](schema) { value =>
      val sumCase = cases(mirror.ordinal(value))
      Expr.NamedTupleExpr(IndexedSeq(sumCase.name -> sumCase.writer.write(value)))
    } { (value, out, depth, format) =>
      val sumCase = cases(mirror.ordinal(value))
      ExprRenderer.renderNamedTuple(out, depth, 1) { _ =>
        out.append(sumCase.name)
        out.append(" = ")
        sumCase.writer.renderText(value, out, depth + 1)(using format)
      }(using format)
    }

  given ExprSchema: Writer[Expr] =
    instance[Expr](RawSchema.AnyExpr)(identity) { (value, out, depth, format) =>
      ExprRenderer.renderExpr(value, out, depth)(using format)
    }

  given StringSchema: Writer[String] =
    instance[String](RawSchema.String)(Expr.StringConstant.apply) { (value, out, _, _) =>
      ExprRenderer.renderStringLiteral(value, out)
    }

  given CharSchema: Writer[Char] =
    instance[Char](RawSchema.Char)(Expr.CharConstant.apply) { (value, out, _, _) =>
      ExprRenderer.renderCharLiteral(value, out)
    }

  given IntSchema: Writer[Int] =
    instance[Int](RawSchema.Int)(Expr.IntConstant.apply) { (value, out, _, _) =>
      out.append(value.toString)
    }

  given LongSchema: Writer[Long] =
    instance[Long](RawSchema.Long)(Expr.LongConstant.apply) { (value, out, _, _) =>
      out.append(s"${value}L")
    }

  given FloatSchema: Writer[Float] =
    instance[Float](RawSchema.Float)(Expr.FloatConstant.apply) { (value, out, _, _) =>
      ExprRenderer.renderFloatLiteral(value, out)
    }

  given DoubleSchema: Writer[Double] =
    instance[Double](RawSchema.Double)(Expr.DoubleConstant.apply) { (value, out, _, _) =>
      ExprRenderer.renderDoubleLiteral(value, out)
    }

  given BooleanSchema: Writer[Boolean] =
    instance[Boolean](RawSchema.Boolean)(Expr.BooleanConstant.apply) { (value, out, _, _) =>
      out.append(value.toString)
    }

  given OptionSchema: [T] => (atPath: AtPath["", Option[T]]) => Writer[Option[T]] =
    atPath.typeclass

  given VectorSchema: [T] => (atPath: AtPath["", Vector[T]]) => Writer[Vector[T]] =
    atPath.typeclass

  given IArraySchema: [T] => (atPath: AtPath["", IArray[T]]) => Writer[IArray[T]] =
    atPath.typeclass

  given ArraySchema: [T] => (atPath: AtPath["", Array[T]]) => Writer[Array[T]] =
    atPath.typeclass

  given SeqSchema: [Col[X] <: scala.collection.Seq[X], T] => (atPath: AtPath["", Col[T]])
    => Writer[Col[T]] =
    atPath.typeclass

  given MapSchema
      : [Col[X, Y] <: scala.collection.Map[X, Y], T] => (atPath: AtPath["", Col[String, T]])
        => Writer[Col[String, T]] =
    atPath.typeclass

  given NamedTupleSchema: [NT <: NamedTuple.AnyNamedTuple]
    => (atPath: AtPath["", NT]) => Writer[NT] =
    atPath.typeclass

  object Builders extends CommonDerivationBuilders[Writer] {
    type FieldRepr      = Field
    type SumCaseRepr[A] = SumCase

    import compiletime.ops.string.+

    private[scalanotation] inline def typeClassName: String = "Writer"

    private def namedTupleWriter[T](fields: List[FieldRepr]): Writer[T] =
      val array = IArray.from(fields)
      instance[T](RawSchema.NamedTuple(IArray.from(array.iterator.map(_.schemaField)))) { value =>
        encodeProduct(value.asInstanceOf[Product], array)
      } { (value, out, depth, format) =>
        renderProductText(value.asInstanceOf[Product], array, out, depth)(using format)
      }

    private[scalanotation] def makeField[T](name: String, typeclass: Writer[T]): FieldRepr =
      Field(name, typeclass.asInstanceOf[Writer[Any]])

    private[scalanotation] def namedTupleTypeClass[T](fields: List[FieldRepr]): Writer[T] =
      namedTupleWriter(fields)

    private[scalanotation] def productTypeClass[T](fields: List[FieldRepr])(
        using mirror: Mirror.ProductOf[T]
    ): Writer[T] =
      namedTupleWriter(fields)

    private[scalanotation] def singletonTypeClass[T](label: String)(
        using mirror: Mirror.ProductOf[T],
        noFields: mirror.MirroredElemTypes =:= EmptyTuple
    ): Writer[T] =
      val schema = RawSchema.NamedTuple(IArray(RawSchema.Field(label, RawSchema.Nullary)))
      instance[T](schema) { _ =>
        Expr.NamedTupleExpr(IndexedSeq(label -> Expr.NullConstant))
      } { (_, out, depth, format) =>
        ExprRenderer.renderNamedTuple(out, depth, 1) { _ =>
          out.append(label)
          out.append(" = ")
          out.append("null")
        }(using format)
      }

    private[scalanotation] def nullaryEnumCaseTypeClass[T](
        using mirror: Mirror.ProductOf[T],
        empty: mirror.MirroredElemTypes =:= EmptyTuple
    ): Writer[T] =
      forNull[T]

    private[scalanotation] def sumCaseTypeClass[A, T <: A](
        name: String,
        typeclass: Writer[T]
    ): SumCaseRepr[A] =
      SumCase(name, typeclass.asInstanceOf[Writer[Any]])

    given VectorAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Vector[T]] =
      val elementWriter = wrapped.typeclass
      liftAtPath[Path, Vector[T]](
        instance[Vector[T]](RawSchema.Vector(elementWriter.schema)) { values =>
          Expr.VectorExpr(values.map(elementWriter.write))
        } { (values, out, depth, format) =>
          renderVectorText(values.iterator, values.length, elementWriter, out, depth)(using format)
        }
      )

    given SeqAtPath: [Path <: String, T, Col[X] <: scala.collection.Seq[X]]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Col[T]] =
      val elementWriter = wrapped.typeclass
      liftAtPath[Path, Col[T]](
        instance[Col[T]](RawSchema.Vector(elementWriter.schema)) { values =>
          Expr.VectorExpr(values.iterator.map(elementWriter.write).toIndexedSeq)
        } { (values, out, depth, format) =>
          renderVectorText(values.iterator, values.size, elementWriter, out, depth)(using format)
        }
      )

    given IArrayAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, IArray[T]] =
      val elementWriter = wrapped.typeclass
      liftAtPath[Path, IArray[T]](
        instance[IArray[T]](RawSchema.Vector(elementWriter.schema)) { values =>
          Expr.VectorExpr(values.iterator.map(elementWriter.write).toIndexedSeq)
        } { (values, out, depth, format) =>
          renderVectorText(values.iterator, values.length, elementWriter, out, depth)(using format)
        }
      )

    given ArrayAtPath: [Path <: String, T]
      => (wrapped: AtPath[Path + "[]", T])
      => AtPath[Path, Array[T]] =
      val elementWriter = wrapped.typeclass
      liftAtPath[Path, Array[T]](
        instance[Array[T]](RawSchema.Vector(elementWriter.schema)) { values =>
          Expr.VectorExpr(values.iterator.map(elementWriter.write).toIndexedSeq)
        } { (values, out, depth, format) =>
          renderVectorText(values.iterator, values.length, elementWriter, out, depth)(using format)
        }
      )

    given MapAtPath: [Path <: String, T, Col[X, Y] <: scala.collection.Map[X, Y]]
      => (wrapped: AtPath[Path + ".*", T])
      => AtPath[Path, Col[String, T]] =
      val elementWriter = wrapped.typeclass
      liftAtPath[Path, Col[String, T]](
        instance[Col[String, T]](RawSchema.Dict(elementWriter.schema)) { values =>
          Expr.NamedTupleExpr(
            values.iterator.map((key, value) => key -> elementWriter.write(value)).toIndexedSeq
          )
        } { (values, out, depth, format) =>
          renderDictText(values.iterator, values.size, elementWriter, out, depth)(using format)
        }
      )

    given OptionAtPath: [Path <: String, T]
      => NonNestedOption[Path, T]
      => (wrapped: AtPath[Path, T])
      => AtPath[Path, Option[T]] =
      val elementWriter = wrapped.typeclass
      liftAtPath[Path, Option[T]](
        instance[Option[T]](RawSchema.Option(elementWriter.schema)) {
          case Some(value) => elementWriter.write(value)
          case None        => Expr.NullConstant
        } { (value, out, depth, format) =>
          value match
            case Some(inner) => elementWriter.renderText(inner, out, depth)(using format)
            case None        => out.append("null")
        }
      )
  }
}
