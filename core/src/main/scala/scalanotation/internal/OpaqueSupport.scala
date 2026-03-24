package scalanotation.internal

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.ReadWriter
import scalanotation.Reader
import scalanotation.TextFormat
import scalanotation.Writer
import scalanotation.internal.TokenDecoder.describe
import steps.result.Result

private[scalanotation] object OpaqueSupport:
  final case class Read[T](
      rawSchema: RawSchema,
      tokenRead: TokenDecoder => Result[T, DecodeError],
      exprRead: (ExprDecoder, Expr) => Result[T, DecodeError]
  )

  object Read:
    def primitive[T](schema0: RawSchema)(
        tokenRead0: TokenDecoder => Result[T, DecodeError],
        exprRead0: Expr => Result[T, DecodeError]
    ): Read[T] =
      Read(schema0, tokenRead0, (_, expr) => exprRead0(expr))

    def mapped[A, B](base: Reader[A])(transform: A => Result[B, DecodeError]): Read[B] =
      Read(
        rawSchema = base.schema,
        tokenRead = decoder => decoder.decodeTaggedAs(base).flatMap(transform),
        exprRead = (decoder, expr) => decoder.decodeInto(base, expr).flatMap(transform)
      )

    def nullary[T](value: T): Read[T] =
      primitive(RawSchema.Nullary)(
        decoder => decoder.decodeNullValue(value),
        expr =>
          expr match
            case Expr.NullConstant => Result.Ok(value)
            case other             =>
              Result.Err(
                DecodeError.ExpectedType(RawSchema.Nullary.describeSelf, describe(other))
              )
      )

  final case class Write[T](
      rawSchema: RawSchema,
      exprWrite: T => Expr,
      textWrite: (T, ExprRenderer.Output, Int, TextFormat) => Unit
  )

  object Write:
    def contramapped[A, B](base: Writer[A])(transform: B => A): Write[B] =
      Write(
        rawSchema = base.schema,
        exprWrite = value => base.write(transform(value)),
        textWrite = (value, out, depth, format) =>
          base.renderText(transform(value), out, depth)(using format)
      )

    def nullary[T]: Write[T] =
      Write(
        rawSchema = RawSchema.Nullary,
        exprWrite = _ => Expr.NullConstant,
        textWrite = (_, out, _, _) => out.append("null")
      )

  final case class ReadWrite[T](read: Read[T], write: Write[T]):
    require(read.rawSchema == write.rawSchema, "ReadWriter support must share the same RawSchema")

    def rawSchema: RawSchema = read.rawSchema

  object ReadWrite:
    def primitive[T](schema0: RawSchema)(
        tokenRead0: TokenDecoder => Result[T, DecodeError],
        exprRead0: Expr => Result[T, DecodeError],
        exprWrite0: T => Expr
    )(
        textWrite0: (T, ExprRenderer.Output, Int, TextFormat) => Unit
    ): ReadWrite[T] =
      ReadWrite(
        Read.primitive(schema0)(tokenRead0, exprRead0),
        Write(schema0, exprWrite0, textWrite0)
      )

    def mapped[A, B](base: ReadWriter[A])(
        readTransform: A => Result[B, DecodeError]
    )(
        writeTransform: B => A
    ): ReadWrite[B] =
      val baseReader = base.reader
      val baseWriter = base.writer
      ReadWrite(
        Read(
          rawSchema = base.schema,
          tokenRead = decoder => decoder.decodeTaggedAs(baseReader).flatMap(readTransform),
          exprRead = (decoder, expr) => decoder.decodeInto(baseReader, expr).flatMap(readTransform)
        ),
        Write(
          rawSchema = base.schema,
          exprWrite = value => baseWriter.write(writeTransform(value)),
          textWrite = (value, out, depth, format) =>
            baseWriter.renderText(writeTransform(value), out, depth)(using format)
        )
      )

    def nullary[T](value: T): ReadWrite[T] =
      ReadWrite(Read.nullary(value), Write.nullary)

  object Primitives:
    private def expectedScalar[T](schema: RawSchema & RawSchema.Scalar)(
        extract: PartialFunction[Expr, T]
    ): Expr => Result[T, DecodeError] =
      expr =>
        expr match
          case extract(value) => Result.Ok(value)
          case other          =>
            Result.Err(DecodeError.ExpectedType(schema.describeSelf, describe(other)))

    private def scalar[T](schema: RawSchema & RawSchema.Scalar)(
        tokenRead0: TokenDecoder => Result[T, DecodeError],
        exprExtract: PartialFunction[Expr, T],
        exprWrite0: T => Expr
    )(
        textWrite0: (T, ExprRenderer.Output, Int, TextFormat) => Unit
    ): ReadWrite[T] =
      ReadWrite.primitive(schema)(
        tokenRead0,
        expectedScalar(schema)(exprExtract),
        exprWrite0
      )(textWrite0)

    val ExprCodec: ReadWrite[Expr] =
      ReadWrite.primitive(RawSchema.AnyExpr)(
        decoder => decoder.decodeAnyExpr(),
        expr => Result.Ok(expr),
        identity
      ) { (value, out, depth, format) =>
        ExprRenderer.renderExpr(value, out, depth)(using format)
      }

    val StringCodec: ReadWrite[String] =
      scalar(RawSchema.String)(
        decoder => decoder.decodeString(identity),
        { case Expr.StringConstant(value) => value },
        Expr.StringConstant.apply
      ) { (value, out, _, _) =>
        ExprRenderer.renderStringLiteral(value, out)
      }

    val CharCodec: ReadWrite[Char] =
      scalar(RawSchema.Char)(
        decoder => decoder.decodeChar(identity),
        { case Expr.CharConstant(value) => value },
        Expr.CharConstant.apply
      ) { (value, out, _, _) =>
        ExprRenderer.renderCharLiteral(value, out)
      }

    val IntCodec: ReadWrite[Int] =
      scalar(RawSchema.Int)(
        decoder => decoder.decodeInt(identity),
        { case Expr.IntConstant(value) => value },
        Expr.IntConstant.apply
      ) { (value, out, _, _) =>
        out.append(value.toString)
      }

    val LongCodec: ReadWrite[Long] =
      scalar(RawSchema.Long)(
        decoder => decoder.decodeLong(identity),
        { case Expr.LongConstant(value) => value },
        Expr.LongConstant.apply
      ) { (value, out, _, _) =>
        out.append(s"${value}L")
      }

    val FloatCodec: ReadWrite[Float] =
      scalar(RawSchema.Float)(
        decoder => decoder.decodeFloat(identity),
        { case Expr.FloatConstant(value) => value },
        Expr.FloatConstant.apply
      ) { (value, out, _, _) =>
        ExprRenderer.renderFloatLiteral(value, out)
      }

    val DoubleCodec: ReadWrite[Double] =
      scalar(RawSchema.Double)(
        decoder => decoder.decodeDouble(identity),
        { case Expr.DoubleConstant(value) => value },
        Expr.DoubleConstant.apply
      ) { (value, out, _, _) =>
        ExprRenderer.renderDoubleLiteral(value, out)
      }

    val BooleanCodec: ReadWrite[Boolean] =
      scalar(RawSchema.Boolean)(
        decoder => decoder.decodeBoolean(identity),
        { case Expr.BooleanConstant(value) => value },
        Expr.BooleanConstant.apply
      ) { (value, out, _, _) =>
        out.append(value.toString)
      }

  private def widen[T](result: Result[T, DecodeError]): Result[Any, DecodeError] =
    result.asInstanceOf[Result[Any, DecodeError]]

  def compiled[T](support: Read[T]): CompiledSchema =
    CompiledSchema.Opaque(
      rawSchema = support.rawSchema,
      tokenRead = decoder => widen(support.tokenRead(decoder)),
      exprRead = (decoder, expr) => widen(support.exprRead(decoder, expr))
    )

  def compiled[T](support: Write[T]): CompiledSchema =
    CompiledSchema.Opaque(
      rawSchema = support.rawSchema,
      exprWrite = value => support.exprWrite(value.asInstanceOf[T]),
      textWrite = (value, out, depth, format) =>
        support.textWrite(value.asInstanceOf[T], out, depth, format)
    )

  def compiled[T](support: ReadWrite[T]): CompiledSchema =
    CompiledSchema.Opaque(
      rawSchema = support.rawSchema,
      tokenRead = decoder => widen(support.read.tokenRead(decoder)),
      exprRead = (decoder, expr) => widen(support.read.exprRead(decoder, expr)),
      exprWrite = value => support.write.exprWrite(value.asInstanceOf[T]),
      textWrite = (value, out, depth, format) =>
        support.write.textWrite(value.asInstanceOf[T], out, depth, format)
    )
