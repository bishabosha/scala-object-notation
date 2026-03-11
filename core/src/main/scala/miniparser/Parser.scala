package scalanotation

import scala.NamedTuple

import scala.collection.mutable

object Parser:
  object quick:
    def parse(input: String): SourceFile[Expr] =
      Tokenizer.tokenize(input)
        .left.map(DecodeError.UnexpectedToken(_))
        .flatMap(parseAs[Expr](_)) match
          case Right(value) => value
          case Left(err) => throw IllegalArgumentException(err.format)

  def parseAs[T: TaggedSchema as decoder](input: String): Either[DecodeError, SourceFile[T]] =
    Tokenizer.tokenize(input) match
      case Left(value) => Left(DecodeError.UnexpectedToken(value))
      case Right(tokens) => parseAs(tokens)

  def parseAs[T: TaggedSchema as decoder](input: IArray[Token]): Either[DecodeError, SourceFile[T]] =
    SchemaTokenDecoder.decodeAnyRoot(input, decoder)

  def parseValueAs[T: TaggedSchema as decoder](input: String, name: String): Either[DecodeError, T] =
    Tokenizer.tokenize(input) match
      case Left(value) => Left(DecodeError.UnexpectedToken(value))
      case Right(tokens) => parseValueAs(tokens, name)

  def parseValueAs[T: TaggedSchema as decoder](input: IArray[Token], name: String): Either[DecodeError, T] =
    SchemaTokenDecoder.decode(input, name, decoder)
