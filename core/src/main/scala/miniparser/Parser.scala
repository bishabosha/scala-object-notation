package scalanotation

import scala.NamedTuple

import scala.collection.mutable

object Parser:
  def parse(input: String): SourceFile =
    val tokens = Tokenizer.tokenize(input)
    parse(tokens)

  def parse(tokens: IArray[Token]): SourceFile =
    SchemaTokenDecoder.decodeAnyRoot(tokens) match
      case Right(sourceFile) => sourceFile
      case Left(error) => throw ParseException(s"$error")

  def parseValueAs[T](input: String, name: String)(using decoder: TaggedSchema[T]): Either[DecodeError, T] =
    val tokens = Tokenizer.tokenize(input)
    decoder.decodeTokens(tokens, name)
