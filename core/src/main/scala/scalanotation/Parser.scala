package scalanotation

import scala.NamedTuple
import steps.result.Result
import steps.result.Result.eval.{ok, raise, break}

import scala.collection.mutable

object Parser:
  object quick:
    def parse(input: String): SourceFile[Expr] =
      // TODO: add an okOrElse method that can recover the error somehow or return the value.
      parseAs[Expr](input) match
        case Result.Ok(value) => value
        case Result.Err(err) => throw IllegalArgumentException(err.format)

  def parseAs[T: TaggedSchema as decoder](input: String): Result[SourceFile[T], DecodeError] =
    Result:
      val tokens = Tokenizer.tokenize(input).ok
      break(parseAs(tokens))

  def parseAs[T: TaggedSchema as decoder](input: IArray[Token]): Result[SourceFile[T], DecodeError] =
    SchemaTokenDecoder.decodeAnyRoot(input, decoder)


  def parseValueAs[T: TaggedSchema as decoder](input: String, name: String): Result[T, DecodeError] =
    Result:
      val tokens = Tokenizer.tokenize(input).ok
      break(parseValueAs(tokens, name))

  def parseValueAs[T: TaggedSchema as decoder](input: IArray[Token], name: String): Result[T, DecodeError] =
    SchemaTokenDecoder.decode(input, name, decoder)

