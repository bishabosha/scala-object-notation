package scalanotation

import scala.NamedTuple
import steps.result.Result
import steps.result.Result.eval.{ok, raise, break}

import scala.collection.mutable

object Readers:
  object quick:
    def read(input: String, debugTokens: Boolean = false): SourceFile[Expr] =
      // TODO: add an okOrElse method that can recover the error somehow or return the value.
      readAs[Expr](input, debugTokens) match
        case Result.Ok(value) => value
        case Result.Err(err)  => throw IllegalArgumentException(err.format)

  def readAs[T: TaggedSchema as decoder](
      input: String,
      debugTokens: Boolean = false
  ): Result[SourceFile[T], DecodeError] =
    Result:
      val tokens = Tokenizer.tokenize(input, debugTokens).ok
      break(TokenDecoder.decodeAnyRoot(tokens, decoder))

  def readValueAs[T: TaggedSchema as decoder](
      input: String,
      name: String,
      debugTokens: Boolean = false
  ): Result[T, DecodeError] =
    Result:
      val tokens = Tokenizer.tokenize(input, debugTokens).ok
      break(TokenDecoder.decode(tokens, name, decoder))
