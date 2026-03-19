package scalanotation

import scala.NamedTuple
import steps.result.Result
import steps.result.Result.eval.{ok, raise, break}

import scala.collection.mutable

object Readers:
  object quick:
    def readDecls(input: String, debugTokens: Boolean = false): SourceFile[Expr] =
      // TODO: add an okOrElse method that can recover the error somehow or return the value.
      readDeclsAs[Expr](input, debugTokens) match
        case Result.Ok(value) => value
        case Result.Err(err)  => throw IllegalArgumentException(err.format)

    def read(input: String, debugTokens: Boolean = false): Expr =
      // TODO: add an okOrElse method that can recover the error somehow or return the value.
      readAs[Expr](input, debugTokens) match
        case Result.Ok(value) => value
        case Result.Err(err)  => throw IllegalArgumentException(err.format)

  def readAs[T: TaggedSchema as decoder](
      input: String,
      debugTokens: Boolean = false
  ): Result[T, DecodeError] =
    Result:
      val tokens = Tokenizer.tokenize(input, debugTokens).ok
      break(TokenDecoder.decodeExpression(tokens, decoder))

  def readDeclsAs[T: TaggedSchema as decoder](
      input: String,
      debugTokens: Boolean = false
  ): Result[SourceFile[T], DecodeError] =
    Result:
      val tokens = Tokenizer.tokenize(input, debugTokens).ok
      break(TokenDecoder.decodeAnyRoot(tokens, decoder))

  def readDeclAs[T: TaggedSchema as decoder](
      input: String,
      rootName: String,
      debugTokens: Boolean = false
  ): Result[T, DecodeError] =
    Result:
      val tokens = Tokenizer.tokenize(input, debugTokens).ok
      break(TokenDecoder.decode(tokens, rootName, decoder))
