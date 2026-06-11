package scalanotation

import scala.NamedTuple
import steps.result.Result

import scalanotation.internal.TokenDecoder

object Readers:
  object quick:
    def readDecls(
        input: String,
        debugTokens: Boolean = false,
        packageName: String = ""
    ): Expr.SourceFile[Expr] =
      // TODO: add an okOrElse method that can recover the error somehow or return the value.
      readDeclsAs[Expr](input, debugTokens, packageName) match
        case Result.Ok(value) => value
        case Result.Err(err)  => throw IllegalArgumentException(err.format)

    def read(input: String, debugTokens: Boolean = false): Expr =
      // TODO: add an okOrElse method that can recover the error somehow or return the value.
      readAs[Expr](input, debugTokens) match
        case Result.Ok(value) => value
        case Result.Err(err)  => throw IllegalArgumentException(err.format)

  def readAs[T: Reader as reader](
      input: String,
      debugTokens: Boolean = false
  ): Result[T, DecodeError] =
    TokenDecoder.decodeExpression(input, debugTokens, reader)

  def readDeclsAs[T: Reader as reader](
      input: String,
      debugTokens: Boolean = false,
      packageName: String = ""
  ): Result[Expr.SourceFile[T], DecodeError] =
    TokenDecoder.decodeAnyRoot(input, debugTokens, packageName, reader)

  def readDeclAs[T: Reader as reader](
      input: String,
      rootName: String,
      debugTokens: Boolean = false,
      packageName: String = ""
  ): Result[T, DecodeError] =
    TokenDecoder.decode(input, debugTokens, rootName, packageName, reader)
