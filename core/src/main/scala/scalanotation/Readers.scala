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
    TokenDecoder.decodeExpression(input, debugTokens, reader)(
      using BatchContext.garbageCollected.holder
    )

  def readDeclsAs[T: Reader as reader](
      input: String,
      debugTokens: Boolean = false,
      packageName: String = ""
  ): Result[Expr.SourceFile[T], DecodeError] =
    TokenDecoder.decodeAnyRoot(input, debugTokens, packageName, reader)(
      using BatchContext.garbageCollected.holder
    )

  def readDeclAs[T: Reader as reader](
      input: String,
      rootName: String,
      debugTokens: Boolean = false,
      packageName: String = ""
  ): Result[T, DecodeError] =
    TokenDecoder.decode(input, debugTokens, rootName, packageName, reader)(
      using BatchContext.garbageCollected.holder
    )

  /** Variants of the read methods that reuse decoder machinery across calls, as configured by the
    * given [[BatchContext]] — see its factories for the pooling options. Results are identical to
    * the plain methods; only the allocation behaviour differs.
    */
  object batched:
    def readAs[T](
        input: String,
        debugTokens: Boolean = false
    )(using reader: Reader[T], ctx: BatchContext): Result[T, DecodeError] =
      given TokenDecoder.PoolHolder = ctx.holder
      TokenDecoder.decodeExpression(input, debugTokens, reader)

    def readDeclsAs[T](
        input: String,
        debugTokens: Boolean = false,
        packageName: String = ""
    )(using reader: Reader[T], ctx: BatchContext): Result[Expr.SourceFile[T], DecodeError] =
      given TokenDecoder.PoolHolder = ctx.holder
      TokenDecoder.decodeAnyRoot(input, debugTokens, packageName, reader)

    def readDeclAs[T](
        input: String,
        rootName: String,
        debugTokens: Boolean = false,
        packageName: String = ""
    )(using reader: Reader[T], ctx: BatchContext): Result[T, DecodeError] =
      given TokenDecoder.PoolHolder = ctx.holder
      TokenDecoder.decode(input, debugTokens, rootName, packageName, reader)
