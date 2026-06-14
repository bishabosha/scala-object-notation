package scalanotation.internal

import scalanotation.BuilderSlots
import scalanotation.DecodeError
import scalanotation.internal.BuilderSlotsPool.given
import steps.result.Result

import scala.util.boundary.Label

private[scalanotation] trait TokenDecoderSupport:
  self: TokenStream =>

  protected type Resulting[+A, +E] = Label[Result.Err[E]] ?=> A

  protected def slotsPooling: Boolean

  protected final val namedTupleParseResult: NamedTupleParseResult =
    new NamedTupleParseResult()

  // pooled builder slots for product-like schemas with a slots factory; borrow/release pairs nest,
  // so a reentrant decode inside a field value borrows a fresh instance. Lazy: a one-shot decoder
  // skips the slots path and must not pay for the pool either.
  private lazy val slotsPool = Internal.LocalPool[BuilderSlots]()

  protected final def expectedTypeAtCurrent(schema: RawSchema): DecodeError =
    DecodeError.ExpectedType(schema.describeSelf, describeCurrent()).atToken(currentSpan())

  protected final def missingReadCapability(schema: RawSchema): Nothing =
    throw IllegalStateException(
      s"read is not available for schema ${schema.describeSelf}"
    )

  protected inline def withRead[T, S <: RawSchema, R](
      schema: S,
      inline r: S => R | Null
  )(inline f: R => T): T =
    val read = r(schema)
    if read == null then missingReadCapability(schema)
    else f(read.nn)

  protected inline def withBorrowSlots[T](
      factory: scalanotation.TypedFactory.OfProduct[?] | Null
  )(inline f: (BuilderSlots | Null) => T): T =
    def useSlots(slots: BuilderSlots | Null): T =
      f(slots)
    if !slotsPooling || factory == null then useSlots(null)
    else slotsPool.withBorrowed(useSlots)
