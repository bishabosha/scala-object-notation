package scalanotation.schema

import scalanotation.DecodeError
import steps.result.Result, Result.eval.{ok}
import RawSchema.{InputMap, ResultMap}
import scalanotation.Reader
import scalanotation.Writer

final case class SchemaMapping[Base, A](
    resultMap: ResultMap[Base, A] | Null = null,
    inputMap: InputMap[A, Base] | Null = null,
    totalMaps: SchemaMapping.TotalMap[Base, A] =
      SchemaMapping.TotalMap.Empty.asInstanceOf[SchemaMapping.TotalMap[Base, A]]
):

  def mapInput(value: A): Base =
    val fn = inputMap
    if fn == null then value.asInstanceOf[Base]
    else fn(value)

  // Specialized input maps — the write-side duals of [[mapResult]]'s TotalMap dispatch. When the
  // stored input map carries a Writer typed function (via the [[SchemaMapping.IntInput]] family
  // of adapters) the primitive comes back unboxed; otherwise the plain function applies and the
  // result unboxes here, exactly as the generic [[mapInput]] path would.

  def mapInputInt(value: A): Int =
    inputMap match
      case typed: SchemaMapping.IntInput[?] => typed.asInstanceOf[SchemaMapping.IntInput[A]](value)
      case null                             => value.asInstanceOf[Int]
      case fn                               => fn(value).asInstanceOf[Int]

  def mapInputLong(value: A): Long =
    inputMap match
      case typed: SchemaMapping.LongInput[?] =>
        typed.asInstanceOf[SchemaMapping.LongInput[A]](value)
      case null => value.asInstanceOf[Long]
      case fn   => fn(value).asInstanceOf[Long]

  def mapInputFloat(value: A): Float =
    inputMap match
      case typed: SchemaMapping.FloatInput[?] =>
        typed.asInstanceOf[SchemaMapping.FloatInput[A]](value)
      case null => value.asInstanceOf[Float]
      case fn   => fn(value).asInstanceOf[Float]

  def mapInputDouble(value: A): Double =
    inputMap match
      case typed: SchemaMapping.DoubleInput[?] =>
        typed.asInstanceOf[SchemaMapping.DoubleInput[A]](value)
      case null => value.asInstanceOf[Double]
      case fn   => fn(value).asInstanceOf[Double]

  /** Composes `f` before this mapping's input map, preserving a Writer typed function — the
    * write-side dual of [[withPureMap]]'s TotalMap composition.
    */
  private def composedInputMap[B](f: InputMap[B, A]): InputMap[B, Base] =
    inputMap match
      case null =>
        // no input map means Base =:= A semantically (mapInput is the identity)
        f.asInstanceOf[InputMap[B, Base]]
      case typed: SchemaMapping.IntInput[?] =>
        val fn = typed.asInstanceOf[SchemaMapping.IntInput[A]].typed
        SchemaMapping.IntInput[B](value => fn(f(value))).asInstanceOf[InputMap[B, Base]]
      case typed: SchemaMapping.LongInput[?] =>
        val fn = typed.asInstanceOf[SchemaMapping.LongInput[A]].typed
        SchemaMapping.LongInput[B](value => fn(f(value))).asInstanceOf[InputMap[B, Base]]
      case typed: SchemaMapping.FloatInput[?] =>
        val fn = typed.asInstanceOf[SchemaMapping.FloatInput[A]].typed
        SchemaMapping.FloatInput[B](value => fn(f(value))).asInstanceOf[InputMap[B, Base]]
      case typed: SchemaMapping.DoubleInput[?] =>
        val fn = typed.asInstanceOf[SchemaMapping.DoubleInput[A]].typed
        SchemaMapping.DoubleInput[B](value => fn(f(value))).asInstanceOf[InputMap[B, Base]]
      case fn =>
        value => fn(f(value))

  def mapResult(value: Base): Result[A, DecodeError] =
    totalMaps match
      case SchemaMapping.TotalMap.IntMap(fn) =>
        Result.Ok(fn(value.asInstanceOf[Int]))
      case SchemaMapping.TotalMap.LongMap(fn) =>
        Result.Ok(fn(value.asInstanceOf[Long]))
      case SchemaMapping.TotalMap.FloatMap(fn) =>
        Result.Ok(fn(value.asInstanceOf[Float]))
      case SchemaMapping.TotalMap.DoubleMap(fn) =>
        Result.Ok(fn(value.asInstanceOf[Double]))
      case SchemaMapping.TotalMap.AnyMap(fn) =>
        Result.Ok(fn(value))
      case SchemaMapping.TotalMap.Empty =>
        val fn = resultMap
        if fn == null then Result.Ok(value.asInstanceOf[A])
        else fn(value)

  def withResultMap[B](f: ResultMap[A, B]): SchemaMapping[Base, B] =
    SchemaMapping(
      resultMap = value => mapResult(value).flatMap(f)
    )

  def withInputMap[B](f: InputMap[B, A]): SchemaMapping[Base, B] =
    SchemaMapping(
      inputMap = composedInputMap(f)
    )

  def withMapped[B](
      resultMap0: ResultMap[A, B],
      inputMap0: InputMap[B, A]
  ): SchemaMapping[Base, B] =
    SchemaMapping(
      resultMap = value => mapResult(value).flatMap(resultMap0),
      inputMap = composedInputMap(inputMap0)
    )

  def withPureAndInput[B](
      resultMap0: InputMap[A, B],
      inputMap0: InputMap[B, A]
  ): SchemaMapping[Base, B] =
    withPureMap(resultMap0).copy(inputMap = composedInputMap(inputMap0))

  def withPureMap[B](f: InputMap[A, B]): SchemaMapping[Base, B] =
    if resultMap == null then
      val mappedTotal = (totalMaps match
        case SchemaMapping.TotalMap.Empty =>
          SchemaMapping.TotalMap.AnyMap((value: Base) => f(value.asInstanceOf[A]))
        case SchemaMapping.TotalMap.IntMap(fn) =>
          SchemaMapping.TotalMap.IntMap(value => f(fn(value)))
        case SchemaMapping.TotalMap.LongMap(fn) =>
          SchemaMapping.TotalMap.LongMap(value => f(fn(value)))
        case SchemaMapping.TotalMap.FloatMap(fn) =>
          SchemaMapping.TotalMap.FloatMap(value => f(fn(value)))
        case SchemaMapping.TotalMap.DoubleMap(fn) =>
          SchemaMapping.TotalMap.DoubleMap(value => f(fn(value)))
        case SchemaMapping.TotalMap.AnyMap(fn) =>
          val fn0 = fn.asInstanceOf[InputMap[Base, A]]
          SchemaMapping.TotalMap.AnyMap((value: Base) => f(fn0(value)))
      ).asInstanceOf[SchemaMapping.TotalMap[Base, B]]
      SchemaMapping(totalMaps = mappedTotal)
    else SchemaMapping(resultMap = value => resultMap(value).map(f))

  def withIntMap[B](f: Reader.IntMap[B]): SchemaMapping[Int, B] =
    SchemaMapping(totalMaps = SchemaMapping.TotalMap.IntMap(f))

  def withLongMap[B](f: Reader.LongMap[B]): SchemaMapping[Long, B] =
    SchemaMapping(totalMaps = SchemaMapping.TotalMap.LongMap(f))

  def withFloatMap[B](f: Reader.FloatMap[B]): SchemaMapping[Float, B] =
    SchemaMapping(totalMaps = SchemaMapping.TotalMap.FloatMap(f))

  def withDoubleMap[B](f: Reader.DoubleMap[B]): SchemaMapping[Double, B] =
    SchemaMapping(totalMaps = SchemaMapping.TotalMap.DoubleMap(f))

  def withTotalMap[B](f: InputMap[Base, B]): SchemaMapping[Base, B] =
    SchemaMapping(totalMaps = SchemaMapping.TotalMap.AnyMap(f))

object SchemaMapping:
  // Adapters carrying the Writer typed functions through the plain-function-typed
  // [[SchemaMapping.inputMap]] field: [[SchemaMapping.mapInput]] applies them like any input
  // function, while the specialized mapInput* dispatch calls `typed` unboxed. The write-side
  // dual of TotalMap, without changing the published mapping shape.

  private[scalanotation] final class IntInput[A](val typed: Writer.IntMap[A]) extends (A => Int):
    def apply(value: A): Int = typed(value)

  private[scalanotation] final class LongInput[A](val typed: Writer.LongMap[A]) extends (A => Long):
    def apply(value: A): Long = typed(value)

  private[scalanotation] final class FloatInput[A](val typed: Writer.FloatMap[A])
      extends (A => Float):
    def apply(value: A): Float = typed(value)

  private[scalanotation] final class DoubleInput[A](val typed: Writer.DoubleMap[A])
      extends (A => Double):
    def apply(value: A): Double = typed(value)

  enum TotalMap[Base, A]:
    case Empty                                  extends TotalMap[Any, Any]
    case IntMap[A](fn: Reader.IntMap[A])        extends TotalMap[Int, A]
    case LongMap[A](fn: Reader.LongMap[A])      extends TotalMap[Long, A]
    case FloatMap[A](fn: Reader.FloatMap[A])    extends TotalMap[Float, A]
    case DoubleMap[A](fn: Reader.DoubleMap[A])  extends TotalMap[Double, A]
    case AnyMap[Base, A](fn: InputMap[Base, A]) extends TotalMap[Base, A]

    def isEmpty: Boolean = this eq TotalMap.Empty

  def empty[A]: SchemaMapping[A, A] = SchemaMapping()

  def apply[Base, A](
      resultMap: ResultMap[Base, A] | Null,
      inputMap: InputMap[A, Base] | Null
  ): SchemaMapping[Base, A] =
    new SchemaMapping(resultMap, inputMap)
