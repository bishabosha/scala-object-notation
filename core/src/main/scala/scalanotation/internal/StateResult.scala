package scalanotation.internal

import scalanotation.DecodeError
import steps.result.Result

/** The error arm of [[StateResult]]. Package-private: no builder state can be an instance of a type
  * its author cannot reference, so the [[getOrRaise]] type test is exact. Only error paths allocate
  * one.
  */
private[internal] final class StateErrBox(val error: DecodeError)

/** A decoded builder state, or the decode error — the value dispatches' return type. Zero-cost: the
  * union keeps the success side in the return register with no wrapper and no tag, and
  * [[getOrRaise]] expands to exactly the handwritten one-test match (a scrutinee binding and
  * nothing else — deliberately a transparent alias, not an opaque type, whose prefix refinement
  * would bind module proxies in every expansion and cost stack in unoptimized builds; see the
  * decoders' recursion-depth guard). Matching the union directly is unsound against a
  * caller-abstract state type, so consume it through [[getOrRaise]] only.
  */
private[scalanotation] type StateResult[+S] = S | StateErrBox

private[scalanotation] object StateResult:
  /** the completed builder state */
  inline def apply[S](state: S): StateResult[S] = state

  /** the failed decode — the protocol's only allocation, on the error path */
  inline def fail[S](error: DecodeError): StateResult[S] = StateErrBox(error)

extension [S](result: StateResult[S])
  /** the state, or raises the error to the enclosing decode boundary */
  private[scalanotation] inline def getOrRaise(
      using scala.util.boundary.Label[Result.Err[DecodeError]]
  ): S =
    result match
      case err: StateErrBox => Result.eval.raise(err.error)
      case state            => state.asInstanceOf[S]

  /** the state, or decorates the error and raises it to the enclosing decode boundary */
  private[scalanotation] inline def getOrRaise(inline decorate: DecodeError => DecodeError)(
      using scala.util.boundary.Label[Result.Err[DecodeError]]
  ): S =
    result match
      case err: StateErrBox => Result.eval.raise(decorate(err.error))
      case state            => state.asInstanceOf[S]
