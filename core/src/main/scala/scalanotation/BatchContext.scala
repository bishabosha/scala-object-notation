package scalanotation

import scalanotation.internal.TokenDecoder

/** Controls how decoder machinery is reused across calls of the [[Readers.batched]] API. The plain
  * [[Readers]] API is equivalent to batching with [[BatchContext.garbageCollected]]; the pooled
  * contexts amortize the construction of the decoder, its scanner, and its buffers, which dominates
  * the fixed cost of small decodes.
  */
final class BatchContext private (
    private[scalanotation] val holder: TokenDecoder.PoolHolder
)

object BatchContext:
  /** Allocates a fresh decoder per call and never retains it, leaving reclamation to the GC. This
    * matches the behaviour of the plain [[Readers]] API: no state is shared between calls.
    */
  val garbageCollected: BatchContext = BatchContext(TokenDecoder.gcContext)

  /** Reuses decoder instances through a single-threaded pool — the cheapest reuse for a batch of
    * decodes confined to one thread. Not thread-safe: do not share the returned context between
    * threads.
    */
  def local(): BatchContext = BatchContext(TokenDecoder.localContext())

  /** Reuses decoder instances through a lock-free, fixed-capacity pool that may be shared freely
    * between threads, including virtual threads. When more than `capacityHint` decodes run
    * concurrently, the excess allocate fresh instances that are reclaimed by the GC.
    */
  def shared(
      capacityHint: Int = Runtime.getRuntime.availableProcessors() * 4
  ): BatchContext = BatchContext(TokenDecoder.sharedContext(capacityHint))
