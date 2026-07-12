package scalanotation.json

import scalanotation.internal.json.JsonDecoder

/** Controls how JSON decoder machinery is reused across calls of the [[Json.batched]] API — the
  * JSON mirror of [[scalanotation.BatchContext]]. The plain [[Json]] API is equivalent to batching
  * with [[JsonBatchContext.garbageCollected]]; the pooled contexts amortize the construction of the
  * decoder and its buffers, which dominates the fixed cost of small decodes.
  */
final class JsonBatchContext private (
    private[scalanotation] val holder: JsonDecoder.PoolHolder
)

object JsonBatchContext:
  /** Allocates a fresh decoder per call and never retains it, leaving reclamation to the GC. This
    * matches the behaviour of the plain [[Json]] API: no state is shared between calls.
    */
  val garbageCollected: JsonBatchContext = JsonBatchContext(JsonDecoder.gcContext)

  /** Reuses decoder instances through a single-threaded pool — the cheapest reuse for a batch of
    * decodes confined to one thread. Not thread-safe: do not share the returned context between
    * threads.
    */
  def local(): JsonBatchContext = JsonBatchContext(JsonDecoder.localContext())

  /** Reuses decoder instances through a lock-free, fixed-capacity pool that may be shared freely
    * between threads, including virtual threads. When more than `capacityHint` decodes run
    * concurrently, the excess allocate fresh instances that are reclaimed by the GC.
    */
  def shared(
      capacityHint: Int = Runtime.getRuntime.availableProcessors() * 4
  ): JsonBatchContext = JsonBatchContext(JsonDecoder.sharedContext(capacityHint))
