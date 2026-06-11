package scalanotation.internal

import scalanotation.BatchContext
import scalanotation.Reader
import scalanotation.Readers
import scalanotation.ScalanotationSuite

import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger

/** Exercises the shared decoder pool from as many threads as the pool has slots, so steady-state
  * borrows are satisfied from the pool (no per-decode allocation churn) and every instance is
  * exercised concurrently.
  */
class ConcurrentDecodingSuite extends ScalanotationSuite:

  private def runOnThreads(threads: Int)(task: Int => Unit): Unit =
    val executor = Executors.newFixedThreadPool(threads)
    try
      val start   = new CountDownLatch(1)
      val futures = (0 until threads).map { worker =>
        executor.submit[Unit] { () =>
          start.await()
          task(worker)
        }
      }
      start.countDown()
      futures.foreach { future =>
        try future.get(60, TimeUnit.SECONDS)
        catch case e: java.util.concurrent.ExecutionException => throw e.getCause
      }
    finally executor.shutdownNow()

  test("shared pool never double-leases an instance and reuses instead of allocating"):
    final class Probe:
      val leased = new AtomicBoolean(false)

    val allocations = new AtomicInteger(0)
    given Internal.Alloc[Probe]:
      def alloc(): Probe =
        allocations.incrementAndGet()
        new Probe
      def prepare(t: Probe): t.type = t

    val pool       = Internal.SharedPool[Probe](capacityHint = 8)
    val iterations = 2000

    runOnThreads(pool.capacity) { _ =>
      var i = 0
      while i < iterations do
        pool.withBorrowed { probe =>
          assert(probe.leased.compareAndSet(false, true), "instance leased to two owners at once")
          probe.leased.set(false)
        }
        i += 1
    }

    // with one thread per slot, steady-state borrows hit the pool: allocation stays bounded by
    // the capacity (plus rare borrow/release races), instead of growing with iteration count
    assert(
      allocations.get() <= pool.capacity * 2,
      s"expected bounded allocations, got ${allocations.get()} for capacity ${pool.capacity}"
    )

  test("parallel batched decodes through a shared context are isolated"):
    type Data = (x: Int, y: Int, label: String)
    given Reader[Data] = summon[Reader[Data]]
    val iterations     = 500

    // one worker thread per pool slot, so steady-state borrows reuse pooled decoders instead of
    // allocating per decode
    val capacity            = 8
    given ctx: BatchContext = BatchContext.shared(capacityHint = capacity)

    runOnThreads(capacity) { worker =>
      var i = 0
      while i < iterations do
        val x      = worker * iterations + i
        val input  = s"""(x = $x, y = ${-x}, label = "item$x")"""
        val parsed = Readers.batched.readAs[Data](input)
        val data = parsed.getOrElse(fail(s"worker $worker: expected successful parse, got $parsed"))
        assertEquals(data, (x = x, y = -x, label = s"item$x"))
        i += 1
    }
