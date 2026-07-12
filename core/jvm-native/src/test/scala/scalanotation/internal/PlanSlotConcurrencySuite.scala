package scalanotation.internal

import scalanotation.schema.RawSchema

class PlanSlotConcurrencySuite extends munit.FunSuite:

  test("racing first uses agree on one cached plan per slot"):
    val slots =
      Vector.fill(4)(RawSchema.PlanSlot.allocate[java.util.concurrent.atomic.AtomicInteger]())
    val node = RawSchema.Vector[Vector[Int], Int](RawSchema.Int, null, null)

    val threads = 8
    val gate    = new java.util.concurrent.CountDownLatch(1)
    val results =
      java.util.concurrent.ConcurrentHashMap.newKeySet[
        (Int, java.util.concurrent.atomic.AtomicInteger)
      ]()
    val workers = (1 to threads).map { _ =>
      val thread = new Thread(() =>
        gate.await()
        var round = 0
        while round < 1000 do
          var slotIndex = 0
          while slotIndex < slots.length do
            val plan = node.externalPlan(slots(slotIndex))(_ =>
              new java.util.concurrent.atomic.AtomicInteger(slotIndex)
            )
            results.add((slotIndex, plan))
            slotIndex += 1
          round += 1
      )
      thread.start()
      thread
    }
    gate.countDown()
    workers.foreach(_.join())

    // every thread saw exactly one plan instance per slot, carrying that slot's value
    assertEquals(results.size, slots.length)
    results.forEach((slotIndex, plan) => assertEquals(plan.get(), slotIndex))
