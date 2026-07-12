package scalanotation

import scalanotation.schema.RawSchema

class PlanSlotSuite extends munit.FunSuite:

  private def freshNode: RawSchema[Vector[Int]] =
    RawSchema.Vector(RawSchema.Int, null, null)

  test("a slot caches its plan per schema node and computes once"):
    val slot     = RawSchema.PlanSlot.allocate[java.lang.StringBuilder]()
    val node     = freshNode
    var computes = 0
    def get()    = node.externalPlan(slot) { schema =>
      computes += 1
      new java.lang.StringBuilder(schema.describeSelf)
    }
    val first = get()
    assert(get() eq first)
    assert(get() eq first)
    assertEquals(computes, 1)
    // a different node computes its own plan
    val other = freshNode.externalPlan(slot)(_ => new java.lang.StringBuilder("other"))
    assert(!(other eq first))

  test("multiple decoders' slots hold independent plans on one node"):
    val slotA = RawSchema.PlanSlot.allocate[java.lang.StringBuilder]()
    val slotB = RawSchema.PlanSlot.allocate[java.lang.StringBuilder]()
    val node  = freshNode
    val planA = node.externalPlan(slotA)(_ => new java.lang.StringBuilder("A"))
    val planB = node.externalPlan(slotB)(_ => new java.lang.StringBuilder("B"))
    assertEquals(planA.toString, "A")
    assertEquals(planB.toString, "B")
    // the storage grew without disturbing earlier slots
    assert(node.externalPlan(slotA)(_ => fail("must be cached")) eq planA)
    assert(node.externalPlan(slotB)(_ => fail("must be cached")) eq planB)

  test("slots allocated later reach nodes whose storage predates them"):
    val early = RawSchema.PlanSlot.allocate[java.lang.StringBuilder]()
    val node  = freshNode
    node.externalPlan(early)(_ => new java.lang.StringBuilder("early"))
    val late     = RawSchema.PlanSlot.allocate[java.lang.StringBuilder]()
    val latePlan = node.externalPlan(late)(_ => new java.lang.StringBuilder("late"))
    assertEquals(latePlan.toString, "late")
    assertEquals(node.externalPlan(early)(_ => fail("must be cached")).toString, "early")
