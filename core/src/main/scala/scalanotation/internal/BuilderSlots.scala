package scalanotation.internal

import scalanotation.BuilderSlots

/** Integer tags for the typed value slots of the push-model decoders: a decode step records the
  * slot it pushed into, so the consumer can pull from the matching typed slot without boxing.
  * [[Ref]] marks the Any slot used for composite and reference results.
  */
private[scalanotation] object SlotKind:
  inline val Ref     = 0
  inline val String  = 1
  inline val Char    = 2
  inline val Int     = 3
  inline val Long    = 4
  inline val Float   = 5
  inline val Double  = 6
  inline val Boolean = 7

private[scalanotation] object BuilderSlotsPool:
  given Internal.Alloc[BuilderSlots]:
    def alloc(): BuilderSlots            = new BuilderSlots
    def prepare(t: BuilderSlots): t.type = t // re-aimed by reset(size) after borrowing
