# Implicit search spawned during inline expansion does compounding redundant work (×16–×500 vs the identical search at a call site)

Investigation notes for scala-object-notation issue #77, written up as an upstream-ready
report. **Not filed anywhere** — kept here for reference; file manually if/when desired.

## Summary

The same implicit search costs wildly different amounts depending only on *where it is
initiated*: ~1,000 search "expressions" as a plain `summon` or call-site `using` parameter,
~16,000–64,000 via `compiletime.summonInline` inside an inline def, and >256,000 when
initiated from `summonInline` inside a typeclass-derivation `inline def` whose target types
mention mirror-dependent types (`m.MirroredElemLabels`). The cost compounds exponentially
with the nesting depth of the resolved structure. Once it crosses the default
`-Ximplicit-search-limit` (50,000), the user gets a misleading "no implicit values were
found" error — and through Bloop/scala-cli, not even the E168 hint that a limit was hit.

## Compiler versions

Reproduced identically on 3.8.3 and 3.9.0-RC4 (JVM 21).

## Reproduction

Self-contained minimization resisted a time-boxed attempt — the blowup needs the interaction
of an opaque-type given machinery, named-tuple types, and `summonInline` (a mini replica
with the same shape — opaque `At[T]`, priority traits, structural cons givens, a
trivially-succeeding inline fallback with deferred `summonFrom`, decoy leaf givens, growing
type-level path strings — stays at ~500 expressions). So the repro pins a published
artifact:

```scala
//> using scala 3.9.0-RC4
//> using dep io.github.bishabosha::scala-object-notation:0.4.4

import scala.collection.immutable.SeqMap
import scalanotation.Reader

type Schema = (
    invoice: (id: Int, period: (start: String, days: Int)),
    client: (id: Int, name: String, address: String, contactPerson: Option[String]),
    listings: (
        items: Vector[(desc: String, body: Option[String], qty: Double, price: Int)],
        taxRate: Int,
        useHours: Boolean
    ),
    business: String,
    copyright: Option[String],
    currency: (code: String, symbol: String, left: Boolean),
    bank: SeqMap[String, String],
    appendices: Vector[
      (title: String, description: String,
       sections: Vector[(title: String, desc: String, itemsTitle: String, items: SeqMap[String, String])])
    ]
)

val ok: Reader[Schema]    = summon[Reader[Schema]]  // compiles, needs -Ximplicit-search-limit:1000
val fails: Reader[Schema] = Reader.derived          // "No given instance ... found" + E168 warning
```

`Reader.derived` (library version 0.4.4) is a plain `inline def` that dispatches on
`Mirror.Of[T]` and does
`compiletime.summonInline[ProductFieldsAtPath["", m.MirroredElemLabels, m.MirroredElemTypes]]`.
That opaque-type machinery recursively resolves one given per field, bottoming out in a
trivially-succeeding low-priority `inline given` fallback whose `summonFrom` body defers
leaf lookups to expansion time.

## Measurements

Method: minimum `-Ximplicit-search-limit` at which compilation succeeds, probed with direct
`scalac` (`cs launch scalac:3.9.0-RC4 -- -classpath ... -Ximplicit-search-limit:N file.scala`),
judged by exit code. Ladder: 1000, 2000, 4000, … 256000.

Identical resolution work, varying only the initiation site (all against the published
0.4.4 machinery):

| entry point | min limit |
|---|---|
| `summon[Reader[Schema]]` (implicit instance) | ~1,000 |
| `summon[Reader.Builders.ProductFieldsAtPath["", Labels, Values]]` — the derivation spine as a plain summon | ~1,000 |
| `Reader.ofFields[S]` — inline def, spine resolved as a call-site `using` parameter | ~1,000 |
| `summonInline[Reader.Builders.AtPath["", T]]` inside a *user-written* inline def (2-layer nested T) | ~16,000 |
| `summonInline[Reader.Builders.ProductFieldsAtPath[...]]` inside a user-written inline def, same T | ~64,000 |
| `Reader.ofFields[T](using m)` called from inside a user inline def (target types mention `m.MirroredElemLabels`) | >256,000 |
| `Reader.derived[Schema]` (library `summonInline`, mirror-dependent types) | between 256,000 and 1,000,000 |

Scaling with structure, all via `Reader.derived`:

| schema | min limit |
|---|---|
| `(a: Vector[(t: String, d: String)])` | 2,000 |
| `(a: Vector[(t: String, d: String, i: SeqMap[String, String])])` | 8,000 |
| `(a: Vector[(t: String, d: String, s: Vector[(x: String, y: String)])])` | 256,000 |
| one more nested `Vector`-of-record layer | >1,000,000 |

The identical types via plain `summon` stay at ~1,000 throughout. So the per-node overhead
of an in-expansion search *multiplies* per nesting level (~×128 per added Vector-of-record
layer) instead of adding.

## Library-side observations that may help localize the cost

Found while fixing this downstream (the fix shipped in scala-object-notation makes
`derived` cost ~2,000 on the full schema above):

1. **Losing candidates appear to be fully re-explored per node under the inliner.**
   Removing one overlapping-but-never-selected structural candidate (a `Seq` given that
   unifies with `Vector` targets but always loses the specificity comparison to the
   `Vector` given) cut the total cost ×8. Under a plain summon, the same candidate costs
   nothing measurable — it is pruned by comparison without a full trial.

2. **A trivially-succeeding `inline given` is poisonous as a mere candidate.** The
   machinery's fallback given (`inline given DefaultAtPath: [Path <: String, T] => AtPath[Path, T]`,
   no parameters, `summonFrom` body) always loses the priority ranking to the structural
   givens, yet its *eligibility* at every node was the dominant amplifier: replacing it with
   a non-inline parameterized given restored ~90 % of the loss, and even *adding it back*
   alongside the fixed structure (still never selected) re-triggered the full explosion.
   This suggests inline-given candidate bodies (or their deferred `summonFrom`s) are
   re-elaborated once per enclosing candidate trial when the root search comes from the
   Inliner, compounding with depth.

3. **Mirror-proxy-dependent types add a further multiplier.** The same spine summon costs
   ~64,000 with concrete tuple type arguments and >256,000 when the arguments are spelled
   `m.MirroredElemLabels` / `m.MirroredElemTypes` through an inline proxy.

## Diagnostics issue

When the budget is exhausted, direct scalac emits the E168 warning
("Implicit search problem too large. an implicit search was terminated with failure after
trying 50000 expressions … You can change the behavior by setting the
`-Ximplicit-search-limit` value") *next to* the "No given instance … was found" error.
Through Bloop / scala-cli, only the error surfaces — the warning is dropped — so users see
a plain missing-instance failure with a partial candidate trace and no pointer to the
limit. The original downstream report burned its debugging time on `-Xmax-inlines` for
exactly this reason. If a search dies on the budget, the resulting *error* should carry the
hint, not a separate warning.

A secondary confusion: the error's "I found: … But no implicit values were found that match
type …" framing names an arbitrary frontier of the aborted search, which reads as a
missing-instance bug in whatever type happens to be at that frontier.

## Expectation

A search initiated from inline expansion should cost the same as the identical search
initiated by the typer at a call site — or at least degrade linearly, not exponentially in
the depth of the resolved structure.
