# Scala Object Notation

Scala Object Notation is a small data-oriented subset of Scala syntax that can decode directly into
typed Scala values.

## Example: Configuration

Scala Object Notation supports two input modes:
- a single top-level `val` declaration, optionally preceded by a single package statement
- a top-level expression

```scala
package example.config

/** Comments are supported! */
val conf = (
  app = (
    host = "127.0.0.1",
    port = 8080,
    // mode = "prod",
    mode = "dev",
    replicas = Vector(
      (region = "eu-central", weight = 2),
      (region = "us-east", weight = 1)
    )
  ),
  schedule = (
    start = "2021-12-15",
    refreshSeconds = null
  ),
  features = Vector("metrics", "tracing"),
)
```

Why Scala syntax?

- data files can be compiled and introspected as part of a program or decoded by external programs
- structural data can be interpreted via a schema to richer types (via `Reader` and `Writer` type classes) or used as is.
- config can be edited programmatically and written back
- schema checking errors index to the position in the source file
- named tuples have strict ordering and non-duplication requirements.

There are no methods (yet?) only pure data.

## Supported Syntax

The supported syntax is deliberately small:

- an optional single `package foo.bar` statement before a declaration
- one top-level `val` declaration
- named tuples for structured objects: `(name = value, other = value)`
- tuples: `EmptyTuple`, `Tuple(value)` for exactly one element, and `(a, b, ...)` for two or more elements
- `Vector(...)` for sequences
- Scala literal values
- string concatenation

Plain parenthesized grouping is not supported. `(value)` is rejected; use `Tuple(value)` for a
singleton tuple.

## Quick Start: Direct Structural Decoding

If your config structure already matches Scala data closely, you can decode directly into a named
tuple type. This is the most direct config workflow: parse the file and get back a nested typed
structure without defining intermediate case classes.

```scala
import scalanotation.*
import steps.result.Result

val input =
  """package example.config
    |val conf = (
    |  app = (
    |    host = "127.0.0.1",
    |    port = 8080,
    |    mode = "dev",
    |    replicas = Vector(
    |      (region = "eu-central", weight = 2),
    |      (region = "us-east", weight = 1)
    |    )
    |  ),
    |  schedule = (
    |    start = "2021-12-15",
    |    refreshSeconds = null
    |  ),
    |  features = Vector("metrics", "tracing"),
    |)
    |""".stripMargin

type Config =
  (
    app: (
      host: String,
      port: Int,
      mode: String,
      replicas: Vector[(region: String, weight: Int)]
    ),
    schedule: (
      start: String,
      refreshSeconds: Option[Int]
    ),
    features: Vector[String],
  )

val decoded: Result[Config, DecodeError] =
  Readers.readDeclAs[Config](input, rootName = "conf", packageName = "example.config")
```

If you don't require a package statement, then omit the `packageName` argument (its default value is `""`).

Package statements are always rejected by top-level expression readers such as `readAs` and `quick.read`.

This direct structural decoding is especially useful when your config is already naturally
tree-shaped and you want Scala’s nested named tuple types to mirror the file exactly.

## Batched Decoding

The plain `Readers` methods allocate a fresh decoder for each call. That is a good default for
one-off reads because no mutable decode state is retained after the call returns.

When reading many values with the same process, use `Readers.batched` with a `BatchContext` to
reuse decoder machinery across calls. The result values and errors are identical to the plain API;
only allocation behaviour changes.

```scala
import scalanotation.*

val inputs = IArray(
  """(region = "eu-central", weight = 2)""",
  """(region = "us-east", weight = 1)"""
)

given BatchContext = BatchContext.local()

val decoded = inputs.map: input =>
  Readers.batched.readAs[(region: String, weight: Int)](input)
```

`BatchContext.local()` is the cheapest pooled context for a batch confined to one thread. Do not
share it between threads.

For concurrent batches, use a shared context:

```scala
given BatchContext = BatchContext.shared()
```

`BatchContext.shared(capacityHint)` uses a fixed-capacity lock-free pool. When more decodes run
concurrently than the pool can hold, the excess decodes allocate temporary decoder instances and
leave them to the garbage collector.

For completeness, `BatchContext.garbageCollected` is the no-pooling context. The plain `Readers`
API is equivalent to using it.

## Experimental Features

Experimental syntax lives behind `Readers.experimental`, so the normal readers stay predictable.
Put any supported experimental imports at the top of the input: before an expression, or after the
optional package statement and before declarations.

### SIP-72

SIP-72 adds dedented multiline string literals. See the
[proposal](https://github.com/scala/improvement-proposals/pull/112) for the full design context.
To use them, opt in with:

```scala
import language.experimental.dedentedStringLiterals
```

After that import, string values can use `'''` delimiters:

```scala
import scalanotation.*
import steps.result.Result

val input =
  """import language.experimental.dedentedStringLiterals
    |'''
    |host = "127.0.0.1"
    |port = 8080
    |'''
    |""".stripMargin

val decoded = Readers.experimental.readAs[String](input)
assert(decoded == Result.Ok("host = \"127.0.0.1\"\nport = 8080"))
```

The opening `'''` delimiter must be followed by a newline, and the
closing delimiter must sit on its own line with only indentation before it. That closing indentation
is removed from each content line, and line endings inside the literal are normalized to `\n`.

For declaration inputs, keep the same order you would use in Scala source: package first, then the
experimental import, then the declaration:

```scala
package example.config

import language.experimental.dedentedStringLiterals

val conf = (
  message = '''
    hello
    world
    '''
)
```

### Collection Literals

Collection literals add `[...]` as an experimental shorthand for `Vector(...)`.
To use them, opt in with:

```scala
import language.experimental.collectionLiterals
```

After that import, sequence values can use bracket syntax:

```scala
import scalanotation.*
import steps.result.Result

val input =
  """import language.experimental.collectionLiterals
    |[1, 2, 3]
    |""".stripMargin

val decoded = Readers.experimental.readAs[Vector[Int]](input)
assert(decoded == Result.Ok(Vector(1, 2, 3)))
```

The `->` pair operator can be used inside collection literals for pair-sequence schemas:

```scala
import language.experimental.collectionLiterals

["abc" -> 1, "def" -> 2]
```

Chained pairs associate left-to-right, matching Scala operator syntax:
`1 -> "abc" -> true` decodes as `((1, "abc"), true)`.

## Dynamic Data

`scalanotation.Expr` is an algebraic data type representing the syntax of Scala Object Notation:
```scala
enum Expr:
  case NamedTupleExpr(elements: IndexedSeq[(name: String, value: Expr)])
  case TupleExpr(elements: IndexedSeq[Expr])
  case VectorExpr(elements: IndexedSeq[Expr])
  case StringConstant(value: String)
  case CharConstant(value: Char)
  case IntConstant(value: Int)
  case LongConstant(value: Long)
  case FloatConstant(value: Float)
  case DoubleConstant(value: Double)
  case BooleanConstant(value: Boolean)
  case NullConstant
```

it can also be directly decoded to from text:
```scala
import scalanotation.*
import scalanotation.Expr.*
import steps.result.Result

val decoded = Readers.readAs[Expr]("(ok = true, retries = 3)")
assert(decoded == Result.Ok(NamedTupleExpr(IndexedSeq("ok" -> BooleanConstant(true), "retries" -> IntConstant(3)))))

val tuple = Readers.readAs[Expr]("""(1, "two", Vector(3))""")
assert(tuple == Result.Ok(TupleExpr(IndexedSeq(IntConstant(1), StringConstant("two"), VectorExpr(IndexedSeq(IntConstant(3)))))))

val singleton = Readers.readAs[Expr]("Tuple(1)")
assert(singleton == Result.Ok(TupleExpr(IndexedSeq(IntConstant(1)))))
```

## Moving From Structural Config To Domain Types

Traditional application config parsing in Scala decodes to domain types rather than raw data,
which is supported by Scala Object Notation.

The library provides the `ReadWriter[T]` type class which declares a schema for the shape of data.
supporting both reading and writing. `Reader[T]` and `Writer[T]` also exist to restrict capabilities
to one way.

Both type classes can be derived automatically, or allow you to transform an existing schema.

```scala
import scalanotation.*
import steps.result.Result

import java.time.LocalDate

enum Mode:
  case Dev, Prod

// map the existing `ReadWriter[String]`:
given ReadWriter[Mode] =
  summon[ReadWriter[String]].bimapResult {
    case "dev"  => Result.Ok(Mode.Dev)
    case "prod" => Result.Ok(Mode.Prod)
    case other  => Result.Err(DecodeError.Custom(s"Unknown mode '$other'"))
  } {
    case Mode.Dev  => "dev"
    case Mode.Prod => "prod"
  }

// map the existing `ReadWriter[String]`:
given ReadWriter[LocalDate] =
  summon[ReadWriter[String]].bimapResult { raw =>
    Result.catchException({ case _: java.time.format.DateTimeParseException =>
      DecodeError.Custom(s"Invalid ISO date '$raw'")
    }) {
      LocalDate.parse(raw)
    }
  }(_.toString)

// semi-automatic derivation
case class Replica(region: String, weight: Int) derives ReadWriter
case class App(host: String, port: Int, mode: Mode, replicas: Vector[Replica]) derives ReadWriter
case class Schedule(start: LocalDate, refreshSeconds: Option[Int]) derives ReadWriter
case class Config(app: App, schedule: Schedule, features: Vector[String]) derives ReadWriter

// decode the same input as before to a richer type
val decoded = Readers.readDeclAs[Config](input, rootName = "conf", packageName = "example.config")
```

That lets you keep the text format simple while still decoding into domain-specific Scala types.

## Encoding values as data

The same typed values can be written back out as Scala Object Notation text, or to an `Expr`.

```scala
import scalanotation.*

val value =
  (
    app = (host = "127.0.0.1", port = 8080, mode = "dev"),
    features = Vector("metrics", "tracing"),
    refreshSeconds = Option.empty[Int]
  )

val expr: Expr = Writers.writeExpr(value) // write to dynamic data
val text: String = Writers.write(value) // write to expression
val declText: String = Writers.writeDecl("conf", value) // write to declaration of name "conf"
val prettyDecl: String = Writers.writeDeclPretty("conf", value, indent = 2)
```

`Expr` values can also be rendered directly:

```scala
import scalanotation.*

val expr = Writers.writeExpr((ok = true, retries = 3))

val compact = expr.render
val pretty = expr.renderPretty(indent = 2)
val alsoPretty = expr.render(TextFormat.pretty(indent = 2))
```

## Derivation

You can derive the type classes for product and sum types:

- `derives Reader` for read-only use
- `derives Writer` for write-only use
- `derives ReadWriter` for both directions

`ReadWriter` is usually the best fit for config models because it keeps both directions aligned.

### Case Classes

Case classes derive structurally from their fields:

```scala
import scalanotation.*

case class Database(host: String, port: Int) derives ReadWriter
case class Config(database: Database, debug: Boolean) derives ReadWriter
```

Derived case class readers and read-writers keep fields ordered and required by default. To allow
nullable `Option` fields to be skipped while decoding, opt in with the skippable namespace:

```scala
case class Config(host: String, port: Option[Int])

given Reader[Config] = Reader.skippable.derived
```

Use `ReadWriter.skippable.derived` for bidirectional schemas.

That corresponds to config shaped like:

```scala
val conf = (
  database = (
    host = "localhost",
    port = 5432
  ),
  debug = true
)
```

### Enums

Enums derive as a single-field object whose field name is the case label.

```scala
import scalanotation.*

enum Mode derives ReadWriter:
  case Fast
  case Scheduled(at: String, retries: Int)
```

The encoded form is:

```scala
// Fast
(Fast = null)

// Scheduled(at = "2026-03-15", retries = 2)
(Scheduled = (at = "2026-03-15", retries = 2))
```

Case objects and empty products follow the same nullary representation:

```scala
// case object Foo
(Foo = null)
```

### Configured Derivation

Use `Reader.configured.derived`, `Writer.configured.derived`, or
`ReadWriter.configured.derived` when a type should use an explicit `Configured[T]`.

Currently, configuration supports:

- discriminator-field encoding for sum types
- skippable decoding for product fields
- default values for omitted fields
- opt-in typed factories for lower-boxing product construction

Discriminator-field encoding flattens the selected enum case into the surrounding named tuple.
The discriminator field is written first and is not preserved in the decoded value:

```scala
import scalanotation.*

enum Mode:
  case Fast
  case Scheduled(at: String, retries: Int)

given Configured[Mode] =
  Configured.discriminator("type")

given ReadWriter[Mode] =
  ReadWriter.configured.derived
```

The encoded form is:

```scala
// Fast
(`type` = "Fast")

// Scheduled(at = "2026-03-15", retries = 2)
(`type` = "Scheduled", at = "2026-03-15", retries = 2)
```

`Configured.discriminator[T]` is only available for sum types.

Configured derivation can also be skippable:

```scala
case class User(name: String, nickname: Option[String])

given Configured[User] =
  Configured.skippable

given Reader[User] =
  Reader.configured.derived
```

That reader accepts:

```scala
(name = "Ada")
```

Products whose fields are all `Option`s are also supported. When every field is skipped the record
is empty, and `()` is the Unit literal — never a record — so the empty record is spelled the way
Scala spells it:

```scala
NamedTuple.Empty
```

Discriminator sum types compose with skippable decoding, so product cases may contain only
optional fields:

```scala
enum Event:
  case Ping(id: Option[Int], label: Option[String])

given Configured[Event] =
  Configured.discriminator("type", skippable = true)

given Reader[Event] =
  Reader.configured.derived
```

This accepts either:

```scala
(`type` = "Ping")
(`type` = "Ping", id = 1)
```

### Default Values

Fields omitted from the input can decode to default values instead of failing. This is a mode
switch with skippable options: a configuration is either defaults-filling or skippable, never
both.

Constructor default parameters are gathered by `Defaults.derived` from the `scalanotation.macros`
package, for case classes and for the structured cases of an enum:

```scala
import scalanotation.*
import scalanotation.macros.Defaults

case class Server(host: String, port: Int = 8080, secure: Boolean = false)

given DefaultValues[Server] = Defaults.derived
given Configured[Server]    = Configured.default.withDefaultValues
given Reader[Server]        = Reader.configured.derived
```

That reader accepts any of:

```scala
(host = "a")
(host = "a", secure = true)
(host = "a", port = 9000, secure = true)
```

Omitted fields fill with their defaults in place, so provided fields may skip over defaulted ones.
Defaults that depend on other constructor parameters cannot be gathered and are treated as absent.
A record whose fields all have defaults can be spelled with every field omitted as
`NamedTuple.Empty`.

Defaults can also be assembled manually with `DefaultValues.of`, without a macro. A typed
lens-like path selects a field anywhere in the nested structure — through records, `Option`s
(`.some`) and `Vector`s (`.each`) — and `:=` binds the default installed for it:

```scala
import scalanotation.*

case class Worker(id: Int, retries: Int)
case class Db(host: String, port: Int)
case class Config(name: String, db: Option[Db], workers: Vector[Worker])

given Reader[Db]     = Reader.derived
given Reader[Worker] = Reader.derived

given DefaultValues[Config] = DefaultValues.of[Config] { c =>
  Seq(
    c.name := "app",
    c.db.some.port := 5432,
    c.workers.each.retries := 3
  )
}
given Configured[Config] = Configured.default.withDefaultValues
given Reader[Config]     = Reader.configured.derived
```

Field selections are compiler-typed (only real fields with their real types are selectable), and
`:=` only typechecks on a path that ends at a field — not after `.some`/`.each` or at the root.
Paths naming a missing field are rejected when the reader is built.

`.some` and `.each` step through any type *represented* by an `Option` or `Vector` schema. The
library provides witnesses mirroring its readers (`Option`; `Vector`, `Seq` subtypes, `IArray`,
`Array`); a custom mapped type can supply its own `DefaultValues.OptionRepr` / `VectorRepr`
witness to become steppable.

Both sources share one representation — a sequence of paths to bound values — and both compose
with typed factories, e.g. `Configured.typed.withDefaultValues`.

### Opt-in Typed Derivation

Decoding in batched mode already shares reusable intermediate buffers with typed slots for primitive
values, however by default derived Reader instances use `scala.deriving.Mirror.Product's`
`fromProduct` method, which passes all values through the `productElement(index: Int): Any` method.

Without inlining and escape analysis, this will box primitive values, so an alternative
`TypedFactory[T]` typeclass exists that gives unboxed accessors to builder state.
This can give a measurable reduction in allocation count for classes with many primitive fields.

You can opt in to a lower-boxing path by deriving a `TypedFactory[T]` and attaching it to the
configured reader. This is an example using the macros package, and attaching
to a `Configured` instance:

```scala
import scalanotation.*
import scalanotation.macros.TypedFactories

final case class Endpoint(host: String, port: Int, secure: Boolean)

given TypedFactory[Endpoint] = TypedFactories.derived
given Configured[Endpoint]   = Configured.typed
given Reader[Endpoint]       = Reader.configured.derived

given BatchContext = BatchContext.local()

val decoded =
  Readers.batched.readAs[Endpoint]("""(host = "localhost", port = 8080, secure = true)""")
```

`Configured.typed` is equivalent to `Configured.default.withTypedFactories`. It is most useful with
`Readers.batched`, where the decoder can reuse its typed product slots across reads.

Typed factories also compose with other configured derivation modes:

```scala
enum Shape:
  case Circle(radius: Double)
  case Rect(width: Int, height: Int)
  case Dot

given TypedFactory[Shape] = TypedFactories.derived

given Configured[Shape] =
  Configured.discriminator[Shape]("type").withTypedFactories

given Reader[Shape] =
  Reader.configured.derived
```

For sums, the derived `TypedFactory` stores typed factories for the structured product cases.
Nullary cases still decode as fixed nullary values and do not need a product factory.

### BuilderSlots

`BuilderSlots` is the decoder-owned product buffer behind the batched and typed decoding paths. A
slot records both the value and its kind. Reference values are stored as references, while primitive
values are packed without boxing.

You can either manually provide a TypedFactory yourself, or derive one from
the `scalanotation.macros` package.

```scala
import scalanotation.*

final case class Point(x: Int, y: Int)

given TypedFactory.OfProduct[Point]:
  def fromSlots(slots: BuilderSlots): Point =
    Point(slots.getInt(0), slots.getInt(1))
```

A `TypedFactory` must treat `BuilderSlots` as borrowed decoder state: read the values during
`fromSlots` and do not store the `BuilderSlots` instance anywhere. In pooled batched decoding, the
same slot buffer may be reused for later decodes.

## Custom Decoding And Encoding

If a config field should still be represented as a simple scalar in text, but map to a richer type
in Scala, build on an existing type class:

```scala
// convert result of decoding
val r: Reader[T]
r.map(...); r.mapResult(...)

// convert input that will be passed to an encoder
val w: Writer[T]
w.contraMap(...)

// map in both directions
val rw: ReadWriter[T]
rw.bimap(...)(...); rw.bimapResult(...)(...)
```

Typical examples are dates, paths, IDs, and string-backed enums:

```scala
import scalanotation.*
import steps.result.Result

final case class UserId(value: Int)

given ReadWriter[UserId] =
  summon[ReadWriter[Int]].bimap(UserId(_))(_.value)
```

## Working With `Expr`

If you want a generic syntax tree first, read into `Expr` and decode later:

```scala
import scalanotation.*

val expr = Readers.readDeclAs[Expr](input, rootName = "conf", packageName = "example.config").get
val decoded = expr.decodeAs[(ok: Boolean, retries: Int)]
```

This is useful if you want to inspect, transform, or export a config before decoding it into a
final domain type.

## Strictness And Errors

Typed decoding is intentionally strict:

- if `packageName` is supplied for declaration parsing, the package statement must match exactly
- the requested root declaration name must match
- named tuple field names must match exactly
- field count must match exactly
- field order must match exactly
- duplicate fields are rejected
- singleton tuples must use `Tuple(value)`; `(value)` is not grouping syntax
- only `Int` literals, plus `Double` literals when reading as `Float`, are promoted across numeric targets, and only when exact

Errors include useful context:

- decode errors include nested paths such as `.database.host` or `.items[1]`
- token and parse errors include line and column information

This is especially useful for configuration, the exact location of an error helps a user correct
the issue.

## Supported Typed Targets

The library supports decoding and writing for:

- arbitrary Scala 3 named tuples, including deeply nested named tuple and `Vector` combinations
- arbitrary Scala 3 tuples, including `EmptyTuple` and singleton tuple types
- case classes, case objects, and enums via derivation
- `String`, `Char`, `Int`, `Long`, `Float`, `Double`, `Boolean`, and `Null`
- `Option[T]`
- `Vector[T]`
- `Array[T]` and `IArray[T]`
- arbitrary `scala.collection.Seq[T]`
- arbitrary `scala.collection.Map[String, T]`
- `Expr`
- custom types via mapping from an existing type class

## Why The Syntax Is Narrow

Scala Object Notation is meant to stay import-free and data-oriented. Apart from the built-in
collection forms `Vector(...)` and `Tuple(...)`, it does not support constructors (e.g. `Foo(1)`) or
references (e.g. `Bar`).

Keeping the format narrow has a few benefits:
- the document is clearly raw data, not executable code
- generic tooling can work without needing application-specific symbol resolution
- config stays copy-pasteable into ordinary Scala files without extra imports
- schema derivation remains structural and predictable

## Demo CLI

The `demo` module provides a small CLI for converting from Scala Object Notation to other
data formats (YAML, JSON):

```bash
./mill demo.run example/config.scala --name conf
```

Available options:

- `--name <value>`: required root declaration name
- `--tokens`: print the token stream before parsing
- `--json`: render the parsed value as JSON
- `--yaml`: render the parsed value as YAML
- `--safe-nums`: preserve lossy JSON numeric cases as strings where relevant

Examples:

```bash
./mill demo.run example/config.scala --name conf --tokens
./mill demo.run example/config.scala --name conf --json
./mill demo.run example/config.scala --name conf --yaml
./mill demo.run example/config.scala --name conf --json --safe-nums
```

## Project Layout

- `core`: tokenizer, AST, parser, schema derivation, decoding, and writing
- `macros`: opt-in macro derivation of typed factories and constructor default values
- `demo`: CLI for parsing config files and exporting JSON or YAML
- `example/config.scala`: minimal sample input for the demo

The code lives in the `scalanotation` package.

## Build And Test

This project uses Mill.

Run all tests:

```bash
./mill __.test
```

Compile the core module:

```bash
./mill core.compile
```

Compile the demo module:

```bash
./mill demo.compile
```

Run benchmarks with memory profiling:

```bash
./mill bench.HEAD.runJmhMem
```

Run benchmarks with memory profiling, extra iterations and JIT profiling: (and filter specific tests)

```bash
./mill bench.HEAD.runJmhMemJit 'TypedDecodeBenchmark\.(withVecBatched|withIntArrayBatched|primitive10ClassTypedBatched)'
```
