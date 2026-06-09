# Scala Object Notation

Scala Object Notation is subset of Scala programming language that can decode directly into data.

## Example: Configuration

Scala Object Notation supports writing a Scala source files in two modes:
- a single one top-level `val` declaration, optionally preceded by a single package statement;
- or a top-level expression.

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

- they can be compiled and introspected as part of a program or decoded by external programs.
- structural data can be interpreted via a schema to richer types (via `Reader` and `Writer` type classes) or used as is.
- config can be edited programatically and written back
- schema checking errors index to the position in the source file
- named tuples have strict ordering and non-duplication requirements.

There are no methods (yet?) only pure data.

## Supported Syntax

The supported syntax is deliberately small:

- an optional single `package foo.bar` statement before a declaration
- one top-level `val` declaration
- named tuples for structured objects,
- `Vector(...)` for sequences,
- scala literal values
- string concatenation

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
val decoded = Readers.readAs[scalanotation.Expr]("(ok = true, retries = 3)")
assert(decoded == NamedTupleExpr(Vector("ok" -> BooleanConstant(true), ...)))

val tuple = Readers.readAs[scalanotation.Expr]("""1 *: "two" *: Vector(3) *: EmptyTuple""")
assert(tuple == TupleExpr(Vector(IntConstant(1), StringConstant("two"), ...)))
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

For product types, skippable configured derivation still requires at least one non-`Option` field.
For discriminator sum types, the discriminator field is enough, so product cases may contain only
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
- only `Int` literals, plus `Double` literals when reading as `Float`, are promoted across numeric targets, and only when exact

Errors include useful context:

- decode errors include nested paths such as `.database.host` or `.items[1]`
- token and parse errors include line and column information

This is especially useful for configuration, the exact location of an error helps a user correct
the issue.

## Supported Typed Targets

The library supports decoding and writing for:

- arbitrary Scala 3 named tuples, including deeply nested named tuple and `Vector` combinations
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

Scala Object Notation is meant to stay import-free and data-oriented. That is why it does not
support constructors (e.g. `Foo(1)`) or references (e.g. `Bar`).

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
