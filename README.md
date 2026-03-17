# Scala Object Notation

This repository contains a small Scala 3 parser and decoder for a constrained, data-oriented subset of Scala syntax.

Currently the supported use case is decoding data from text into either:
- a Syntax tree (`Expr`),
- a Scala type that directly matches the structure as written, (preserving nested named tuple types)
- or advanced decoders into custom types.

Decoding into custom types is theoretically faster than from JSON as the order of fields is significant,
and repeated field names are forbidden.

This can be used as an alternative configuration file format for applications.
Currently encoders are not supported but this is an area of interest.

## What Is Supported

A source file deliberately supports only syntax that is valid with the default imports and classpath
of Scala 3.8.1. (So no references to external classes)

Only allowed are expressions of the types:
- String literal (and `+` concatenation)
- Int, Long, Float, Double literal
- `null`
- `true` or `false`
- NamedTuple literal (for structured objects)
- `Vector(...)` literal (for random-access sequences)

```scala
/** comments are valid too! */
val conf = (
  x = (
    label = "abc" + "def",
    ys = Vector(1, 2L, -0x1A, 3.14f, 1_000_000)
  ),
  y = null,
  // z = temp removal
  ok = true
)
```

A source file consists of a single top-level `val` declaration (of any name), meaning it should
be a valid `.scala` file.

> If tooling permits we could envision a `.scon` file format that only consists of an expression,
> and no declaration.

Not supported:
- multiple declarations in one file
- arbitrary Scala expressions
- methods, classes, imports, or type definitions
- general collection syntax beyond `Vector(...)`
- string interpolation or advanced string forms

## Decoding

The `core` module exposes two main flows.

Parse and decode directly into type that matches the structure of data using a contextual `TaggedDecoder[T]`:

```scala
import scalanotation.*

type Data =
  (x: (label: String, xs: Vector[String], ys: IArray[Int]), y: Option[Int], ok: Boolean)

// `given TaggedDecoder[Data]` is derived automatically
val decoded: Result[Data, DecodeError] =
  Parser.parseValueAs[Data](input, name = "conf")
```

You can also parse as a generic syntax tree (`Expr`) first and use that, or decode into
structured data afterwords:

```scala
import scalanotation.*

type Data = (ok: Boolean)

val expr = Parser.parseValueAs[Expr](input, name = "conf").get
val decoded = expr.decodeAs[Data]
```

Supported typed decoding targets currently include:

- nested Scala 3 named tuples
- `Vector[T]`, `Array[T]`, `IArray[T]`
- `Option[T]` for nullable values
- `Expr` (generic syntax tree)
- `String`, `Char`, `Int`, `Long`, `Float`, `Double`, and `Boolean`
- custom types via transformation of an existing `TaggedSchema[T]`
- case class, case object, and enum derived encoders via `derives TaggedSchema`

**Mapping an existing Schema**

Preferred for direct decoding from Strings into domain objects such as Date, or simple enums.

```scala
import scalanotation.*
import steps.result.Result

import java.time.LocalDate

enum Mode:
  case Fast, Safe

given TaggedSchema[Mode] =
  summon[TaggedSchema[String]].emap {
    case "fast" => Result.Ok(Mode.Fast)
    case "safe" => Result.Ok(Mode.Safe)
    case other  => Result.Err(DecodeError.Custom(s"Unknown mode '$other'"))
  }
```

**Derived Schemas**
For product and sum types, you can derive a decoder automatically, (semi-auto)
it is composed of exising schemas for each field.

```scala
import scalanotation.*
import steps.result.Result, Result.eval.raise

import java.time.LocalDate

case class Metadata(created: LocalDate, tags: Vector[String]) derives TaggedSchema
case class User(name: String, age: Int, metadata: Metadata) derives TaggedSchema

given TaggedSchema[LocalDate] =
  summon[TaggedSchema[String]].emap { raw =>
    Result:
      try LocalDate.parse(raw)
      catch case _: java.time.format.DateTimeParseException =>
        raise(DecodeError.Custom(s"Invalid ISO date '$raw'"))
  }
```

> Note: for case class with no fields or a case object, the payload should be a named tuple with a single field - the class label, and null value.
> e.g. `case class Foo()` will derive a decoder for `(Foo = null)`

Derivation also supports enums. Each case is represented as a single-field object where the field name is the case label. Cases with fields use a nested named tuple payload, and nullary cases use `null`.

```scala
import scalanotation.*
import steps.result.Result, Result.eval.raise

import java.time.LocalDate

enum Mode derives TaggedSchema:
  case Fast
  case Scheduled(at: LocalDate, retries: Int)

given TaggedSchema[LocalDate] =
  summon[TaggedSchema[String]].emap { raw =>
    Result:
      try LocalDate.parse(raw)
      catch case _: java.time.format.DateTimeParseException =>
        raise(DecodeError.Custom(s"Invalid ISO date '$raw'"))
  }

// Fast        => (Fast = null)
// Scheduled   => (Scheduled = (at = "2026-03-15", retries = 2))
```

Typed decoding is strict:

- the requested root declaration name must match
- named tuple field order must match the target type
- field count and field names must match exactly
- decode errors include nested path information such as `.items[0].value`
- token-based parsing errors include line and column information

### Why not support class constructors and object references?

> i.e. why no `val data = Foo(23)` and `val data = Bar` in a source file?

It is still possible to derive decoders automatically for these types, just the syntax may be awkward for enums,
and singleton objects (i.e. a discriminator is needed) (e.g. `(Bar = null)` and `(Foo = (id = 23))`).

The envisioned use case is to parse a valid Scala file with no imports needed, i.e. to encode raw data
with the syntax of Scala. Permitting references to external classes would mean we can no longer
copy-paste the raw data into any scala file, (ignoring import overrides)
now we need to resolve external references!

Also opening the config file would render the presentation compiler useless, so we would be required
to build a more specialised tool that expects unknown references.

How would a generic tool such as a "`jq` for Scala Object Notation" work to traverse such objects
without type information?

Likely `Foo(23)` would be illegal as it could be ambiguous with sequence syntax,
and therefore only `Foo(id = 23)` allowed i.e. require named-arguments,
and then it can be a "labelled object literal".

Perhaps these could be traversed by a new notion of path, instead of just `.name` for a field, and
`[i]` for an index, perhaps `#Foo` to "cast down" to that type, and nothing is needed for `Bar`  as
it is an opaque reference.

It becomes tougher to understand the meaning of the document
because now it is harder to preserve an illusion of "simple data",
and we must now ask "what version of `Foo` is it?" what fields is it expected to have? is subtyping meaningful?

And for a standalone identifier `Bar`, it can only be treated as a reference of unknown type until decoding.

At least with decoding a literal like `(Foo = (id = 23))` into `Foo(23)` then it is more honest that
it is unversioned, and now the document itself should carry that information.

Another potential enabler could be to support explicit schemas to encode such definitions,
but then they have to be included or referenced somehow in the document (again requiring tooling support).

## Demo CLI

The `demo` module provides a small CLI entry point:

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

## Structure
The current project state is:

- `core`: tokenizer, AST model, parser, schema validation, and typed decoding into Scala 3 named tuples
- `demo`: a CLI that reads a config-like Scala file, optionally prints tokens, and can render the parsed value as JSON or YAML
- `example/config.scala`: a minimal input file used by the demo

The code currently lives in the `scalanotation` package.

## Build And Test

This project uses Mill.

Run the full test suite:

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

## Current Coverage

The test suite currently covers:

- AST parsing for nested tuples and vectors
- booleans, negative numbers, binary and hexadecimal literals
- line comments and nested block comments
- typed decoding into nested named tuples
- vector decoding for structured values
- root-name validation and field-order validation
- runtime decode error reporting with path and source location information
- compile-time schema derivation failures for unsupported target types
- demo JSON and YAML rendering
