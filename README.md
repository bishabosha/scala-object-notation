# Scala Object Notation

This repository contains a small Scala 3 parser and decoder for a constrained, data-oriented subset of Scala syntax.

The current project state is:

- `core`: tokenizer, AST model, parser, schema validation, and typed decoding into Scala 3 named tuples
- `demo`: a CLI that reads a config-like Scala file, optionally prints tokens, and can render the parsed value as JSON or YAML
- `example/config.scala`: a minimal input file used by the demo

The code currently lives in the `scalanotation` package.

## What It Parses

The parser is intentionally narrow. It is designed for files shaped like a single top-level `val` declaration:

```scala
val conf = (
  x = (
    label = "abc" + "def",
    ys = Vector(1, 2L, -0x1A, 3.14f)
  ),
  y = null,
  ok = true
)
```

Supported syntax currently includes:

- one top-level `val name = ...` declaration
- named tuples using parentheses and `field = value`
- nested named tuples and `Vector(...)` values
- strings, chars, booleans, and `null`
- integers, longs, floats, and doubles
- decimal, binary (`0b...`), and hexadecimal (`0x...`) integer literals
- numeric separators such as `1_000` and `0x00_1A`
- unary minus for numeric literals
- string literal concatenation with `+`
- line comments `// ...`
- nested block comments `/* ... */`

## What It Does Not Parse

This is still a subset parser, not a general Scala parser.

Not supported:

- multiple declarations in one file
- arbitrary Scala expressions
- methods, classes, imports, or type definitions
- general collection syntax beyond `Vector(...)`
- string interpolation or advanced string forms
- decoding to arbitrary Scala types

## Public API

The `core` module exposes two main flows.

Parse into the generic AST:

```scala
import scalanotation.*

val parsed = Parser.parseAs[Expr](input)
val ast = parsed.get.declaration.value
```

Parse and decode directly into a Scala 3 named tuple:

```scala
import scalanotation.*

type Data =
  (x: (label: String, ys: Vector[Int]), y: Null, ok: Boolean)

val decoded = Parser.parseValueAs[Data](input, name = "conf")
```

You can also parse as `Expr` first and validate later:

```scala
import scalanotation.*

type Data = (ok: Boolean)

val expr = Parser.parseValueAs[Expr](input, name = "conf").get
val decoded = expr.decodeAs[Data]
```

Supported typed decoding targets currently include:

- nested Scala 3 named tuples
- `Vector[T]`
- `Expr`
- `String`, `Char`, `Int`, `Long`, `Float`, `Double`, `Boolean`, and `Null`

Typed decoding is strict:

- the requested root declaration name must match
- named tuple field order must match the target type
- field count and field names must match exactly
- decode errors include nested path information such as `.items[0].value`
- token-based parsing errors include line and column information

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
