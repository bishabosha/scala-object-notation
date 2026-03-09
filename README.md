# Named Tuple Parser Extract

This is a small standalone Scala project that extracts the minimum parser shape needed to parse a single declaration of the form:

```scala
val data = (
  x = (
    ls = Vector("abc" + "def", 'b', 123, 3.1, 4.1f, 23L),
  ),
  y = null
)
```

Supported subset:

- one top-level `val` declaration
- nested Vectors and named tuples of arbitrary depth
- literals: `null`, booleans, strings, chars, integers, longs, floats, doubles
- string concatenation with `+`
- trailing commas inside tuples
- Scala comments: `// ...` and `/* ... */` (including nested block comments)

Not supported:

- general Scala expressions
- arbitrary declarations
- type syntax
- interpolation-heavy string syntax beyond standard string and char escapes

Typed deserialization:

- AST values can be decoded directly into a Scala 3 named tuple type.
- Supported target field types are nested named tuples, `Vector[T]`, `String`, `Char`, `Int`, `Long`, `Float`, `Double`, `Boolean`, and `Null`.
- Schemas are derived implicitly from the target type and validated before the result is cast to the requested named tuple type.
- Named tuple field order must match the target type exactly.

Example:

```scala
type Data = (x: (label: String, ys: Vector[Int]), y: Null, ok: Boolean)

val decoded = Parser.parseNamedTupleAs[Data](input)
```

Run:

```bash
cd sandbox/named-tuple-parser
sbt test
sbt "demo/runMain miniparser.Main examples/sample.scala"
sbt "demo/runMain miniparser.Main examples/sample.scala --json"
```
