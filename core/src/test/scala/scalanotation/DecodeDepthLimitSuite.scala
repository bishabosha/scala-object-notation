package scalanotation

import steps.result.Result

import scala.collection.mutable

class DecodeDepthLimitSuite extends ScalanotationSuite:
  /** `Vector(Vector(...Vector(1)...))` nested `depth` times */
  private def nestedVectorInput(depth: Int): String =
    val builder = new StringBuilder
    var i       = 0
    while i < depth do
      builder ++= "Vector("
      i += 1
    builder += '1'
    i = 0
    while i < depth do
      builder += ')'
      i += 1
    builder.result()

  private enum Nested:
    case Wrap(inner: Vector[Nested])
    case Leaf

  private object Nested:
    private object WrapBuilder
        extends Reader.VectorBuilder[Nested, mutable.Builder[Nested, Vector[Nested]], Nested]:
      def init(): mutable.Builder[Nested, Vector[Nested]] =
        Vector.newBuilder[Nested]
      def add(
          repr: mutable.Builder[Nested, Vector[Nested]],
          elem: Nested
      ): mutable.Builder[Nested, Vector[Nested]] =
        repr.addOne(elem)
      def finish(repr: mutable.Builder[Nested, Vector[Nested]]): Nested =
        Nested.Wrap(repr.result())

    given Reader[Nested] =
      Reader.router[Nested]("Nested", "nested test node")(
        cases = self =>
          List(
            RouterSchema.RouterConstruct.Vector -> RouterSchema.ReadCase(
              "Wrap",
              Reader.vector(self, WrapBuilder)
            ),
            RouterSchema.RouterConstruct.Int -> RouterSchema.ReadCase(
              "Leaf",
              summon[Reader[Int]].map(_ => Nested.Leaf)
            )
          )
      )

  private def nestedVectorExpr(depth: Int): Expr =
    var expr: Expr = Expr.IntConstant(1)
    var i          = 0
    while i < depth do
      expr = Expr.VectorExpr(IndexedSeq(expr))
      i += 1
    expr

  private def nestedDepth(node: Nested): Int =
    var depth   = 0
    var current = node
    var done    = false
    while !done do
      current match
        case Nested.Wrap(inner) =>
          depth += 1
          current = inner.head
        case Nested.Leaf =>
          done = true
    depth

  test("deeply nested text input is rejected with an error instead of a stack overflow"):
    Readers.readAs[Expr](nestedVectorInput(100_000)) match
      case Result.Err(err) =>
        assert(clue(err.format).contains("Nesting depth"))
      case Result.Ok(value) =>
        fail(s"Expected nesting-depth error, got $value")

  test("moderately nested text input still decodes"):
    Readers.readAs[Expr](nestedVectorInput(100)) match
      case Result.Ok(_)    => ()
      case Result.Err(err) => fail(s"Expected successful decode, got ${err.format}")

  test("deeply nested recursive router input is rejected with an error"):
    Readers.readAs[Nested](nestedVectorInput(100_000)) match
      case Result.Err(err) =>
        assert(clue(err.format).contains("Nesting depth"))
      case Result.Ok(value) =>
        fail(s"Expected nesting-depth error, got $value")

  test("moderately nested recursive router input still decodes"):
    Readers.readAs[Nested](nestedVectorInput(100)) match
      case Result.Ok(value) => assertEquals(nestedDepth(value), 100)
      case Result.Err(err)  => fail(s"Expected successful decode, got ${err.format}")

  test("deeply nested Expr tree decode is rejected with an error"):
    nestedVectorExpr(100_000).decodeAs[Nested] match
      case Result.Err(err) =>
        assert(clue(err.format).contains("Nesting depth"))
      case Result.Ok(value) =>
        fail(s"Expected nesting-depth error, got $value")

  test("moderately nested Expr tree decode still succeeds"):
    nestedVectorExpr(100).decodeAs[Nested] match
      case Result.Ok(value) => assertEquals(nestedDepth(value), 100)
      case Result.Err(err)  => fail(s"Expected successful decode, got ${err.format}")

  test("pooled decoder recovers after a nesting-depth error"):
    given BatchContext = BatchContext.local()
    // hitting the depth limit leaves the counter stale until reset; the next borrow must succeed
    Readers.batched.readAs[Expr](nestedVectorInput(100_000)) match
      case Result.Err(err)  => assert(clue(err.format).contains("Nesting depth"))
      case Result.Ok(value) => fail(s"Expected nesting-depth error, got $value")
    Readers.batched.readAs[Expr](nestedVectorInput(100)) match
      case Result.Ok(_)    => ()
      case Result.Err(err) => fail(s"Expected successful decode after error, got ${err.format}")
