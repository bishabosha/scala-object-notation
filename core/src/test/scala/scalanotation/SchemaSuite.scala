package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.internal.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class SchemaSuite extends ScalanotationSuite:
  test("schema mappings only appear on mapped schemas"):
    final case class UserId(value: Int)

    assertEquals(summon[Reader[Int]].schema, RawSchema.Int)
    assertEquals(summon[Writer[Int]].schema, RawSchema.Int)

    val mappedReader = summon[Reader[Int]].map(UserId(_))
    mappedReader.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Int)
        assert(mapping.resultMap != null)
        assertEquals(mapping.inputMap, null)
        assertEquals(mappedReader.schema.describeSelf, "Int")
      case other =>
        fail(s"Expected a mapped reader schema, got $other")

    val mappedWriter = summon[Writer[Int]].contramap[UserId](_.value)
    mappedWriter.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Int)
        assertEquals(mapping.resultMap, null)
        assert(mapping.inputMap != null)
        assertEquals(mappedWriter.schema.describeSelf, "Int")
      case other =>
        fail(s"Expected a mapped writer schema, got $other")

    val mappedReadWriter = summon[ReadWriter[Int]].bimap(UserId(_))(_.value)
    mappedReadWriter.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Int)
        assert(mapping.resultMap != null)
        assert(mapping.inputMap != null)
      case other =>
        fail(s"Expected a mapped read-writer schema, got $other")

    Reader.forNull(UserId(1)).schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base, RawSchema.Null)
        assert(mapping.resultMap != null)
        assertEquals(mapping.inputMap, null)
      case other =>
        fail(s"Expected a mapped nullary schema, got $other")

  test("Null is a primitive schema and decodes to null"):
    assertEquals(summon[Reader[Null]].schema, RawSchema.Null)
    assertEquals(summon[Writer[Null]].schema, RawSchema.Null)
    assertEquals(summon[ReadWriter[Null]].schema, RawSchema.Null)

    assertEquals(Readers.readAs[Null]("null"), Result.Ok(null: Null))
    assertEquals(Writers.write(null: Null), "null")

  test("Expr is represented as a recursive router schema"):
    summon[ReadWriter[Expr]].schema match
      case router: RawSchema.Router =>
        assertEquals(router.numberMode, RawSchema.RouterNumberMode.Bounded)
        assert(router.read != null)
        assert(router.write != null)
        assertEquals(
          router.cases.iterator.map(_.name).toList,
          List(
            "NamedTupleExpr",
            "TupleExpr",
            "VectorExpr",
            "StringConstant",
            "CharConstant",
            "IntConstant",
            "LongConstant",
            "FloatConstant",
            "DoubleConstant",
            "BooleanConstant",
            "NullConstant"
          )
        )
        assertEquals(
          router.read.nn.route(RawSchema.RouterConstruct.Record),
          RawSchema.ExprRouter.NamedTupleCase
        )
        router.cases(RawSchema.ExprRouter.NamedTupleCase).schema match
          case RawSchema.Dict(RawSchema.Ref("Expr", target), _, _) =>
            assert(target() eq router)
          case other =>
            fail(s"Expected a recursive Expr dict case, got ${other.describeSelf}")
      case other =>
        fail(s"Expected Expr router schema, got ${other.describeSelf}")

    val expr = Expr.TupleExpr(
      IndexedSeq(Expr.TupleExpr(IndexedSeq(Expr.IntConstant(1))))
    )
    assertEquals(Writers.write(expr), "(1 *: EmptyTuple) *: EmptyTuple")
    assertEquals(Readers.readAs[Expr](Writers.write(expr)), Result.Ok(expr))

  test("RawSchema.describeSelf"):
    // primitive schemas
    assertEquals(RawSchema.Int.describeSelf, "Int")
    assertEquals(RawSchema.Long.describeSelf, "Long")
    assertEquals(RawSchema.Float.describeSelf, "Float")
    assertEquals(RawSchema.Double.describeSelf, "Double")
    assertEquals(RawSchema.Boolean.describeSelf, "Boolean")
    assertEquals(RawSchema.Char.describeSelf, "Char")
    assertEquals(RawSchema.String.describeSelf, "String")
    assertEquals(RawSchema.AnyExpr.describeSelf, "Any")
    assertEquals(RawSchema.Null.describeSelf, "Null")
    // empty named tuple
    assertEquals(
      RawSchema.NamedTuple(IArray.empty[RawSchema.Field]).describeSelf,
      "AnyNamedTuple"
    )
    // named tuple with fields
    val withFields = RawSchema.NamedTuple(
      IArray(
        RawSchema.Field("x", summon[Reader[Int]].schema),
        RawSchema.Field("y", summon[Reader[String]].schema)
      )
    )
    assertEquals(withFields.describeSelf, "(x: ..., y: ...)")
    // single-case sum schema
    val sumSchema = RawSchema.Sum(
      IArray(RawSchema.SumCase("Fast", summon[Reader[Int]].schema))
    )
    assertEquals(sumSchema.describeSelf, "(Fast: ...)")
    enum AorB:
      case A, B

    // multi-case sum schema
    val multiSumDesc = RawSchema
      .Sum(
        IArray(
          RawSchema.SumCase("A", Reader.forNull(AorB.A).schema),
          RawSchema.SumCase("B", Reader.forNull(AorB.B).schema)
        )
      )
      .describeSelf
    assert(clue(multiSumDesc).contains("(A: ...)"))
    assert(clue(multiSumDesc).contains("(B: ...)"))
    assert(clue(multiSumDesc).contains(" | "))
    // Vector schema
    assertEquals(
      summon[Reader[Vector[Int]]].schema.describeSelf,
      "Vector[...]"
    )
    assertEquals(
      summon[Reader[Vector[(x: String, y: Int)]]].schema.describeSelf,
      "Vector[...]"
    )
    // Tuple schema
    assertEquals(
      RawSchema.Tuple(IArray(RawSchema.Int)).describeSelf,
      "... *: EmptyTuple"
    )
    assertEquals(
      summon[Reader[(Int, String)]].schema.describeSelf,
      "(..., ...)"
    )
    assertEquals(
      summon[Reader[(Int, (Boolean, String), Long)]].schema.describeSelf,
      "(..., ..., ...)"
    )
    // Option schema
    assertEquals(
      RawSchema.Option(summon[Reader[Int]].schema).describeSelf,
      "Int | Null"
    )
    assertEquals(
      RawSchema.Option(summon[Reader[String]].schema).describeSelf,
      "String | Null"
    )
