package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import steps.result.Result

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class SchemaSuite extends ScalanotationSuite:
  private enum MiniNode:
    case Obj(fields: ListMap[String, MiniNode])
    case Arr(values: Vector[MiniNode])
    case Str(value: String)
    case IntNum(value: Int)
    case Bool(value: Boolean)
    case Null

  private object MiniNode:
    type FieldsBuilder = mutable.Builder[(String, MiniNode), ListMap[String, MiniNode]]
    type ValuesBuilder = mutable.Builder[MiniNode, Vector[MiniNode]]

    object ObjBuilder extends Reader.DictBuilder[MiniNode, FieldsBuilder, MiniNode]:
      def init(): FieldsBuilder =
        ListMap.newBuilder[String, MiniNode]

      def add(repr: FieldsBuilder, key: String, elem: MiniNode): FieldsBuilder =
        repr.addOne(key -> elem)

      def finish(repr: FieldsBuilder): MiniNode =
        MiniNode.Obj(repr.result())

    object ArrBuilder extends Reader.VectorBuilder[MiniNode, ValuesBuilder, MiniNode]:
      def init(): ValuesBuilder =
        Vector.newBuilder[MiniNode]

      def add(repr: ValuesBuilder, elem: MiniNode): ValuesBuilder =
        repr.addOne(elem)

      def finish(repr: ValuesBuilder): MiniNode =
        MiniNode.Arr(repr.result())

    object Select extends RouterSchema.Write[MiniNode]:
      def caseIndex(router: RouterSchema.Router, value: MiniNode): RouterSchema.Index =
        value match
          case MiniNode.Obj(_)    => router.recordIndex
          case MiniNode.Arr(_)    => router.vectorIndex
          case MiniNode.Str(_)    => router.stringIndex
          case MiniNode.IntNum(_) => router.intIndex
          case MiniNode.Bool(_)   => router.booleanIndex
          case MiniNode.Null      => router.nullIndex

    given ReadWriter[MiniNode] =
      ReadWriter.router[MiniNode]("MiniNode", "mini dynamic node")(
        cases = self =>
          List(
            RouterSchema.RouterConstruct.Record -> RouterSchema.Case(
              "Obj",
              ReadWriter.dict[MiniNode, MiniNode, FieldsBuilder](
                self,
                ObjBuilder,
                {
                  case MiniNode.Obj(fields) => fields.size
                  case other                => fail(s"Expected Obj, got $other")
                },
                {
                  case MiniNode.Obj(fields) => fields.iterator
                  case other                => fail(s"Expected Obj, got $other")
                }
              )
            ),
            RouterSchema.RouterConstruct.Tuple -> RouterSchema.Case(
              "TupleArr",
              ReadWriter.tupleOf[MiniNode, MiniNode, ValuesBuilder](
                self,
                ArrBuilder,
                {
                  case MiniNode.Arr(values) => values.length
                  case other                => fail(s"Expected Arr, got $other")
                },
                {
                  case MiniNode.Arr(values) => values.iterator
                  case other                => fail(s"Expected Arr, got $other")
                }
              )
            ),
            RouterSchema.RouterConstruct.Vector -> RouterSchema.Case(
              "VectorArr",
              ReadWriter.vector[MiniNode, MiniNode, ValuesBuilder](
                self,
                ArrBuilder,
                {
                  case MiniNode.Arr(values) => values.length
                  case other                => fail(s"Expected Arr, got $other")
                },
                {
                  case MiniNode.Arr(values) => values.iterator
                  case other                => fail(s"Expected Arr, got $other")
                }
              )
            ),
            RouterSchema.RouterConstruct.String -> RouterSchema.Case(
              "Str",
              summon[ReadWriter[String]].bimap(MiniNode.Str(_)) {
                case MiniNode.Str(value) => value
                case other               => fail(s"Expected Str, got $other")
              }
            ),
            RouterSchema.RouterConstruct.Int -> RouterSchema.Case(
              "IntNum",
              ReadWriter.int[MiniNode](MiniNode.IntNum(_)) {
                case MiniNode.IntNum(value) => value
                case other                  => fail(s"Expected IntNum, got $other")
              }
            ),
            RouterSchema.RouterConstruct.Boolean -> RouterSchema.Case(
              "Bool",
              summon[ReadWriter[Boolean]].bimap(MiniNode.Bool(_)) {
                case MiniNode.Bool(value) => value
                case other                => fail(s"Expected Bool, got $other")
              }
            ),
            RouterSchema.RouterConstruct.Null -> RouterSchema.Case(
              "Null",
              ReadWriter.forNull(MiniNode.Null)
            )
          ),
        write = Select
      )

  test("schema mappings only appear on mapped schemas"):
    final case class UserId(value: Int)
    final case class UserLabel(value: String)

    assertEquals(summon[Reader[Int]].schema, RawSchema.Int)
    assertEquals(summon[Writer[Int]].schema, RawSchema.Int)

    val mappedReader = summon[Reader[Int]].map(UserId(_))
    mappedReader.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base.asInstanceOf[Any], RawSchema.Int)
        assertEquals(mapping.resultMap, null)
        mapping.totalMaps match
          case RawSchema.SchemaMapping.TotalMap.AnyMap(_) => ()
          case other => fail(s"Expected a pure mapped reader schema, got $other")
        assertEquals(mapping.inputMap, null)
        assertEquals(mappedReader.schema.describeSelf, "Int")
      case other =>
        fail(s"Expected a mapped reader schema, got $other")

    val chainedReader = summon[Reader[Int]].map(UserId(_)).map(id => UserLabel(id.value.toString))
    chainedReader.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base.asInstanceOf[Any], RawSchema.Int)
        assertEquals(mapping.resultMap, null)
        mapping.totalMaps match
          case RawSchema.SchemaMapping.TotalMap.AnyMap(_) => ()
          case other => fail(s"Expected a composed pure mapped reader schema, got $other")
      case other =>
        fail(s"Expected a mapped reader schema, got $other")
    assertEquals(Readers.readAs[UserLabel]("1")(using chainedReader), Result.Ok(UserLabel("1")))

    val mappedWriter = summon[Writer[Int]].contramap[UserId](_.value)
    mappedWriter.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base.asInstanceOf[Any], RawSchema.Int)
        assertEquals(mapping.resultMap, null)
        assert(mapping.inputMap != null)
        assertEquals(mappedWriter.schema.describeSelf, "Int")
      case other =>
        fail(s"Expected a mapped writer schema, got $other")

    val mappedReadWriter = summon[ReadWriter[Int]].bimap(UserId(_))(_.value)
    mappedReadWriter.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base.asInstanceOf[Any], RawSchema.Int)
        assertEquals(mapping.resultMap, null)
        mapping.totalMaps match
          case RawSchema.SchemaMapping.TotalMap.AnyMap(_) => ()
          case other => fail(s"Expected a pure mapped read-writer schema, got $other")
        assert(mapping.inputMap != null)
      case other =>
        fail(s"Expected a mapped read-writer schema, got $other")

    Reader.forNull(UserId(1)).schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base.asInstanceOf[Any], RawSchema.Null)
        assertEquals(mapping.resultMap, null)
        mapping.totalMaps match
          case RawSchema.SchemaMapping.TotalMap.AnyMap(_) => ()
          case other => fail(s"Expected a pure mapped nullary schema, got $other")
        assertEquals(mapping.inputMap, null)
      case other =>
        fail(s"Expected a mapped nullary schema, got $other")

  test("mapping an already mapped ReadWriter composes read and write"):
    final case class UserId(value: Int)
    final case class UserLabel(value: String)

    val idReadWriter =
      summon[ReadWriter[Int]].bimap(value => UserId(value + 10))(id => id.value - 10)
    val labelReadWriter =
      idReadWriter.bimap(id => UserLabel(id.value.toString))(label => UserId(label.value.toInt))

    labelReadWriter.schema match
      case RawSchema.Mapped(base, mapping) =>
        assertEquals(base.asInstanceOf[Any], RawSchema.Int)
        assertEquals(mapping.resultMap, null)
        mapping.totalMaps match
          case RawSchema.SchemaMapping.TotalMap.AnyMap(_) => ()
          case other => fail(s"Expected a composed pure mapped read-writer schema, got $other")
        assert(mapping.inputMap != null)
      case other =>
        fail(s"Expected a mapped read-writer schema, got $other")

    assertEquals(
      Readers.readAs[UserLabel]("5")(using labelReadWriter.reader),
      Result.Ok(UserLabel("15"))
    )
    assertEquals(Writers.write(UserLabel("15"))(using labelReadWriter.writer), "5")

  test("public raw schema exposes typed mapping internals"):
    final case class UserId(value: Int)

    val readWriter                = summon[ReadWriter[Int]].bimap(UserId(_))(_.value)
    val schema: RawSchema[UserId] = readWriter.schema

    schema match
      case mapped: RawSchema.Mapped[Int, UserId] @unchecked =>
        val result: Result[UserId, DecodeError] = mapped.mapping.mapResult(12)
        val input: Int                          = mapped.mapping.mapInput(UserId(34))

        assertEquals(mapped.base, RawSchema.Int)
        assertEquals(result, Result.Ok(UserId(12)))
        assertEquals(input, 34)
      case other =>
        fail(s"Expected a typed mapped schema, got ${other.describeSelf}")

  test("Null is a primitive schema and decodes to null"):
    assertEquals(summon[Reader[Null]].schema, RawSchema.Null)
    assertEquals(summon[Writer[Null]].schema, RawSchema.Null)
    assertEquals(summon[ReadWriter[Null]].schema, RawSchema.Null)

    assertEquals(Readers.readAs[Null]("null"), Result.Ok(null: Null))
    assertEquals(Writers.write(null: Null), "null")

  test("Expr is represented as a recursive router schema"):
    summon[ReadWriter[Expr]].schema match
      case router: RawSchema.Router[?] =>
        assertEquals(router.numberMode, RouterSchema.NumberMode.Bounded)
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
          RawSchema.routerCase(router, router.router.recordIndex).nn.name,
          "NamedTupleExpr"
        )
        router.cases(RawSchema.ExprRouter.NamedTupleCase).schema match
          case dict: RawSchema.Dict[?] =>
            dict.element match
              case ref: RawSchema.Ref[?] =>
                assertEquals(ref.name, "Expr")
                assert(ref.target().asInstanceOf[AnyRef].eq(router))
              case other =>
                fail(s"Expected a recursive Expr ref, got ${other.describeSelf}")
          case other =>
            fail(s"Expected a recursive Expr dict case, got ${other.describeSelf}")
      case other =>
        fail(s"Expected Expr router schema, got ${other.describeSelf}")

    val expr = Expr.TupleExpr(
      IndexedSeq(Expr.TupleExpr(IndexedSeq(Expr.IntConstant(1))))
    )
    assertEquals(Writers.write(expr), "Tuple(Tuple(1))")
    assertEquals(Readers.readAs[Expr](Writers.write(expr)), Result.Ok(expr))

  test("public RouterSchema duplicate read routes use the most recent case"):
    val reader = Reader.router[String]("DuplicateString", "duplicate string router")(
      cases = _ =>
        List(
          RouterSchema.RouterConstruct.String -> RouterSchema.ReadCase(
            "FirstString",
            summon[Reader[String]].map(value => s"first:$value")
          ),
          RouterSchema.RouterConstruct.String -> RouterSchema.ReadCase(
            "SecondString",
            summon[Reader[String]].map(value => s"second:$value")
          )
        )
    )

    assertEquals(Readers.readAs[String]("\"hello\"")(using reader), Result.Ok("second:hello"))
    reader.schema match
      case router: RawSchema.Router[?] =>
        assertEquals(
          RawSchema.routerCase(router, router.router.stringIndex).nn.name,
          "SecondString"
        )
      case other =>
        fail(s"Expected a router schema, got ${other.describeSelf}")

  test("public RouterSchema builds recursive dynamic read-writers"):
    import MiniNode.*

    val expected = Obj(
      ListMap(
        "items" -> Arr(Vector(IntNum(1), Str("two"), Null)),
        "tuple" -> Arr(Vector(Bool(true))),
        "ok"    -> Bool(false)
      )
    )

    val input =
      """(
        |  items = Vector(1, "two", null),
        |  tuple = Tuple(true),
        |  ok = false
        |)
        |""".stripMargin

    assertEquals(Readers.readAs[MiniNode](input), Result.Ok(expected))
    assertEquals(
      Writers.write(expected),
      """(items = Vector(1, "two", null), tuple = Vector(true), ok = false)"""
    )
    assertEquals(Writers.writeExpr(expected).decodeAs[MiniNode], Result.Ok(expected))

    summon[ReadWriter[MiniNode]].schema match
      case router: RawSchema.Router[?] =>
        assertEquals(router.numberMode, RouterSchema.NumberMode.Bounded)
        assertEquals(
          RawSchema.routerCase(router, router.router.tupleIndex).nn.name,
          "TupleArr"
        )
        router.cases(0).schema match
          case dict: RawSchema.Dict[?] =>
            dict.element match
              case ref: RawSchema.Ref[?] =>
                assertEquals(ref.name, "MiniNode")
                assert(ref.target().asInstanceOf[AnyRef].eq(router))
              case other =>
                fail(s"Expected a recursive MiniNode ref, got ${other.describeSelf}")
          case other =>
            fail(s"Expected a recursive dict case, got ${other.describeSelf}")
      case other =>
        fail(s"Expected a router schema, got ${other.describeSelf}")

  test("RawSchema.describeSelf"):
    // primitive schemas
    assertEquals(RawSchema.Int.describeSelf, "Int")
    assertEquals(RawSchema.Long.describeSelf, "Long")
    assertEquals(RawSchema.Float.describeSelf, "Float")
    assertEquals(RawSchema.Double.describeSelf, "Double")
    assertEquals(RawSchema.Boolean.describeSelf, "Boolean")
    assertEquals(RawSchema.Char.describeSelf, "Char")
    assertEquals(RawSchema.String.describeSelf, "String")
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
      "Tuple(...)"
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
