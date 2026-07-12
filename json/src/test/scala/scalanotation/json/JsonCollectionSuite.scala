package scalanotation.json

import steps.result.Result

class JsonCollectionSuite extends munit.FunSuite:

  test("vectors of primitives"):
    assertEquals(Json.readAs[Vector[Int]]("[1,2,3]"), Result.Ok(Vector(1, 2, 3)))
    assertEquals(Json.readAs[Vector[Int]]("[]"), Result.Ok(Vector.empty[Int]))
    assertEquals(Json.readAs[Vector[String]]("""["a","b"]"""), Result.Ok(Vector("a", "b")))
    assertEquals(Json.write(Vector(1, 2, 3)), "[1,2,3]")
    assertEquals(Json.write(Vector.empty[Int]), "[]")

  test("seqs and lists"):
    assertEquals(Json.readAs[List[Int]]("[1,2]"), Result.Ok(List(1, 2)))
    assertEquals(Json.write(List(1, 2)), "[1,2]")

  test("arrays decode through the unboxed builders"):
    assertEquals(Json.readAs[Array[Int]]("[1,2,3]").map(_.toSeq), Result.Ok(Seq(1, 2, 3)))
    assertEquals(Json.readAs[Array[Double]]("[1.5,2.5]").map(_.toSeq), Result.Ok(Seq(1.5, 2.5)))
    assertEquals(Json.write(Array(1, 2, 3)), "[1,2,3]")

  test("trailing comma or missing separator is an error"):
    assert(Json.readAs[Vector[Int]]("[1,2,]").isErr)
    assert(Json.readAs[Vector[Int]]("[1 2]").isErr)
    assert(Json.readAs[Vector[Int]]("[1,2").isErr)

  test("element errors carry the index path"):
    Json.readAs[Vector[Int]]("""[1,"x",3]""") match
      case Result.Err(error) => assert(error.format.contains("[1]"), clue = error.format)
      case other             => fail(s"expected an error, got $other")

  test("string-keyed maps are objects"):
    assertEquals(
      Json.readAs[Map[String, Int]]("""{"a":1,"b":2}"""),
      Result.Ok(Map("a" -> 1, "b" -> 2))
    )
    assertEquals(Json.readAs[Map[String, Int]]("{}"), Result.Ok(Map.empty[String, Int]))
    assertEquals(Json.write(Map("a" -> 1, "b" -> 2)), """{"a":1,"b":2}""")

  test("duplicate object keys are an error"):
    assert(Json.readAs[Map[String, Int]]("""{"a":1,"a":2}""").isErr)

  test("non-string-keyed maps are arrays of pairs"):
    assertEquals(
      Json.readAs[Map[Int, String]]("""[[1,"a"],[2,"b"]]"""),
      Result.Ok(Map(1 -> "a", 2 -> "b"))
    )
    assertEquals(Json.write(Map(1 -> "a")), """[[1,"a"]]""")
    assert(Json.readAs[Map[Int, String]]("""[[1,"a",2]]""").isErr)
    assert(Json.readAs[Map[Int, String]]("""[[1]]""").isErr)

  test("tuples are fixed-arity arrays"):
    assertEquals(Json.readAs[(Int, String, Boolean)]("""[1,"a",true]"""), Result.Ok((1, "a", true)))
    assertEquals(Json.write((1, "a", true)), """[1,"a",true]""")
    assert(Json.readAs[(Int, String)]("""[1]""").isErr)
    assert(Json.readAs[(Int, String)]("""[1,"a",2]""").isErr)

  test("nested collections"):
    assertEquals(
      Json.readAs[Vector[Vector[Int]]]("[[1],[2,3],[]]"),
      Result.Ok(Vector(Vector(1), Vector(2, 3), Vector.empty))
    )
    assertEquals(
      Json.readAs[Map[String, Vector[Int]]]("""{"a":[1],"b":[]}"""),
      Result.Ok(Map("a" -> Vector(1), "b" -> Vector.empty))
    )

  test("options nest inside collections"):
    assertEquals(
      Json.readAs[Vector[Option[Int]]]("[1,null,3]"),
      Result.Ok(Vector(Some(1), None, Some(3)))
    )
    assertEquals(Json.write(Vector(Some(1), None, Some(3))), "[1,null,3]")
