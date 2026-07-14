package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.schema.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class DerivationFailureSuite extends ScalanotationSuite:
  test("no decoder is derived for Any"):
    val errors = typeCheckErrors("summon[scalanotation.Reader[Any]]")
    assert(errors.nonEmpty)

  test("no decoder is derived for Vector[Any]"):
    val errors =
      typeCheckErrors("summon[scalanotation.Reader[Vector[Any]]]")
    assert(errors.nonEmpty)

  test("compile-time derivation error includes nested field path"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (outer: (bad: Box[Int]))\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("outer.bad"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error includes nested field path Vector"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (outer: (inner: Vector[(sub1: (bad: Box[Int]))]))\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".outer.inner[].sub1.bad"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error includes nested field path Vector root"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = Vector[(sub1: (bad: Box[Int]))]\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("'[].sub1.bad'"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error includes vector path segment"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (items: Vector[(bad: Box[Int])])\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("'.items[].bad'"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error keeps nested path with overridden readers"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (outer: (count: Int, note: Option[String], bad: Box[Int]))\n" +
        "given scalanotation.Reader[Int] = scalanotation.Reader.int(_ + 100)\n" +
        "given scalanotation.Reader[Option[String]] = summon[scalanotation.Reader[String]].map(value => Some(value))\n" +
        "summon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("'.outer.bad'"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )

  test("compile-time derivation error keeps vector path with overridden readers"):
    class Box[T]
    val errors = typeCheckErrors(
      "type Data = (items: Vector[(count: Int, note: Option[String], bad: Box[Int])])\n" +
        "given scalanotation.Reader[Int] = scalanotation.Reader.int(_ + 100)\n" +
        "given scalanotation.Reader[Option[String]] = summon[scalanotation.Reader[String]].map(value => Some(value))\n" +
        "summon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains("'.items[].bad'"))
    assert(
      clue(errors.head.message)
        .contains("Box[scala.Int]")
    )
