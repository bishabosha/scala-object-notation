package scalanotation

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scalanotation.internal.PublicInternal
import scalanotation.schema.RawSchema
import steps.result.Result

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

class DerivationFailureSuite extends ScalanotationSuite:
  test("no writer is derived for nested Option"):
    val errors = typeCheckErrors(
      "type Data = (x: Option[Option[Int]])\nsummon[scalanotation.Writer[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".x"))
    assert(
      clue(errors.head.message)
        .contains("Writer[Option[Option[?]]] is not supported")
    )

  test("no decoder is derived for Any"):
    val errors = typeCheckErrors("summon[scalanotation.Reader[Any]]")
    assert(errors.nonEmpty)

  test("no decoder is derived for Vector[Any]"):
    val errors =
      typeCheckErrors("summon[scalanotation.Reader[Vector[Any]]]")
    assert(errors.nonEmpty)

  test("no decoder is derived for nested Option"):
    val errors = typeCheckErrors(
      "type Data = (x: Option[Option[Int]])\nsummon[scalanotation.Reader[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".x"))
    assert(
      clue(errors.head.message)
        .contains("Reader[Option[Option[?]]] is not supported")
    )

  test("no skippable case class reader is derived when every field is an Option"):
    val errors = typeCheckErrors(
      "final case class Data(x: Option[Int], y: Option[String])\ngiven scalanotation.Reader[Data] = scalanotation.Reader.skippable.derived"
    )

    assert(errors.nonEmpty)
    assert(
      clue(errors.head.message)
        .contains("Reader derivation for a product with only Option fields is not supported")
    )

  test("no read-writer is derived for nested Option"):
    val errors = typeCheckErrors(
      "type Data = (x: Option[Option[Int]])\nsummon[scalanotation.ReadWriter[Data]]"
    )

    assert(errors.nonEmpty)
    assert(clue(errors.head.message).contains(".x"))
    assert(
      clue(errors.head.message)
        .contains("ReadWriter[Option[Option[?]]] is not supported")
    )

  test("no skippable case class read-writer is derived when every field is an Option"):
    val errors = typeCheckErrors(
      "final case class Data(x: Option[Int], y: Option[String])\ngiven scalanotation.ReadWriter[Data] = scalanotation.ReadWriter.skippable.derived"
    )

    assert(errors.nonEmpty)
    assert(
      clue(errors.head.message)
        .contains("ReadWriter derivation for a product with only Option fields is not supported")
    )

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
