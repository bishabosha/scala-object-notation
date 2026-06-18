package scalanotation

import scalanotation.internal.RawSchema

object RouterSchema:
  enum RouterConstruct:
    case Record
    case Tuple
    case Vector
    case String
    case Char
    case Int
    case Long
    case Float
    case Double
    case Boolean
    case Null
    case RawNumber

  enum NumberMode:
    case Bounded
    case Raw

  trait Write[-A]:
    def caseIndex(value: A): Int

  object Write:
    def apply[A](f: A => Int): Write[A] = new:
      def caseIndex(value: A): Int = f(value)

  final case class ReadCase[A](name: String, reader: Reader[A])
  final case class WriteCase[A](name: String, writer: Writer[A])
  final case class Case[A](name: String, readWriter: ReadWriter[A])
  type ReadRoute[A] = (RouterConstruct, ReadCase[A])
  type Route[A]     = (RouterConstruct, Case[A])

  def reader[A](
      name: String,
      selfKind: String,
      numberMode: NumberMode = NumberMode.Bounded
  )(
      cases: Reader[A] => Iterable[ReadRoute[A]]
  ): Reader[A] =
    Reader.fromSchema(RawSchema.routerReader(name, selfKind, numberMode)(cases))

  def writer[A](
      name: String,
      selfKind: String
  )(
      cases: Writer[A] => Iterable[WriteCase[A]],
      write: Write[A]
  ): Writer[A] =
    Writer.fromSchema(RawSchema.routerWriter(name, selfKind)(cases, write))

  def readWriter[A](
      name: String,
      selfKind: String,
      numberMode: NumberMode = NumberMode.Bounded
  )(
      cases: ReadWriter[A] => Iterable[Route[A]],
      write: Write[A]
  ): ReadWriter[A] =
    ReadWriter.fromSchema(RawSchema.routerReadWriter(name, selfKind, numberMode)(cases, write))
