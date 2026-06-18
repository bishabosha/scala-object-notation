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

  opaque type Index = Int

  object Index:
    private[scalanotation] def fromInt(value: Int): Index =
      value

    private[scalanotation] def toInt(index: Index): Int =
      index

  final class Router private[scalanotation] (
      record: Int,
      tuple: Int,
      vector: Int,
      string: Int,
      char: Int,
      int: Int,
      long: Int,
      float: Int,
      double: Int,
      boolean: Int,
      `null`: Int,
      rawNumber: Int,
      unsupported: Int
  ):
    def recordIndex: Index      = record
    def tupleIndex: Index       = tuple
    def vectorIndex: Index      = vector
    def stringIndex: Index      = string
    def charIndex: Index        = char
    def intIndex: Index         = int
    def longIndex: Index        = long
    def floatIndex: Index       = float
    def doubleIndex: Index      = double
    def booleanIndex: Index     = boolean
    def nullIndex: Index        = `null`
    def rawNumberIndex: Index   = rawNumber
    def unsupportedIndex: Index = unsupported

    def indexFor(construct: RouterConstruct): Index =
      construct match
        case RouterConstruct.Record    => recordIndex
        case RouterConstruct.Tuple     => tupleIndex
        case RouterConstruct.Vector    => vectorIndex
        case RouterConstruct.String    => stringIndex
        case RouterConstruct.Char      => charIndex
        case RouterConstruct.Int       => intIndex
        case RouterConstruct.Long      => longIndex
        case RouterConstruct.Float     => floatIndex
        case RouterConstruct.Double    => doubleIndex
        case RouterConstruct.Boolean   => booleanIndex
        case RouterConstruct.Null      => nullIndex
        case RouterConstruct.RawNumber => rawNumberIndex

  trait Write[-A]:
    def caseIndex(router: Router, value: A): Index

  object Write:
    def apply[A](f: (Router, A) => Index): Write[A] = new:
      def caseIndex(router: Router, value: A): Index =
        f(router, value)

  final case class ReadCase[A](name: String, reader: Reader[A])
  final case class WriteCase[A](name: String, writer: Writer[A])
  final case class Case[A](name: String, readWriter: ReadWriter[A])
  type ReadRoute[A]  = (RouterConstruct, ReadCase[A])
  type WriteRoute[A] = (RouterConstruct, WriteCase[A])
  type Route[A]      = (RouterConstruct, Case[A])

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
      cases: Writer[A] => Iterable[WriteRoute[A]],
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
