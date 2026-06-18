package scalanotation

import scalanotation.internal.RawSchema

object RouterSchema:
  final val Unsupported: Int = -1

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
    lazy val schema: RawSchema[A] =
      val self      = Reader.fromSchema[A](RawSchema.Ref(name, () => schema))
      val routes    = readRoutes(cases(self))
      val caseArray = routeCases(routes)
      RawSchema.Router(
        name,
        selfKind,
        caseArray,
        routeRead(routes),
        write = null,
        rawNumberMode(numberMode)
      )
    Reader.fromSchema(schema)

  def writer[A](
      name: String,
      selfKind: String
  )(
      cases: Writer[A] => Iterable[WriteCase[A]],
      write: Write[A]
  ): Writer[A] =
    lazy val schema: RawSchema[A] =
      val self = Writer.fromSchema[A](RawSchema.Ref(name, () => schema))
      RawSchema.Router(
        name,
        selfKind,
        IArray.from(cases(self).iterator.map(c => RawSchema.RouterCase(c.name, c.writer.schema))),
        read = null,
        rawWrite(write),
        RawSchema.RouterNumberMode.Bounded
      )
    Writer.fromSchema(schema)

  def readWriter[A](
      name: String,
      selfKind: String,
      numberMode: NumberMode = NumberMode.Bounded
  )(
      cases: ReadWriter[A] => Iterable[Route[A]],
      write: Write[A]
  ): ReadWriter[A] =
    lazy val schema: RawSchema[A] =
      val self      = ReadWriter.fromSchema[A](RawSchema.Ref(name, () => schema))
      val routes    = routesFrom(cases(self))
      val caseArray = routeCases(routes)
      RawSchema.Router(
        name,
        selfKind,
        caseArray,
        routeRead(routes),
        rawWrite(write),
        rawNumberMode(numberMode)
      )
    ReadWriter.fromSchema(schema)

  private[scalanotation] def rawWrite[A](write: Write[A]): RawSchema.RouterWrite =
    new RawSchema.RouterWrite:
      def caseIndex(value: Any): Int =
        write.caseIndex(value.asInstanceOf[A])

  private def readRoutes[A](
      routes: Iterable[ReadRoute[A]]
  ): IArray[(RouterConstruct, RawSchema.RouterCase[A])] =
    IArray.from(
      routes.iterator.map { case (construct, c) =>
        construct -> RawSchema.RouterCase(c.name, c.reader.schema)
      }
    )

  private def routesFrom[A](
      routes: Iterable[Route[A]]
  ): IArray[(RouterConstruct, RawSchema.RouterCase[A])] =
    IArray.from(
      routes.iterator.map { case (construct, c) =>
        construct -> RawSchema.RouterCase(c.name, c.readWriter.schema)
      }
    )

  private def routeCases[A](
      routes: IArray[(RouterConstruct, RawSchema.RouterCase[A])]
  ): IArray[RawSchema.RouterCase[A]] =
    IArray.from(routes.iterator.map(_._2))

  private def routeRead[A](
      routes: IArray[(RouterConstruct, RawSchema.RouterCase[A])]
  ): RawSchema.RouterRead =
    var recordCase    = Unsupported
    var tupleCase     = Unsupported
    var vectorCase    = Unsupported
    var stringCase    = Unsupported
    var charCase      = Unsupported
    var intCase       = Unsupported
    var longCase      = Unsupported
    var floatCase     = Unsupported
    var doubleCase    = Unsupported
    var booleanCase   = Unsupported
    var nullCase      = Unsupported
    var rawNumberCase = Unsupported

    var index = 0
    while index < routes.length do
      routes(index)._1 match
        case RouterConstruct.Record =>
          recordCase = index
        case RouterConstruct.Tuple =>
          tupleCase = index
        case RouterConstruct.Vector =>
          vectorCase = index
        case RouterConstruct.String =>
          stringCase = index
        case RouterConstruct.Char =>
          charCase = index
        case RouterConstruct.Int =>
          intCase = index
        case RouterConstruct.Long =>
          longCase = index
        case RouterConstruct.Float =>
          floatCase = index
        case RouterConstruct.Double =>
          doubleCase = index
        case RouterConstruct.Boolean =>
          booleanCase = index
        case RouterConstruct.Null =>
          nullCase = index
        case RouterConstruct.RawNumber =>
          rawNumberCase = index
      index += 1

    new RawSchema.RouterRead:
      private val recordCase0: Int    = recordCase
      private val tupleCase0: Int     = tupleCase
      private val vectorCase0: Int    = vectorCase
      private val stringCase0: Int    = stringCase
      private val charCase0: Int      = charCase
      private val intCase0: Int       = intCase
      private val longCase0: Int      = longCase
      private val floatCase0: Int     = floatCase
      private val doubleCase0: Int    = doubleCase
      private val booleanCase0: Int   = booleanCase
      private val nullCase0: Int      = nullCase
      private val rawNumberCase0: Int = rawNumberCase

      override def onRecord(): Int    = recordCase0
      override def onTuple(): Int     = tupleCase0
      override def onVector(): Int    = vectorCase0
      override def onString(): Int    = stringCase0
      override def onChar(): Int      = charCase0
      override def onInt(): Int       = intCase0
      override def onLong(): Int      = longCase0
      override def onFloat(): Int     = floatCase0
      override def onDouble(): Int    = doubleCase0
      override def onBoolean(): Int   = booleanCase0
      override def onNull(): Int      = nullCase0
      override def onRawNumber(): Int = rawNumberCase0

  private def rawNumberMode(numberMode: NumberMode): RawSchema.RouterNumberMode =
    numberMode match
      case NumberMode.Bounded => RawSchema.RouterNumberMode.Bounded
      case NumberMode.Raw     => RawSchema.RouterNumberMode.Raw
