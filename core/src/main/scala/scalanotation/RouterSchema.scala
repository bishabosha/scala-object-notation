package scalanotation

import scalanotation.internal.RawSchema

object RouterSchema:
  final val Unsupported: Int = -1

  enum Construct:
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

  trait Read:
    def route(construct: Construct): Int =
      construct match
        case Construct.Record    => onRecord()
        case Construct.Tuple     => onTuple()
        case Construct.Vector    => onVector()
        case Construct.String    => onString()
        case Construct.Char      => onChar()
        case Construct.Int       => onInt()
        case Construct.Long      => onLong()
        case Construct.Float     => onFloat()
        case Construct.Double    => onDouble()
        case Construct.Boolean   => onBoolean()
        case Construct.Null      => onNull()
        case Construct.RawNumber => onRawNumber()

    def onRecord(): Int    = Unsupported
    def onTuple(): Int     = Unsupported
    def onVector(): Int    = Unsupported
    def onString(): Int    = Unsupported
    def onChar(): Int      = Unsupported
    def onInt(): Int       = Unsupported
    def onLong(): Int      = Unsupported
    def onFloat(): Int     = Unsupported
    def onDouble(): Int    = Unsupported
    def onBoolean(): Int   = Unsupported
    def onNull(): Int      = Unsupported
    def onRawNumber(): Int = Unsupported

  object Read:
    def apply(f: Construct => Int): Read = new:
      override def route(construct: Construct): Int = f(construct)

  trait Write[-A]:
    def caseIndex(value: A): Int

  object Write:
    def apply[A](f: A => Int): Write[A] = new:
      def caseIndex(value: A): Int = f(value)

  final case class ReadCase[A](name: String, reader: Reader[A])
  final case class WriteCase[A](name: String, writer: Writer[A])
  final case class Case[A](name: String, readWriter: ReadWriter[A])

  def reader[A](
      name: String,
      selfKind: String,
      numberMode: NumberMode = NumberMode.Bounded
  )(
      cases: Reader[A] => Iterable[ReadCase[A]],
      read: Read
  ): Reader[A] =
    lazy val schema: RawSchema =
      val self = Reader.fromSchema[A](RawSchema.Ref(name, () => schema))
      RawSchema.Router(
        name,
        selfKind,
        IArray.from(cases(self).iterator.map(c => RawSchema.RouterCase(c.name, c.reader.schema))),
        rawRead(read),
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
    lazy val schema: RawSchema =
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
      cases: ReadWriter[A] => Iterable[Case[A]],
      read: Read,
      write: Write[A]
  ): ReadWriter[A] =
    lazy val schema: RawSchema =
      val self = ReadWriter.fromSchema[A](RawSchema.Ref(name, () => schema))
      RawSchema.Router(
        name,
        selfKind,
        IArray.from(
          cases(self).iterator.map(c => RawSchema.RouterCase(c.name, c.readWriter.schema))
        ),
        rawRead(read),
        rawWrite(write),
        rawNumberMode(numberMode)
      )
    ReadWriter.fromSchema(schema)

  private[scalanotation] def rawRead(read: Read): RawSchema.RouterRead =
    new RawSchema.RouterRead:
      override def onRecord(): Int    = read.route(Construct.Record)
      override def onTuple(): Int     = read.route(Construct.Tuple)
      override def onVector(): Int    = read.route(Construct.Vector)
      override def onString(): Int    = read.route(Construct.String)
      override def onChar(): Int      = read.route(Construct.Char)
      override def onInt(): Int       = read.route(Construct.Int)
      override def onLong(): Int      = read.route(Construct.Long)
      override def onFloat(): Int     = read.route(Construct.Float)
      override def onDouble(): Int    = read.route(Construct.Double)
      override def onBoolean(): Int   = read.route(Construct.Boolean)
      override def onNull(): Int      = read.route(Construct.Null)
      override def onRawNumber(): Int = read.route(Construct.RawNumber)

  private[scalanotation] def rawWrite[A](write: Write[A]): RawSchema.RouterWrite =
    new RawSchema.RouterWrite:
      def caseIndex(value: Any): Int =
        write.caseIndex(value.asInstanceOf[A])

  private def rawNumberMode(numberMode: NumberMode): RawSchema.RouterNumberMode =
    numberMode match
      case NumberMode.Bounded => RawSchema.RouterNumberMode.Bounded
      case NumberMode.Raw     => RawSchema.RouterNumberMode.Raw
