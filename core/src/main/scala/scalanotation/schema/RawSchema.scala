package scalanotation.schema

import scalanotation.DecodeError
import scalanotation.Expr
import scalanotation.Reader
import scalanotation.ReadWriter
import scalanotation.RouterSchema
import scalanotation.TypedFactory
import scalanotation.Writer
import scalanotation.internal.PublicInternal
import steps.result.Result

import scala.collection.mutable.ArrayBuffer
import Result.eval.check
import Result.eval.raise

/** Schema carried by scalanotation type classes. It describes expected structure, builtin primitive
  * schemas, and mapped schema adapters used to read and write typed values.
  */
enum RawSchema[A]:
  case NamedTuple[A](
      fields: IArray[RawSchema.Field],
      read: RawSchema.NamedTupleRead | Null = null,
      write: RawSchema.NamedTupleWrite | Null = null,
      allowSkippedNullableFields: Boolean = false
  ) extends RawSchema[A]
  case Tuple[A](
      slots: IArray[RawSchema[?]],
      read: RawSchema.TupleRead | Null = null,
      write: RawSchema.TupleWrite | Null = null
  )                                                                       extends RawSchema[A]
  case PartialNamedTuple[A](base: RawSchema[A], alreadySeenField: String) extends RawSchema[A]
  case Sum[A](
      cases: IArray[RawSchema.SumCase],
      write: RawSchema.SumWrite | Null = null
  ) extends RawSchema[A]
  case DiscriminatorSum[A](
      cases: IArray[RawSchema.SumCase],
      write: RawSchema.SumWrite | Null,
      discriminatorField: String
  ) extends RawSchema[A]
  case Vector[A, Elem](
      element: RawSchema[Elem],
      read: RawSchema.VectorRead | Null = null,
      write: RawSchema.VectorWrite | Null = null
  ) extends RawSchema[A] with RawSchema.Collection
  case TupleOf[A, Elem](
      element: RawSchema[Elem],
      read: RawSchema.VectorRead | Null = null,
      write: RawSchema.VectorWrite | Null = null
  ) extends RawSchema[A] with RawSchema.Collection
  case PairSeq[A, K, V](
      key: RawSchema[K],
      value: RawSchema[V],
      read: RawSchema.PairSeqRead | Null = null,
      write: RawSchema.PairSeqWrite | Null = null
  ) extends RawSchema[A] with RawSchema.Collection
  case Dict[A, Elem](
      element: RawSchema[Elem],
      read: RawSchema.DictRead | Null = null,
      write: RawSchema.DictWrite | Null = null
  ) extends RawSchema[A] with RawSchema.Collection
  case Router[A](
      name: String,
      selfKind: String,
      cases: IArray[RawSchema.RouterCase[A]],
      router: RouterSchema.Router,
      write: RouterSchema.Write[A] | Null,
      numberMode: RouterSchema.NumberMode
  )                                                     extends RawSchema[A]
  case Ref[A](name: String, target: () => RawSchema[A]) extends RawSchema[A] with RawSchema.Atomic
  case Option[A](inner: RawSchema[A]) extends RawSchema[scala.Option[A]] with RawSchema.Collection
  case Mapped[Base, A](
      base: RawSchema[Base],
      mapping: SchemaMapping[Base, A]
  )            extends RawSchema[A] with RawSchema.Atomic
  case String  extends RawSchema[scala.Predef.String] with RawSchema.Atomic
  case Char    extends RawSchema[scala.Char] with RawSchema.Atomic
  case Int     extends RawSchema[scala.Int] with RawSchema.Atomic
  case Long    extends RawSchema[scala.Long] with RawSchema.Atomic
  case Float   extends RawSchema[scala.Float] with RawSchema.Atomic
  case Double  extends RawSchema[scala.Double] with RawSchema.Atomic
  case Boolean extends RawSchema[scala.Boolean] with RawSchema.Atomic
  case Null    extends RawSchema[scala.Null] with RawSchema.Atomic

  private[scalanotation] final def withMapping(
      f: [Base] => SchemaMapping[Base, A] => SchemaMapping[Base, A]
  ): RawSchema[A] =
    this match
      case RawSchema.Mapped(base, mapping0) => RawSchema.Mapped(base, f(mapping0))
      case _                                => RawSchema.Mapped(this, f(SchemaMapping.empty[A]))

  private lazy val properties: java.util.concurrent.ConcurrentHashMap[RawSchema.Key[?], AnyRef] =
    java.util.concurrent.ConcurrentHashMap()

  private def getOrComputeProperty[T <: AnyRef](key: RawSchema.Key[T])(
      compute: => T
  ): T =
    properties.computeIfAbsent(key, _ => compute).asInstanceOf[T]

  /** Installs decode-time default values, parallel to this [[RawSchema.NamedTuple]]'s fields (null =
    * the field has no default). [[Configured]] calls this on a freshly copied node, so shared
    * schema instances are never mutated; the defaults must be installed before the first decode and
    * are mutually exclusive with `allowSkippedNullableFields`.
    */
  private[scalanotation] def installFieldDefaults(defaults: IArray[AnyRef | Null]): Unit =
    properties.put(RawSchema.FieldDefaults, defaults.asInstanceOf[AnyRef])

  private[scalanotation] def installedFieldDefaults: IArray[AnyRef | Null] | Null =
    properties.get(RawSchema.FieldDefaults) match
      case null  => null
      case value => value.asInstanceOf[IArray[AnyRef | Null]]

  // Cached named-tuple validation: checked once per decoded record, so it must be a plain volatile
  // read rather than a properties-map lookup. Validation is idempotent — a racing recompute stores
  // the same value.
  @volatile private var isValidNamedTupleCache: Result[Unit, DecodeError] | Null = null

  // Per-field decode plan: whether the decoder may match the name by char-level slice comparison
  // (decided once per schema by running the real scanner over each name — parity by construction)
  // and which primitive decoder the field value dispatches to directly. Plain volatile read;
  // idempotent under races.
  @volatile private var fieldPlansCache: RawSchema.FieldPlans | Null = null

  private[scalanotation] def fieldPlans: RawSchema.FieldPlans =
    val cached = fieldPlansCache
    if cached != null then cached
    else
      val computed = this match
        case namedTuple: RawSchema.NamedTuple[?] =>
          val fields    = namedTuple.fields
          val kinds     = new Array[scala.Byte](fields.length)
          val nameChars = new Array[Array[scala.Char]](fields.length)
          val nullable  = new Array[scala.Boolean](fields.length)
          var index     = 0
          while index < fields.length do
            val field = fields(index)
            kinds(index) =
              if scalanotation.internal.Tokenizer.isPlainFieldName(field.name) then
                RawSchema.valuePlanOf(field.schema)
              else RawSchema.FieldPlan.TokenName
            nameChars(index) = field.name.toCharArray
            nullable(index) = scalanotation.internal.TokenDecoder.isNullable(field.schema)
            index += 1
          val defaults                           = installedFieldDefaults
          val fills: Array[AnyRef | Null] | Null =
            if defaults != null then Array.tabulate[AnyRef | Null](fields.length)(defaults.nn.apply)
            else if namedTuple.allowSkippedNullableFields then
              Array.tabulate[AnyRef | Null](fields.length)(i => if nullable(i) then None else null)
            else null
          RawSchema.FieldPlans(kinds, nameChars, nullable, fills)
        case sum: RawSchema.Sum[?] =>
          val cases     = sum.cases
          val kinds     = new Array[scala.Byte](cases.length)
          val nameChars = new Array[Array[scala.Char]](cases.length)
          val nullable  = new Array[scala.Boolean](cases.length)
          var index     = 0
          while index < cases.length do
            val sumCase = cases(index)
            kinds(index) =
              if scalanotation.internal.Tokenizer.isPlainFieldName(sumCase.name) then
                RawSchema.valuePlanOf(sumCase.schema)
              else RawSchema.FieldPlan.TokenName
            nameChars(index) = sumCase.name.toCharArray
            nullable(index) = scalanotation.internal.TokenDecoder.isNullable(sumCase.schema)
            index += 1
          RawSchema.FieldPlans(kinds, nameChars, nullable, null)
        case sum: RawSchema.DiscriminatorSum[?] =>
          // entry 0 is the discriminator header (its value is always a string); entries 1..n
          // carry the case names' chars so the header decode slice-matches the discriminator
          // value without materializing it (their kind/nullable slots are unused: the payload
          // dispatches through decodeBase)
          val name             = sum.discriminatorField
          val kind: scala.Byte =
            if scalanotation.internal.Tokenizer.isPlainFieldName(name) then
              RawSchema.FieldPlan.StringV
            else RawSchema.FieldPlan.TokenName
          val cases     = sum.cases
          val kinds     = new Array[scala.Byte](cases.length + 1)
          val nameChars = new Array[Array[scala.Char]](cases.length + 1)
          val nullable  = new Array[scala.Boolean](cases.length + 1)
          kinds(0) = kind
          nameChars(0) = name.toCharArray
          var index = 0
          while index < cases.length do
            kinds(index + 1) = RawSchema.FieldPlan.Other
            nameChars(index + 1) = cases(index).name.toCharArray
            index += 1
          RawSchema.FieldPlans(kinds, nameChars, nullable, null)
        case _ => RawSchema.FieldPlans.Empty
      fieldPlansCache = computed
      computed

  private[scalanotation] def isValidNamedTuple[T: PublicInternal.NameSet](
      pool: PublicInternal.Pool[T]
  ): Result[Unit, DecodeError] =
    val cached = isValidNamedTupleCache
    if cached != null then cached
    else
      val computed = validateNamedTuple(pool)
      isValidNamedTupleCache = computed
      computed

  private def validateNamedTuple[T: PublicInternal.NameSet](
      pool: PublicInternal.Pool[T]
  ): Result[Unit, DecodeError] = pool.withBorrowed { seenNames =>
    Result.task:
      this match
        case namedTuple: RawSchema.NamedTuple[?] =>
          val fields = namedTuple.fields
          val len    = fields.length
          var i      = 0
          while i < len do
            val field                    = fields(i)
            val name                     = field.name
            def fmtErr(err: DecodeError) = err.atPath(s".${name}")
            if seenNames.alreadySeen(name) then
              raise(fmtErr(DecodeError.DuplicateSchemaField(name)))
            i += 1
        case RawSchema.Mapped(base, _) =>
          base.validateNamedTuple(pool).check
        case RawSchema.Ref(_, target) =>
          target().validateNamedTuple(pool).check
        case _ => ()
  }

  final def describeSelf: String =
    this match
      case namedTuple: RawSchema.NamedTuple[?] =>
        val fields = namedTuple.fields
        if fields.isEmpty then "AnyNamedTuple"
        else fields.map(f => s"${f.name}: ...").mkString("(", ", ", ")")
      case partial: RawSchema.PartialNamedTuple[?] =>
        partial.base.describeSelf
      case tuple: RawSchema.Tuple[?] =>
        RawSchema.describeTupleSlots(tuple.slots.length)
      case sum: RawSchema.Sum[?] =>
        val cases = sum.cases
        if cases.isEmpty then "AnyNamedTuple"
        else cases.iterator.map(k => s"(${k.name}: ...)").mkString(" | ")
      case sum: RawSchema.DiscriminatorSum[?] =>
        val cases = sum.cases
        if cases.isEmpty then "AnyNamedTuple"
        else
          val field = sum.discriminatorField
          cases.iterator.map(k => s"""($field: "${k.name}", ...)""").mkString(" | ")
      case _: RawSchema.Vector[?, ?]     => "Vector[...]"
      case _: RawSchema.TupleOf[?, ?]    => "Tuple"
      case _: RawSchema.PairSeq[?, ?, ?] => "Vector[(..., ...)]"
      case _: RawSchema.Dict[?, ?]       => "AnyNamedTuple"
      case router: RawSchema.Router[?]   => router.selfKind
      case RawSchema.Ref(name, _)        => name
      case RawSchema.String              => "String"
      case RawSchema.Char                => "Char"
      case RawSchema.Int                 => "Int"
      case RawSchema.Long                => "Long"
      case RawSchema.Float               => "Float"
      case RawSchema.Double              => "Double"
      case RawSchema.Boolean             => "Boolean"
      case RawSchema.Null                => "Null"
      case option: RawSchema.Option[?]   =>
        option.inner match
          case _: RawSchema.Option[?] => "... | Null"
          case other                  => s"${other.describeSelf} | Null"
      case RawSchema.Mapped(base, _) =>
        base.describeSelf

object RawSchema:
  sealed trait Atomic     { self: RawSchema[?] => }
  sealed trait Collection { self: RawSchema[?] => }

  type ResultMap[-In, +Out]               = In => Result[Out, DecodeError]
  type InputMap[-In, +Out]                = In => Out
  private[scalanotation] type TupleRead   = Reader.TupleBuilder[?, ?]
  private[scalanotation] type VectorRead  = Reader.VectorBuilder[?, ?, ?]
  private[scalanotation] type DictRead    = Reader.DictBuilder[?, ?, ?]
  private[scalanotation] type PairSeqRead = Reader.PairSeqBuilder[?, ?, ?, ?]

  private[scalanotation] inline val UnsupportedRouterCase = -1

  /** Flat per-field decode plan: [[FieldPlan]] codes, the field names as char arrays (plain array
    * loads in the header compare, no per-char String machinery), and precomputed nullability so
    * skippable-field resolution never re-derives it from the schema.
    */
  private[scalanotation] final class FieldPlans(
      val kinds: Array[scala.Byte],
      val nameChars: Array[Array[scala.Char]],
      val nullable: Array[scala.Boolean],
      /** Per-field decode-time fill values, or null when fields may not be omitted. In skippable
        * mode every nullable field fills with `None`; in defaults mode a field fills with its
        * installed default. One array serves the whole record loop, so the two modes share every
        * walk and fill site.
        */
      val fills: Array[AnyRef | Null] | Null
  )

  private[scalanotation] object FieldPlans:
    val Empty: FieldPlans = FieldPlans(Array.emptyByteArray, Array.empty, Array.empty, null)

  /** entry values of [[fieldPlans]]: how a field's name is matched and its value dispatched */
  private[scalanotation] object FieldPlan:
    inline val TokenName = 0  // name not slice-matchable: the whole field goes the token path
    inline val Other     = 1  // plain name; value through the general dispatcher
    inline val IntV      = 2
    inline val LongV     = 3
    inline val DoubleV   = 4
    inline val FloatV    = 5
    inline val BooleanV  = 6
    inline val StringV   = 7
    inline val CharV     = 8
    inline val RecordV   = 9  // a named tuple: dispatches straight to the record decoder
    inline val VectorV   = 10 // a vector: dispatches straight to the vector decoder
    inline val OptionV   = 11 // an option: dispatches straight to the option decoder

  /** the [[FieldPlan]] dispatch code for a value of `schema` (independent of any field name) */
  private[scalanotation] def valuePlanOf(schema: RawSchema[?]): scala.Byte =
    schema match
      case RawSchema.Int              => FieldPlan.IntV
      case RawSchema.Long             => FieldPlan.LongV
      case RawSchema.Double           => FieldPlan.DoubleV
      case RawSchema.Float            => FieldPlan.FloatV
      case RawSchema.Boolean          => FieldPlan.BooleanV
      case RawSchema.String           => FieldPlan.StringV
      case RawSchema.Char             => FieldPlan.CharV
      case _: RawSchema.NamedTuple[?] => FieldPlan.RecordV
      case _: RawSchema.Vector[?, ?]  => FieldPlan.VectorV
      case _: RawSchema.Option[?]     => FieldPlan.OptionV
      case _                          => FieldPlan.Other

  def describeTupleSlots(size: Int): String =
    size match
      case 0 => "EmptyTuple"
      case 1 => "Tuple(...)"
      case _ => Iterator.fill(size)("...").mkString("(", ", ", ")")

  final class Key[T]()
  private val SumCaseLookup: Key[Map[String, SumCase]] = Key()

  /** property carrying a [[NamedTuple]]'s decode-time field defaults — see installFieldDefaults */
  private val FieldDefaults: Key[AnyRef] = Key()

  final case class Field(name: String, schema: RawSchema[?])
  final case class SumCase(name: String, schema: RawSchema[?])
  final case class RouterCase[A](name: String, schema: RawSchema[A])

  def routerCase[A](
      schema: RawSchema.Router[A],
      index: RouterSchema.Index
  ): RouterCase[A] | Null =
    val rawIndex = RouterSchema.Index.toInt(index)
    if rawIndex < 0 || rawIndex >= schema.cases.length then null
    else schema.cases(rawIndex)

  def findCase[A](cases: RawSchema.Sum[A] | RawSchema.DiscriminatorSum[A], name: String): SumCase |
    Null =
    val caseMap = cases.getOrComputeProperty(SumCaseLookup) {
      cases match
        case sum: RawSchema.Sum[?] =>
          sum.cases.iterator.map(c => c.name -> c).toMap
        case sum: RawSchema.DiscriminatorSum[?] =>
          sum.cases.iterator.map(c => c.name -> c).toMap
    }
    caseMap.getOrElse(name, null)
    // var i = 0
    // while i < cases.length do
    //   val sumCase = cases(i)
    //   if sumCase.name == name then return sumCase
    //   i += 1
    // null

  private[scalanotation] def routerReader[A](
      name: String,
      selfKind: String,
      numberMode: RouterSchema.NumberMode
  )(
      cases: Reader[A] => Iterable[RouterSchema.ReadRoute[A]]
  ): RawSchema[A] =
    lazy val schema: RawSchema[A] =
      val self      = Reader.fromSchema[A](RawSchema.Ref(name, () => schema))
      val routes    = readRoutes(cases(self))
      val caseArray = routeCases(routes)
      val router    = routeRouter(routes)
      RawSchema.Router(
        name,
        selfKind,
        caseArray,
        router,
        write = null,
        numberMode
      )
    schema

  private[scalanotation] def routerWriter[A](
      name: String,
      selfKind: String
  )(
      cases: Writer[A] => Iterable[RouterSchema.WriteRoute[A]],
      write: RouterSchema.Write[A]
  ): RawSchema[A] =
    lazy val schema: RawSchema[A] =
      val self      = Writer.fromSchema[A](RawSchema.Ref(name, () => schema))
      val routes    = writeRoutes(cases(self))
      val caseArray = routeCases(routes)
      RawSchema.Router(
        name,
        selfKind,
        caseArray,
        routeRouter(routes),
        write,
        RouterSchema.NumberMode.Bounded
      )
    schema

  private[scalanotation] def routerReadWriter[A](
      name: String,
      selfKind: String,
      numberMode: RouterSchema.NumberMode
  )(
      cases: ReadWriter[A] => Iterable[RouterSchema.Route[A]],
      write: RouterSchema.Write[A]
  ): RawSchema[A] =
    lazy val schema: RawSchema[A] =
      val self      = ReadWriter.fromSchema[A](RawSchema.Ref(name, () => schema))
      val routes    = readWriterRoutes(cases(self))
      val caseArray = routeCases(routes)
      val router    = routeRouter(routes)
      RawSchema.Router(
        name,
        selfKind,
        caseArray,
        router,
        write,
        numberMode
      )
    schema

  private type RouterRoute[A] = (RouterSchema.RouterConstruct, RawSchema.RouterCase[A])

  private def readRoutes[A](
      routes: Iterable[RouterSchema.ReadRoute[A]]
  ): IArray[RouterRoute[A]] =
    IArray.from(
      routes.iterator.map { case (construct, c) =>
        construct -> RawSchema.RouterCase(c.name, c.reader.schema)
      }
    )

  private def readWriterRoutes[A](
      routes: Iterable[RouterSchema.Route[A]]
  ): IArray[RouterRoute[A]] =
    IArray.from(
      routes.iterator.map { case (construct, c) =>
        construct -> RawSchema.RouterCase(c.name, c.readWriter.schema)
      }
    )

  private def writeRoutes[A](
      routes: Iterable[RouterSchema.WriteRoute[A]]
  ): IArray[RouterRoute[A]] =
    IArray.from(
      routes.iterator.map { case (construct, c) =>
        construct -> RawSchema.RouterCase(c.name, c.writer.schema)
      }
    )

  private def routeCases[A](
      routes: IArray[RouterRoute[A]]
  ): IArray[RawSchema.RouterCase[A]] =
    IArray.from(routes.iterator.map(_._2))

  private def routeRouter[A](
      routes: IArray[RouterRoute[A]]
  ): RouterSchema.Router =
    import RouterSchema.RouterConstruct

    var recordCase    = UnsupportedRouterCase
    var tupleCase     = UnsupportedRouterCase
    var vectorCase    = UnsupportedRouterCase
    var stringCase    = UnsupportedRouterCase
    var charCase      = UnsupportedRouterCase
    var intCase       = UnsupportedRouterCase
    var longCase      = UnsupportedRouterCase
    var floatCase     = UnsupportedRouterCase
    var doubleCase    = UnsupportedRouterCase
    var booleanCase   = UnsupportedRouterCase
    var nullCase      = UnsupportedRouterCase
    var rawNumberCase = UnsupportedRouterCase

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

    RouterSchema.Router(
      recordCase,
      tupleCase,
      vectorCase,
      stringCase,
      charCase,
      intCase,
      longCase,
      floatCase,
      doubleCase,
      booleanCase,
      nullCase,
      rawNumberCase,
      UnsupportedRouterCase
    )

  trait NamedTupleRead:
    /** Abstract builder state, consumed and returned by the `add` methods. Implementations that
      * only provide the legacy [[build]] inherit the defaults, where the state is the
      * `Array[AnyRef]` of boxed field values handed to [[build]] by [[finish]].
      */
    type State

    /** legacy entry point: builds the result from boxed field values */
    def build(values: Array[AnyRef]): Any

    def init(size: Int, slots: scalanotation.BuilderSlots | Null): State =
      if slots != null && slotsFactory != null then slots.reset(size).asInstanceOf[State]
      else (new Array[AnyRef](size)).asInstanceOf[State]

    def add(state: State, index: Int, value: Any): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setRef(index, value)
      state

    // typed adds: the defaults box and delegate to `add`; specialized states override to
    // consume the primitive directly
    def addString(state: State, index: Int, value: String): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setString(index, value)
      state

    def addChar(state: State, index: Int, value: Char): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setChar(index, value)
      state
    def addInt(state: State, index: Int, value: Int): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setInt(index, value)
      state
    def addLong(state: State, index: Int, value: Long): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setLong(index, value)
      state
    def addFloat(state: State, index: Int, value: Float): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setFloat(index, value)
      state
    def addDouble(state: State, index: Int, value: Double): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setDouble(index, value)
      state
    def addBoolean(state: State, index: Int, value: Boolean): State =
      state match
        case arr: Array[AnyRef] =>
          arr(index) = value.asInstanceOf[AnyRef]
        case slots: scalanotation.BuilderSlots =>
          slots.setBoolean(index, value)
      state

    def finish(state: State): Any = state match
      case arr: Array[AnyRef]                => build(arr)
      case slots: scalanotation.BuilderSlots =>
        slotsFactory.nn.fromSlots(slots)

    /** Optional low-boxing finalizer: when non-null, a decoder may fill its pooled
      * [[scalanotation.BuilderSlots]] with typed field values instead of threading [[State]], and
      * finalize via [[scalanotation.TypedFactory.OfProduct.fromSlots]].
      */
    def slotsFactory: TypedFactory.OfProduct[?] | Null = null

  object NamedTupleRead:
    def from[T](build0: Array[AnyRef] => T): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any = build0(values)

    def from[T](
        build0: Array[AnyRef] => T,
        slotsFactory0: TypedFactory.OfProduct[?] | Null
    ): NamedTupleRead = new:
      def build(values: Array[AnyRef]): Any                       = build0(values)
      override def slotsFactory: TypedFactory.OfProduct[?] | Null = slotsFactory0

    /** attaches (or replaces) a [[scalanotation.TypedFactory]] on an existing read */
    def withSlotsFactory(read: NamedTupleRead, factory: TypedFactory.OfProduct[?]): NamedTupleRead =
      new:
        def build(values: Array[AnyRef]): Any                       = read.build(values)
        override def slotsFactory: TypedFactory.OfProduct[?] | Null = factory

  trait NamedTupleWrite:
    def fieldValue(value: Any, index: Int): Any

    // Typed field accessors — the write-side dual of the decoder's typed slots: precise
    // signatures that let the renderer pull a primitive field without boxing. The defaults
    // delegate to fieldValue (matching previous behaviour); overrides — e.g. the evidence
    // derived by `TypedFactories.derived` in the macros module and attached via
    // `Configured.typed` — read the field straight off the product.
    def stringFieldValue(value: Any, index: Int): String =
      fieldValue(value, index).asInstanceOf[String]
    def charFieldValue(value: Any, index: Int): Char =
      fieldValue(value, index).asInstanceOf[Char]
    def intFieldValue(value: Any, index: Int): Int =
      fieldValue(value, index).asInstanceOf[Int]
    def longFieldValue(value: Any, index: Int): Long =
      fieldValue(value, index).asInstanceOf[Long]
    def floatFieldValue(value: Any, index: Int): Float =
      fieldValue(value, index).asInstanceOf[Float]
    def doubleFieldValue(value: Any, index: Int): Double =
      fieldValue(value, index).asInstanceOf[Double]
    def booleanFieldValue(value: Any, index: Int): Boolean =
      fieldValue(value, index).asInstanceOf[Boolean]

  object NamedTupleWrite:
    val productLike: NamedTupleWrite = new:
      def fieldValue(value: Any, index: Int): Any =
        value.asInstanceOf[Product].productElement(index)

    val singleton: NamedTupleWrite = new:
      def fieldValue(value: Any, index: Int): Any = ()

    /** overlays the typed field accessors of `factory` onto an existing write */
    private[scalanotation] def withTypedFieldValues(
        write: NamedTupleWrite,
        factory: TypedFactory.OfProduct[?]
    ): NamedTupleWrite = new:
      def fieldValue(value: Any, index: Int): Any                   = write.fieldValue(value, index)
      override def stringFieldValue(value: Any, index: Int): String =
        factory.stringFieldValue(value, index)
      override def charFieldValue(value: Any, index: Int): Char =
        factory.charFieldValue(value, index)
      override def intFieldValue(value: Any, index: Int): Int =
        factory.intFieldValue(value, index)
      override def longFieldValue(value: Any, index: Int): Long =
        factory.longFieldValue(value, index)
      override def floatFieldValue(value: Any, index: Int): Float =
        factory.floatFieldValue(value, index)
      override def doubleFieldValue(value: Any, index: Int): Double =
        factory.doubleFieldValue(value, index)
      override def booleanFieldValue(value: Any, index: Int): Boolean =
        factory.booleanFieldValue(value, index)

  trait TupleWrite:
    def size(value: Any): Int
    def elementValue(value: Any, index: Int): Any

    // typed element accessors — see [[NamedTupleWrite]]; the defaults delegate to elementValue.
    // Runtime tuples store their elements boxed, so these only pay off for custom tuple-like
    // writes over unboxed storage.
    def stringElementValue(value: Any, index: Int): String =
      elementValue(value, index).asInstanceOf[String]
    def charElementValue(value: Any, index: Int): Char =
      elementValue(value, index).asInstanceOf[Char]
    def intElementValue(value: Any, index: Int): Int =
      elementValue(value, index).asInstanceOf[Int]
    def longElementValue(value: Any, index: Int): Long =
      elementValue(value, index).asInstanceOf[Long]
    def floatElementValue(value: Any, index: Int): Float =
      elementValue(value, index).asInstanceOf[Float]
    def doubleElementValue(value: Any, index: Int): Double =
      elementValue(value, index).asInstanceOf[Double]
    def booleanElementValue(value: Any, index: Int): Boolean =
      elementValue(value, index).asInstanceOf[Boolean]

  object TupleWrite:
    val productLike: TupleWrite = new:
      def size(value: Any): Int =
        value.asInstanceOf[Product].productArity

      def elementValue(value: Any, index: Int): Any =
        value.asInstanceOf[Product].productElement(index)

    def from[A](size0: A => Int, elementValue0: (A, Int) => Any): TupleWrite = new:
      def size(value: Any): Int =
        size0(value.asInstanceOf[A])

      def elementValue(value: Any, index: Int): Any =
        elementValue0(value.asInstanceOf[A], index)

  trait SumWrite:
    def caseIndex(value: Any): Int

  object SumWrite:
    def from[T](select: T => Int): SumWrite = new:
      def caseIndex(value: Any): Int = select(value.asInstanceOf[T])

  trait VectorWrite:
    def size(value: Any): Int
    def iterator(value: Any): Iterator[Any]

  /** A [[VectorWrite]] with random access. The renderer prefers the indexed accessors over
    * [[VectorWrite.iterator]], pulling primitive elements — e.g. from an `Array[Int]` — without
    * boxing. The typed accessor defaults delegate to [[elementValue]].
    */
  trait IndexedVectorWrite extends VectorWrite:
    def elementValue(value: Any, index: Int): Any

    def stringElementValue(value: Any, index: Int): String =
      elementValue(value, index).asInstanceOf[String]
    def charElementValue(value: Any, index: Int): Char =
      elementValue(value, index).asInstanceOf[Char]
    def intElementValue(value: Any, index: Int): Int =
      elementValue(value, index).asInstanceOf[Int]
    def longElementValue(value: Any, index: Int): Long =
      elementValue(value, index).asInstanceOf[Long]
    def floatElementValue(value: Any, index: Int): Float =
      elementValue(value, index).asInstanceOf[Float]
    def doubleElementValue(value: Any, index: Int): Double =
      elementValue(value, index).asInstanceOf[Double]
    def booleanElementValue(value: Any, index: Int): Boolean =
      elementValue(value, index).asInstanceOf[Boolean]

  object VectorWrite:
    def from[A, Elem](size0: A => Int, iterator0: A => Iterator[Elem]): VectorWrite = new:
      def size(value: Any): Int = size0(value.asInstanceOf[A])

      def iterator(value: Any): Iterator[Any] =
        iterator0(value.asInstanceOf[A]).asInstanceOf[Iterator[Any]]

  trait PairSeqWrite:
    def size(value: Any): Int
    def iterator(value: Any): Iterator[(Any, Any)]

  object PairSeqWrite:
    def from[A, Key, Elem](size0: A => Int, iterator0: A => Iterator[(Key, Elem)]): PairSeqWrite =
      new:
        def size(value: Any): Int = size0(value.asInstanceOf[A])

        def iterator(value: Any): Iterator[(Any, Any)] =
          iterator0(value.asInstanceOf[A]).map((key, elem) => key -> elem.asInstanceOf[Any])

  trait DictWrite:
    def size(value: Any): Int
    def iterator(value: Any): Iterator[(String, Any)]

  object DictWrite:
    def from[A, Elem](size0: A => Int, iterator0: A => Iterator[(String, Elem)]): DictWrite = new:
      def size(value: Any): Int = size0(value.asInstanceOf[A])

      def iterator(value: Any): Iterator[(String, Any)] =
        iterator0(value.asInstanceOf[A]).map((key, elem) => key -> elem.asInstanceOf[Any])

  def mapResult[A, B](
      base: RawSchema[A]
  )(
      f: ResultMap[A, B]
  ): RawSchema[B] =
    base match
      case RawSchema.Mapped(base0, mapping) => RawSchema.Mapped(base0, mapping.withResultMap(f))
      case _ => RawSchema.Mapped(base, SchemaMapping[A, A]().withResultMap(f))

  def mapInput[A, B](
      base: RawSchema[A]
  )(
      f: InputMap[B, A]
  ): RawSchema[B] =
    base match
      case RawSchema.Mapped(base0, mapping) => RawSchema.Mapped(base0, mapping.withInputMap(f))
      case _ => RawSchema.Mapped(base, SchemaMapping[A, A]().withInputMap(f))

  def mapPure[A, B](
      base: RawSchema[A]
  )(
      f: InputMap[A, B]
  ): RawSchema[B] =
    base match
      case RawSchema.Mapped(base0, mapping) => RawSchema.Mapped(base0, mapping.withPureMap(f))
      case _ => RawSchema.Mapped(base, SchemaMapping[A, A]().withPureMap(f))

  def mapIntTotal[A](
      base: RawSchema[Int]
  )(
      resultMap0: Reader.IntMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(base, SchemaMapping[Int, Int]().withIntMap(resultMap0))

  def mapLongTotal[A](
      base: RawSchema[Long]
  )(
      resultMap0: Reader.LongMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(base, SchemaMapping[Long, Long]().withLongMap(resultMap0))

  def mapFloatTotal[A](
      base: RawSchema[Float]
  )(
      resultMap0: Reader.FloatMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(base, SchemaMapping[Float, Float]().withFloatMap(resultMap0))

  def mapDoubleTotal[A](
      base: RawSchema[Double]
  )(
      resultMap0: Reader.DoubleMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(base, SchemaMapping[Double, Double]().withDoubleMap(resultMap0))

  def mapResultAndInput[A, B](
      base: RawSchema[A]
  )(
      resultMap0: ResultMap[A, B],
      inputMap0: InputMap[B, A]
  ): RawSchema[B] =
    base match
      case RawSchema.Mapped(base0, mapping) =>
        RawSchema.Mapped(base0, mapping.withMapped(resultMap0, inputMap0))
      case _ =>
        RawSchema.Mapped(base, SchemaMapping[A, A]().withMapped(resultMap0, inputMap0))

  def mapPureAndInput[A, B](
      base: RawSchema[A]
  )(
      resultMap0: InputMap[A, B],
      inputMap0: InputMap[B, A]
  ): RawSchema[B] =
    base match
      case RawSchema.Mapped(base0, mapping) =>
        RawSchema.Mapped(base0, mapping.withPureAndInput(resultMap0, inputMap0))
      case _ =>
        RawSchema.Mapped(base, SchemaMapping[A, A]().withPureAndInput(resultMap0, inputMap0))

  // The total-map constructors take the Writer typed functions so writes stay unboxed — the
  // write-side mirror of the specialized Reader maps. The plain-function variants live in
  // [[RawSchemaPlainInputMaps]] for binary compatibility only.

  private[scalanotation] def mapIntTotalAndInput[A](
      base: RawSchema[Int]
  )(
      resultMap0: Reader.IntMap[A],
      inputMap0: Writer.IntMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(
      base,
      SchemaMapping[Int, Int]()
        .withIntMap(resultMap0)
        .copy(inputMap = SchemaMapping.IntInput(inputMap0))
    )

  private[scalanotation] def mapLongTotalAndInput[A](
      base: RawSchema[Long]
  )(
      resultMap0: Reader.LongMap[A],
      inputMap0: Writer.LongMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(
      base,
      SchemaMapping[Long, Long]()
        .withLongMap(resultMap0)
        .copy(inputMap = SchemaMapping.LongInput(inputMap0))
    )

  private[scalanotation] def mapFloatTotalAndInput[A](
      base: RawSchema[Float]
  )(
      resultMap0: Reader.FloatMap[A],
      inputMap0: Writer.FloatMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(
      base,
      SchemaMapping[Float, Float]()
        .withFloatMap(resultMap0)
        .copy(inputMap = SchemaMapping.FloatInput(inputMap0))
    )

  private[scalanotation] def mapDoubleTotalAndInput[A](
      base: RawSchema[Double]
  )(
      resultMap0: Reader.DoubleMap[A],
      inputMap0: Writer.DoubleMap[A]
  ): RawSchema[A] =
    RawSchema.Mapped(
      base,
      SchemaMapping[Double, Double]()
        .withDoubleMap(resultMap0)
        .copy(inputMap = SchemaMapping.DoubleInput(inputMap0))
    )

  // The plain-function variants, superseded by the Writer typed variants above. Hidden from
  // source (a plain function would win overload resolution against the typed SAM for lambda
  // arguments) but kept public in binary for compatibility, delegating to the typed variants.

  @deprecated("bincompat only — pass a Writer.IntMap for an unboxed write", "0.5.0")
  @annotation.publicInBinary
  private[scalanotation] def mapIntTotalAndInput[A](
      base: RawSchema[Int]
  )(
      resultMap0: Reader.IntMap[A],
      inputMap0: InputMap[A, Int]
  ): RawSchema[A] =
    mapIntTotalAndInput(base)(resultMap0, (inputMap0(_)): Writer.IntMap[A])

  @deprecated("bincompat only — pass a Writer.LongMap for an unboxed write", "0.5.0")
  @annotation.publicInBinary
  private[scalanotation] def mapLongTotalAndInput[A](
      base: RawSchema[Long]
  )(
      resultMap0: Reader.LongMap[A],
      inputMap0: InputMap[A, Long]
  ): RawSchema[A] =
    mapLongTotalAndInput(base)(resultMap0, (inputMap0(_)): Writer.LongMap[A])

  @deprecated("bincompat only — pass a Writer.FloatMap for an unboxed write", "0.5.0")
  @annotation.publicInBinary
  private[scalanotation] def mapFloatTotalAndInput[A](
      base: RawSchema[Float]
  )(
      resultMap0: Reader.FloatMap[A],
      inputMap0: InputMap[A, Float]
  ): RawSchema[A] =
    mapFloatTotalAndInput(base)(resultMap0, (inputMap0(_)): Writer.FloatMap[A])

  @deprecated("bincompat only — pass a Writer.DoubleMap for an unboxed write", "0.5.0")
  @annotation.publicInBinary
  private[scalanotation] def mapDoubleTotalAndInput[A](
      base: RawSchema[Double]
  )(
      resultMap0: Reader.DoubleMap[A],
      inputMap0: InputMap[A, Double]
  ): RawSchema[A] =
    mapDoubleTotalAndInput(base)(resultMap0, (inputMap0(_)): Writer.DoubleMap[A])
