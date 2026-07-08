package scalanotation

import scala.annotation.publicInBinary

/** Field default values for `T`, consumed by [[Configured.withDefaultValues]]: a field omitted from
  * the input decodes to its default instead of failing. Evidence is gathered from `T`'s definition
  * by `Defaults.derived` in the macros module (case-class and enum-case constructor defaults), or
  * assembled manually.
  *
  * Every default shares one representation: a path of [[DefaultValues.Segment]]s to the value it
  * binds. A case-class constructor default is a single-field path, an enum-case default steps
  * through its case first, and manual bindings spell their whole path through nested records,
  * `Option`s and `Vector`s.
  *
  * Values are evaluated once, when the evidence is created. Defaults that depend on other
  * constructor parameters cannot be gathered and are treated as absent.
  */
final class DefaultValues[T] @publicInBinary private[scalanotation] (
    private[scalanotation] val bindings: List[(List[DefaultValues.Segment], AnyRef)]
)

object DefaultValues:
  @publicInBinary
  private[scalanotation] def create[T](
      selfDefaults: Map[String, AnyRef],
      caseDefaults: Map[String, Map[String, AnyRef]]
  ): DefaultValues[T] =
    val self = selfDefaults.view
      .map((name, value) => (List[Segment](Segment.Field(name)), value))
      .toList
    val cases = caseDefaults.view.flatMap { (caseName, fieldDefaults) =>
      fieldDefaults.view.map((name, value) =>
        (List[Segment](Segment.InCase(caseName), Segment.Field(name)), value)
      )
    }.toList
    new DefaultValues[T](self ++ cases)

  /** Assembles default values manually, as typed path bindings over `T`'s (possibly nested)
    * structure — a lens-like [[Path]] selects a field through records, `Option`s (`.some`) and
    * `Vector`s (`.each`), and `:=` binds the default installed for it:
    * {{{
    * case class Db(host: String, port: Int)
    * case class Config(name: String, db: Option[Db], workers: Vector[Worker])
    *
    * given DefaultValues[Config] = DefaultValues.of[Config] { c =>
    *   Seq(
    *     c.name := "app",
    *     c.db.some.port := 5432,
    *     c.workers.each.retries := 3
    *   )
    * }
    * }}}
    */
  def of[T](bind: Path[T, false] => Seq[Binding]): DefaultValues[T] =
    val bindings = bind(new Path[T, false](Nil)).view.map(b => (b.segments, b.value)).toList
    new DefaultValues[T](bindings)

  /** One step of a [[Path]] into the schema's nested structure. [[Segment.InCase]] selects an enum
    * case — spelled by macro-gathered enum defaults; the manual [[Path]] lens cannot express it yet
    * (a typed constructor for case selection is future design work).
    */
  private[scalanotation] enum Segment:
    case Field(name: String)
    case InOption
    case InVector
    case InCase(name: String)

  /** A typed path into `T`'s nested structure. Field selections are computed from
    * `NamedTuple.From[T]` through `Selectable`, so only `T`'s real fields (with their real types)
    * are selectable; [[some]] and [[each]] step inside `Option` and `Vector` fields.
    *
    * `AtField` witnesses that the path's last step selected a field: only such paths can bind a
    * default with [[`:=`]]. The root path and every [[some]]/[[each]] step sit between fields
    * (`AtField = false`), so `c := ...` or `c.workers.each := ...` do not typecheck.
    */
  final class Path[T, AtField <: Boolean] private[DefaultValues] (
      private[scalanotation] val reversedSegments: List[Segment]
  ) extends Selectable:
    type Fields = NamedTuple.Map[NamedTuple.From[T], [F] =>> Path[F, true]]
    def selectDynamic(name: String): Path[?, true] =
      new Path(Segment.Field(name) :: reversedSegments)

  extension [T](path: Path[T, true])
    /** binds `value` as the decode-time default of the selected field */
    def :=(value: T): Binding =
      Binding(path.reversedSegments.reverse, value.asInstanceOf[AnyRef])

  extension [T, AtField <: Boolean](path: Path[T, AtField])
    /** steps inside a field represented by an Option schema, so defaults can bind to the inner
      * value's fields
      */
    def some(using repr: OptionRepr[T]): Path[repr.Inner, false] =
      new Path(Segment.InOption :: path.reversedSegments)

    /** steps inside a field represented by a Vector schema, so defaults can bind to the elements'
      * fields
      */
    def each(using repr: VectorRepr[T]): Path[repr.Elem, false] =
      new Path(Segment.InVector :: path.reversedSegments)

  /** Witnesses that `T` is represented by an Option schema, with inner values of type `Inner`.
    * Instances mirroring the library's readers are provided; supply one for a custom
    * Option-represented type to make it steppable with [[some]].
    */
  trait OptionRepr[T]:
    type Inner

  object OptionRepr:
    private val witness: OptionRepr[Any] = new OptionRepr[Any] {}

    given option: [E] => (OptionRepr[Option[E]] { type Inner = E }) =
      witness.asInstanceOf[OptionRepr[Option[E]] { type Inner = E }]

  /** Witnesses that `T` is represented by a Vector schema, with elements of type `Elem`. Instances
    * mirror the library's readers — `Vector`, any `scala.collection.Seq` subtype, `IArray` and
    * `Array`; supply one for a custom Vector-represented type to make it steppable with [[each]].
    */
  trait VectorRepr[T]:
    type Elem

  object VectorRepr:
    private val witness: VectorRepr[Any] = new VectorRepr[Any] {}

    given vector: [E] => (VectorRepr[Vector[E]] { type Elem = E }) =
      witness.asInstanceOf[VectorRepr[Vector[E]] { type Elem = E }]

    given seq: [E, Col[X] <: scala.collection.Seq[X]] => (VectorRepr[Col[E]] { type Elem = E }) =
      witness.asInstanceOf[VectorRepr[Col[E]] { type Elem = E }]

    given iarray: [E] => (VectorRepr[IArray[E]] { type Elem = E }) =
      witness.asInstanceOf[VectorRepr[IArray[E]] { type Elem = E }]

    given array: [E] => (VectorRepr[Array[E]] { type Elem = E }) =
      witness.asInstanceOf[VectorRepr[Array[E]] { type Elem = E }]

  /** a bound default: the path to a field, and the value it fills with when omitted */
  final class Binding private[DefaultValues] (
      private[scalanotation] val segments: List[Segment],
      private[scalanotation] val value: AnyRef
  )
