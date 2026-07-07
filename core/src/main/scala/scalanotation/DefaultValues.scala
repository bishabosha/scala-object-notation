package scalanotation

import scala.annotation.publicInBinary

/** Field default values for `T`, consumed by [[Configured.withDefaultValues]]: a field omitted from
  * the input decodes to its default instead of failing. Evidence is gathered from `T`'s definition
  * by `Defaults.derived` in the macros module (case-class and enum-case constructor defaults), or
  * assembled manually.
  *
  * Values are evaluated once, when the evidence is created. Defaults that depend on other
  * constructor parameters cannot be gathered and are treated as absent.
  */
final class DefaultValues[T] @publicInBinary private[scalanotation] (
    private[scalanotation] val selfDefaults: Map[String, AnyRef],
    private[scalanotation] val caseDefaults: Map[String, Map[String, AnyRef]],
    private[scalanotation] val bindings: List[(List[DefaultValues.Segment], AnyRef)]
)

object DefaultValues:
  @publicInBinary
  private[scalanotation] def create[T](
      selfDefaults: Map[String, AnyRef],
      caseDefaults: Map[String, Map[String, AnyRef]]
  ): DefaultValues[T] =
    new DefaultValues[T](selfDefaults, caseDefaults, Nil)

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
  def of[T](bind: Path[T] => Seq[Binding]): DefaultValues[T] =
    val bindings = bind(new Path[T](Nil)).view.map(b => (b.segments, b.value)).toList
    new DefaultValues[T](Map.empty, Map.empty, bindings)

  /** one step of a [[Path]] into the schema's nested structure */
  private[scalanotation] enum Segment:
    case Field(name: String)
    case InOption
    case InVector

  /** A typed path into `T`'s nested structure. Field selections are computed from
    * `NamedTuple.From[T]` through `Selectable`, so only `T`'s real fields (with their real types)
    * are selectable; [[some]] and [[each]] step inside `Option` and `Vector` fields.
    */
  final class Path[T] private[DefaultValues] (
      private[scalanotation] val reversedSegments: List[Segment]
  ) extends Selectable:
    type Fields = NamedTuple.Map[NamedTuple.From[T], Path]
    def selectDynamic(name: String): Path[?] =
      new Path(Segment.Field(name) :: reversedSegments)

  extension [T](path: Path[T])
    /** binds `value` as the decode-time default of the selected field */
    def :=(value: T): Binding =
      require(
        path.reversedSegments.headOption.exists(_.isInstanceOf[Segment.Field]),
        "a default binds to a field — select one before ':=' (after .some/.each, select a field)"
      )
      Binding(path.reversedSegments.reverse, value.asInstanceOf[AnyRef])

  extension [E](path: Path[Option[E]])
    /** steps inside an `Option` field, so defaults can bind to the element's fields */
    def some: Path[E] = new Path(Segment.InOption :: path.reversedSegments)

  extension [E](path: Path[Vector[E]])
    /** steps inside a `Vector` field, so defaults can bind to the element's fields */
    def each: Path[E] = new Path(Segment.InVector :: path.reversedSegments)

  /** a bound default: the path to a field, and the value it fills with when omitted */
  final class Binding private[DefaultValues] (
      private[scalanotation] val segments: List[Segment],
      private[scalanotation] val value: AnyRef
  )
