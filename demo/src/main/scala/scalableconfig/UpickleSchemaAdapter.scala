package scalableconfig

import scalanotation.DecodeError
import scalanotation.ReadWriter as SonReadWriter
import scalanotation.Reader
import scalanotation.RouterSchema
import scalanotation.schema.RawSchema
import scalanotation.schema.SchemaMapping
import steps.result.Result
import upickle.core.AbortException
import upickle.core.ArrVisitor
import upickle.core.NoOpVisitor
import upickle.core.ObjVisitor
import upickle.core.SimpleVisitor
import upickle.core.Visitor

import scala.Option as ScalaOption
import scala.collection.mutable

private object UpickleSchemaAdapter:
  def readWriter[A](readWriter: SonReadWriter[A]): upickle.default.ReadWriter[A] =
    val schema = readWriter.schema
    new upickle.default.ReadWriter.Delegate[A](
      visitorFor(schema).asInstanceOf[Visitor[Any, A]]
    ):
      def write0[V](out: Visitor[?, V], v: A): V =
        writeValue(schema, v, out)

  private def visitorFor(schema: RawSchema[?]): Visitor[Any, Any] =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        val mapping = mapped.mapping.asInstanceOf[SchemaMapping[Any, Any]]
        visitorFor(mapped.base).mapNulls(value => resultOrAbort(mapping.mapResult(value), -1))
      case RawSchema.Ref(_, target) =>
        visitorFor(target())
      case router: RawSchema.Router[?] =>
        routerVisitor(router)
      case namedTuple: RawSchema.NamedTuple[?] =>
        objectVisitor(NamedTupleConsumer(namedTuple, alreadySeenField = null))
      case tuple: RawSchema.Tuple[?] =>
        tupleVisitor(tuple)
      case RawSchema.PartialNamedTuple(base, alreadySeenField) =>
        partialNamedTupleVisitor(base, alreadySeenField)
      case sum: RawSchema.Sum[?] =>
        objectVisitor(SumConsumer(sum))
      case sum: RawSchema.DiscriminatorSum[?] =>
        discriminatorSumVisitor(sum)
      case vector: RawSchema.Vector[?, ?] =>
        vectorVisitor(vector)
      case tupleOf: RawSchema.TupleOf[?, ?] =>
        tupleOfVisitor(tupleOf)
      case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
        pairSeqVisitor(pairSeq)
      case dict: RawSchema.Dict[?, ?] =>
        objectVisitor(DictConsumer(dict))
      case option: RawSchema.Option[?] =>
        optionVisitor(option)
      case RawSchema.String =>
        stringVisitor(schema)
      case RawSchema.Char =>
        charVisitor(schema)
      case RawSchema.Int =>
        intVisitor(schema)
      case RawSchema.Long =>
        longVisitor(schema)
      case RawSchema.Float =>
        floatVisitor(schema)
      case RawSchema.Double =>
        doubleVisitor(schema)
      case RawSchema.Boolean =>
        booleanVisitor(schema)
      case RawSchema.Null =>
        nullVisitor(schema)

  private def routerVisitor(schema: RawSchema.Router[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitObject(
          length: Int,
          jsonableKeys: Boolean,
          index: Int
      ): ObjVisitor[Any, Any] =
        visitorFor(routerCase(schema, RouterSchema.RouterConstruct.Record, index))
          .visitObject(length, jsonableKeys, index)

      override def visitArray(length: Int, index: Int): ArrVisitor[Any, Any] =
        val construct =
          if RawSchema.routerCase(schema, schema.router.vectorIndex) != null then
            RouterSchema.RouterConstruct.Vector
          else RouterSchema.RouterConstruct.Tuple
        visitorFor(routerCase(schema, construct, index)).visitArray(length, index)

      override def visitString(s: CharSequence, index: Int): Any =
        visitorFor(routerCase(schema, RouterSchema.RouterConstruct.String, index))
          .visitString(s, index)

      override def visitChar(s: Char, index: Int): Any =
        visitorFor(routerCase(schema, RouterSchema.RouterConstruct.Char, index)).visitChar(s, index)

      override def visitInt32(i: Int, index: Int): Any =
        val construct = numberConstruct(RouterSchema.RouterConstruct.Int, schema.numberMode)
        visitorFor(routerCase(schema, construct, index)).visitInt32(i, index)

      override def visitInt64(i: Long, index: Int): Any =
        val construct = numberConstruct(RouterSchema.RouterConstruct.Long, schema.numberMode)
        visitorFor(routerCase(schema, construct, index)).visitInt64(i, index)

      override def visitUInt64(i: Long, index: Int): Any =
        val construct = numberConstruct(RouterSchema.RouterConstruct.Long, schema.numberMode)
        visitorFor(routerCase(schema, construct, index)).visitUInt64(i, index)

      override def visitFloat32(d: Float, index: Int): Any =
        val construct = numberConstruct(RouterSchema.RouterConstruct.Float, schema.numberMode)
        visitorFor(routerCase(schema, construct, index)).visitFloat32(d, index)

      override def visitFloat64(d: Double, index: Int): Any =
        val construct = numberConstruct(RouterSchema.RouterConstruct.Double, schema.numberMode)
        visitorFor(routerCase(schema, construct, index)).visitFloat64(d, index)

      override def visitFloat64StringParts(
          s: CharSequence,
          decIndex: Int,
          expIndex: Int,
          index: Int
      ): Any =
        val construct = numberConstruct(RouterSchema.RouterConstruct.Double, schema.numberMode)
        visitorFor(routerCase(schema, construct, index))
          .visitFloat64StringParts(s, decIndex, expIndex, index)

      override def visitTrue(index: Int): Any =
        visitorFor(routerCase(schema, RouterSchema.RouterConstruct.Boolean, index)).visitTrue(index)

      override def visitFalse(index: Int): Any =
        visitorFor(routerCase(schema, RouterSchema.RouterConstruct.Boolean, index))
          .visitFalse(index)

      override def visitNull(index: Int): Any =
        visitorFor(routerCase(schema, RouterSchema.RouterConstruct.Null, index)).visitNull(index)

  private def routerCase(
      schema: RawSchema.Router[?],
      construct: RouterSchema.RouterConstruct,
      index: Int
  ): RawSchema[?] =
    val routerCase = RawSchema.routerCase(schema, schema.router.indexFor(construct))
    if routerCase == null then abort(s"Expected ${schema.describeSelf}", index)
    routerCase.schema

  private def numberConstruct(
      bounded: RouterSchema.RouterConstruct,
      mode: RouterSchema.NumberMode
  ): RouterSchema.RouterConstruct =
    mode match
      case RouterSchema.NumberMode.Bounded => bounded
      case RouterSchema.NumberMode.Raw     => RouterSchema.RouterConstruct.RawNumber

  private def partialNamedTupleVisitor(
      base: RawSchema[?],
      alreadySeenField: String
  ): Visitor[Any, Any] =
    base match
      case mapped: RawSchema.Mapped[?, ?] =>
        val mapping = mapped.mapping.asInstanceOf[SchemaMapping[Any, Any]]
        partialNamedTupleVisitor(mapped.base, alreadySeenField)
          .mapNulls(value => resultOrAbort(mapping.mapResult(value), -1))
      case RawSchema.Ref(_, target) =>
        partialNamedTupleVisitor(target(), alreadySeenField)
      case namedTuple: RawSchema.NamedTuple[?] =>
        objectVisitor(NamedTupleConsumer(namedTuple, alreadySeenField))
      case RawSchema.Null =>
        objectVisitor(EmptyObjectConsumer)
      case other =>
        typeVisitor(other)

  private def optionVisitor(schema: RawSchema.Option[?]): Visitor[Any, Any] =
    val inner = visitorFor(schema.inner)
    new Visitor.Delegate[Any, Any](inner):
      override def visitNull(index: Int): Any                    = None
      override def visitFalse(index: Int): Any                   = Some(inner.visitFalse(index))
      override def visitTrue(index: Int): Any                    = Some(inner.visitTrue(index))
      override def visitString(s: CharSequence, index: Int): Any = Some(inner.visitString(s, index))
      override def visitChar(s: Char, index: Int): Any           = Some(inner.visitChar(s, index))
      override def visitInt32(i: Int, index: Int): Any           = Some(inner.visitInt32(i, index))
      override def visitInt64(i: Long, index: Int): Any          = Some(inner.visitInt64(i, index))
      override def visitUInt64(i: Long, index: Int): Any         = Some(inner.visitUInt64(i, index))
      override def visitFloat32(d: Float, index: Int): Any  = Some(inner.visitFloat32(d, index))
      override def visitFloat64(d: Double, index: Int): Any = Some(inner.visitFloat64(d, index))
      override def visitFloat64StringParts(
          s: CharSequence,
          decIndex: Int,
          expIndex: Int,
          index: Int
      ): Any =
        Some(inner.visitFloat64StringParts(s, decIndex, expIndex, index))
      override def visitObject(
          length: Int,
          jsonableKeys: Boolean,
          index: Int
      ): ObjVisitor[Any, Any] =
        mapObject(inner.visitObject(length, jsonableKeys, index), Some(_))
      override def visitArray(length: Int, index: Int): ArrVisitor[Any, Any] =
        mapArray(inner.visitArray(length, index), Some(_))

  private def tupleVisitor(schema: RawSchema.Tuple[?]): Visitor[Any, Any] =
    val read = schema.read.asInstanceOf[Reader.TupleBuilder[Any, Any]]
    if read == null then missingRead(schema)
    new BaseVisitor(schema):
      override def visitArray(length: Int, index: Int): ArrVisitor[Any, Any] =
        new ArrVisitor[Any, Any]:
          private var elementIndex = 0
          private var state        = read.init(schema.slots.length)

          def subVisitor: Visitor[?, ?] =
            if elementIndex >= schema.slots.length then abort("Too many array elements", index)
            else visitorFor(schema.slots(elementIndex))

          def visitValue(v: Any, index: Int): Unit =
            if elementIndex >= schema.slots.length then abort("Too many array elements", index)
            state = read.add(state, elementIndex, v)
            elementIndex += 1

          def visitEnd(index: Int): Any =
            if elementIndex != schema.slots.length then
              abort(
                s"Expected ${schema.slots.length} array elements but found $elementIndex",
                index
              )
            read.finish(state)

  private def vectorVisitor(schema: RawSchema.Vector[?, ?]): Visitor[Any, Any] =
    val read = schema.read.asInstanceOf[Reader.VectorBuilder[Any, Any, Any]]
    if read == null then missingRead(schema)
    new BaseVisitor(schema):
      override def visitArray(length: Int, index: Int): ArrVisitor[Any, Any] =
        new VectorContext(read, schema.element)

  private def tupleOfVisitor(schema: RawSchema.TupleOf[?, ?]): Visitor[Any, Any] =
    val read = schema.read.asInstanceOf[Reader.VectorBuilder[Any, Any, Any]]
    if read == null then missingRead(schema)
    new BaseVisitor(schema):
      override def visitArray(length: Int, index: Int): ArrVisitor[Any, Any] =
        new VectorContext(read, schema.element)

  private final class VectorContext(
      read: Reader.VectorBuilder[Any, Any, Any],
      element: RawSchema[?]
  ) extends ArrVisitor[Any, Any]:
    private var state = read.init()

    def subVisitor: Visitor[?, ?] =
      visitorFor(element)

    def visitValue(v: Any, index: Int): Unit =
      state = read.add(state, v)

    def visitEnd(index: Int): Any =
      read.finish(state)

  private def pairSeqVisitor(schema: RawSchema.PairSeq[?, ?, ?]): Visitor[Any, Any] =
    val read = schema.read.asInstanceOf[Reader.PairSeqBuilder[Any, Any, Any, Any]]
    if read == null then missingRead(schema)
    new BaseVisitor(schema):
      override def visitArray(length: Int, index: Int): ArrVisitor[Any, Any] =
        new ArrVisitor[Any, Any]:
          private var state = read.init()

          def subVisitor: Visitor[?, ?] =
            pairVisitor(schema.key, schema.value)

          def visitValue(v: Any, index: Int): Unit =
            val pair = v.asInstanceOf[(Any, Any)]
            state = read.addKey(state, pair._1)
            state = read.addValue(state, pair._2)

          def visitEnd(index: Int): Any =
            read.finish(state)

  private def pairVisitor(key: RawSchema[?], value: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(RawSchema.Tuple(IArray(key, value))):
      override def visitArray(length: Int, index: Int): ArrVisitor[Any, Any] =
        new ArrVisitor[Any, Any]:
          private var elementIndex   = 0
          private var keyValue: Any  = null
          private var elemValue: Any = null

          def subVisitor: Visitor[?, ?] =
            elementIndex match
              case 0 => visitorFor(key)
              case 1 => visitorFor(value)
              case _ => abort("Too many pair elements", index)

          def visitValue(v: Any, index: Int): Unit =
            elementIndex match
              case 0 => keyValue = v
              case 1 => elemValue = v
              case _ => abort("Too many pair elements", index)
            elementIndex += 1

          def visitEnd(index: Int): Any =
            if elementIndex != 2 then
              abort(s"Expected 2 pair elements but found $elementIndex", index)
            keyValue -> elemValue

  private def objectVisitor(consumer0: => ObjectConsumer): Visitor[Any, Any] =
    new BaseVisitor(RawSchema.NamedTuple(IArray.empty)):
      override def visitObject(
          length: Int,
          jsonableKeys: Boolean,
          index: Int
      ): ObjVisitor[Any, Any] =
        val consumer = consumer0
        new ObjVisitor[Any, Any]:
          def visitKey(index: Int): Visitor[?, ?] =
            KeyVisitor

          def visitKeyValue(v: Any): Unit =
            consumer.visitKeyValue(v.asInstanceOf[String], -1)

          def subVisitor: Visitor[?, ?] =
            consumer.subVisitor

          def visitValue(v: Any, index: Int): Unit =
            consumer.visitValue(v, index)

          def visitEnd(index: Int): Any =
            consumer.finish(index)

  private trait ObjectConsumer:
    def visitKeyValue(key: String, index: Int): Unit
    def subVisitor: Visitor[?, ?]
    def visitValue(value: Any, index: Int): Unit
    def finish(index: Int): Any

  private final class NamedTupleConsumer(
      schema: RawSchema.NamedTuple[?],
      alreadySeenField: String | Null
  ) extends ObjectConsumer:
    private val read = schema.read
    if read == null then missingRead(schema)
    private val fields = schema.fields
    private val seen   = mutable.HashSet.empty[String]
    if alreadySeenField != null then seen += alreadySeenField

    private var state: read.nn.State        = read.nn.init(fields.length, null)
    private var nextIndex                   = 0
    private var currentIndex                = -1
    private var currentSchema: RawSchema[?] = RawSchema.Null

    def visitKeyValue(key: String, index: Int): Unit =
      if !seen.add(key) then abort(s"Duplicate field '$key'", index)
      if schema.allowSkippedNullableFields then
        while nextIndex < fields.length && fields(nextIndex).name != key && isOption(
            fields(nextIndex).schema
          )
        do
          state = read.nn.add(state, nextIndex, None)
          nextIndex += 1

      if nextIndex >= fields.length then abort(s"Unexpected field '$key'", index)
      val expectedField = fields(nextIndex)
      if expectedField.name != key then
        abort(s"Field was expected to be '${expectedField.name}' but was '$key'", index)
      currentIndex = nextIndex
      currentSchema = expectedField.schema

    def subVisitor: Visitor[?, ?] =
      visitorFor(currentSchema)

    def visitValue(value: Any, index: Int): Unit =
      state = read.nn.add(state, currentIndex, value)
      nextIndex = currentIndex + 1

    def finish(index: Int): Any =
      if schema.allowSkippedNullableFields then
        while nextIndex < fields.length && isOption(fields(nextIndex).schema) do
          state = read.nn.add(state, nextIndex, None)
          nextIndex += 1
      if nextIndex != fields.length then
        abort(s"Expected ${fields.length} fields but found $nextIndex", index)
      read.nn.finish(state)

  private final class DictConsumer(schema: RawSchema.Dict[?, ?]) extends ObjectConsumer:
    private val read = schema.read.asInstanceOf[Reader.DictBuilder[Any, Any, Any]]
    if read == null then missingRead(schema)
    private val seen  = mutable.HashSet.empty[String]
    private var state = read.init()
    private var key   = ""

    def visitKeyValue(key: String, index: Int): Unit =
      if !seen.add(key) then abort(s"Duplicate field '$key'", index)
      this.key = key

    def subVisitor: Visitor[?, ?] =
      visitorFor(schema.element)

    def visitValue(value: Any, index: Int): Unit =
      state = read.add(state, key, value)

    def finish(index: Int): Any =
      read.finish(state)

  private final class SumConsumer(schema: RawSchema.Sum[?]) extends ObjectConsumer:
    private var currentSchema: RawSchema[?] = RawSchema.Null
    private var seenCase                    = false
    private var value: Any                  = null

    def visitKeyValue(key: String, index: Int): Unit =
      if seenCase then abort(s"Unexpected field '$key'", index)
      val sumCase = RawSchema.findCase(schema, key)
      if sumCase == null then abort(s"Unexpected field '$key'", index)
      currentSchema = sumCase.schema
      seenCase = true

    def subVisitor: Visitor[?, ?] =
      visitorFor(currentSchema)

    def visitValue(value: Any, index: Int): Unit =
      this.value = value

    def finish(index: Int): Any =
      if !seenCase then abort("Expected 1 field but found 0", index)
      value

  private object EmptyObjectConsumer extends ObjectConsumer:
    def visitKeyValue(key: String, index: Int): Unit =
      abort(s"Unexpected field '$key'", index)

    def subVisitor: Visitor[?, ?] =
      NoOpVisitor

    def visitValue(value: Any, index: Int): Unit =
      ()

    def finish(index: Int): Any =
      null

  private def discriminatorSumVisitor(schema: RawSchema.DiscriminatorSum[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitObject(
          length: Int,
          jsonableKeys: Boolean,
          index: Int
      ): ObjVisitor[Any, Any] =
        new ObjVisitor[Any, Any]:
          private var sawDiscriminator                = false
          private var readingDiscriminator            = false
          private var selected: ObjectConsumer | Null = null

          def visitKey(index: Int): Visitor[?, ?] =
            KeyVisitor

          def visitKeyValue(v: Any): Unit =
            val key = v.asInstanceOf[String]
            if !sawDiscriminator then
              if key != schema.discriminatorField then
                abort(s"Field was expected to be '${schema.discriminatorField}' but was '$key'", -1)
              readingDiscriminator = true
            else selected.nn.visitKeyValue(key, -1)

          def subVisitor: Visitor[?, ?] =
            if readingDiscriminator then stringVisitor(RawSchema.String)
            else selected.nn.subVisitor

          def visitValue(v: Any, index: Int): Unit =
            if readingDiscriminator then
              val caseName = v.asInstanceOf[String]
              val sumCase  = RawSchema.findCase(schema, caseName)
              if sumCase == null then abort(s"Unexpected field '$caseName'", index)
              selected = consumerForPartial(sumCase.schema, schema.discriminatorField)
              sawDiscriminator = true
              readingDiscriminator = false
            else selected.nn.visitValue(v, index)

          def visitEnd(index: Int): Any =
            if !sawDiscriminator then
              abort(s"Missing required field '${schema.discriminatorField}'", index)
            selected.nn.finish(index)

  private def consumerForPartial(schema: RawSchema[?], alreadySeenField: String): ObjectConsumer =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        val mapping = mapped.mapping.asInstanceOf[SchemaMapping[Any, Any]]
        val inner   = consumerForPartial(mapped.base, alreadySeenField)
        new ObjectConsumer:
          def visitKeyValue(key: String, index: Int): Unit = inner.visitKeyValue(key, index)
          def subVisitor: Visitor[?, ?]                    = inner.subVisitor
          def visitValue(value: Any, index: Int): Unit     = inner.visitValue(value, index)
          def finish(index: Int): Any                      =
            resultOrAbort(mapping.mapResult(inner.finish(index)), index)
      case RawSchema.Ref(_, target) =>
        consumerForPartial(target(), alreadySeenField)
      case namedTuple: RawSchema.NamedTuple[?] =>
        NamedTupleConsumer(namedTuple, alreadySeenField)
      case RawSchema.Null =>
        EmptyObjectConsumer
      case other =>
        abort(s"Expected object payload schema, got ${other.describeSelf}", -1)

  private object KeyVisitor extends SimpleVisitor[Any, String]:
    def expectedMsg: String                                       = "expected object key"
    override def visitString(s: CharSequence, index: Int): String = s.toString
    override def visitChar(s: Char, index: Int): String           = s.toString

  private abstract class BaseVisitor(schema: RawSchema[?]) extends SimpleVisitor[Any, Any]:
    def expectedMsg: String                 = s"expected ${schema.describeSelf}"
    override def visitNull(index: Int): Any =
      abort(s"Expected ${schema.describeSelf} but found null", index)

  private def typeVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema) {}

  private def stringVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitString(s: CharSequence, index: Int): Any = s.toString

  private def charVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitString(s: CharSequence, index: Int): Any =
        val value = s.toString
        if value.length == 1 then value.charAt(0)
        else abort("Expected single-character string", index)
      override def visitChar(s: Char, index: Int): Any = s

  private def intVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitInt32(i: Int, index: Int): Any  = i
      override def visitInt64(i: Long, index: Int): Any =
        if i >= Int.MinValue && i <= Int.MaxValue then i.toInt
        else abort(s"Expected Int but found $i", index)
      override def visitFloat64(d: Double, index: Int): Any =
        if d.isWhole && d >= Int.MinValue && d <= Int.MaxValue then d.toInt
        else abort(s"Expected Int but found $d", index)
      override def visitFloat64StringParts(
          s: CharSequence,
          decIndex: Int,
          expIndex: Int,
          index: Int
      ): Any =
        visitFloat64(s.toString.toDouble, index)

  private def longVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitInt32(i: Int, index: Int): Any      = i.toLong
      override def visitInt64(i: Long, index: Int): Any     = i
      override def visitUInt64(i: Long, index: Int): Any    = i
      override def visitFloat64(d: Double, index: Int): Any =
        if d.isWhole && d >= Long.MinValue && d <= Long.MaxValue then d.toLong
        else abort(s"Expected Long but found $d", index)
      override def visitFloat64StringParts(
          s: CharSequence,
          decIndex: Int,
          expIndex: Int,
          index: Int
      ): Any =
        visitFloat64(s.toString.toDouble, index)

  private def floatVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitInt32(i: Int, index: Int): Any      = i.toFloat
      override def visitInt64(i: Long, index: Int): Any     = i.toFloat
      override def visitUInt64(i: Long, index: Int): Any    = i.toFloat
      override def visitFloat32(d: Float, index: Int): Any  = d
      override def visitFloat64(d: Double, index: Int): Any = d.toFloat
      override def visitFloat64StringParts(
          s: CharSequence,
          decIndex: Int,
          expIndex: Int,
          index: Int
      ): Any =
        s.toString.toFloat

  private def doubleVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitInt32(i: Int, index: Int): Any      = i.toDouble
      override def visitInt64(i: Long, index: Int): Any     = i.toDouble
      override def visitUInt64(i: Long, index: Int): Any    = i.toDouble
      override def visitFloat32(d: Float, index: Int): Any  = d.toDouble
      override def visitFloat64(d: Double, index: Int): Any = d
      override def visitFloat64StringParts(
          s: CharSequence,
          decIndex: Int,
          expIndex: Int,
          index: Int
      ): Any =
        s.toString.toDouble

  private def booleanVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitTrue(index: Int): Any  = true
      override def visitFalse(index: Int): Any = false

  private def nullVisitor(schema: RawSchema[?]): Visitor[Any, Any] =
    new BaseVisitor(schema):
      override def visitNull(index: Int): Any = null

  private def mapObject[A](ctx: ObjVisitor[Any, Any], f: Any => A): ObjVisitor[Any, Any] =
    new ObjVisitor[Any, Any]:
      def visitKey(index: Int): Visitor[?, ?]  = ctx.visitKey(index)
      def visitKeyValue(v: Any): Unit          = ctx.visitKeyValue(v)
      def subVisitor: Visitor[?, ?]            = ctx.subVisitor
      def visitValue(v: Any, index: Int): Unit = ctx.visitValue(v, index)
      def visitEnd(index: Int): Any            = f(ctx.visitEnd(index))

  private def mapArray[A](ctx: ArrVisitor[Any, Any], f: Any => A): ArrVisitor[Any, Any] =
    new ArrVisitor[Any, Any]:
      def subVisitor: Visitor[?, ?]            = ctx.subVisitor
      def visitValue(v: Any, index: Int): Unit = ctx.visitValue(v, index)
      def visitEnd(index: Int): Any            = f(ctx.visitEnd(index))

  private def writeValue[V](schema: RawSchema[?], value: Any, out: Visitor[?, V]): V =
    writeAny(schema, value, out.asInstanceOf[Visitor[Any, Any]]).asInstanceOf[V]

  private def writeAny(schema: RawSchema[?], value: Any, out: Visitor[Any, Any]): Any =
    schema match
      case mapped: RawSchema.Mapped[?, ?] =>
        val mapping = mapped.mapping.asInstanceOf[SchemaMapping[Any, Any]]
        writeAny(mapped.base, mapping.mapInput(value), out)
      case RawSchema.Ref(_, target) =>
        writeAny(target(), value, out)
      case router: RawSchema.Router[?] =>
        writeAny(selectedRouterCase(router, value).schema, value, out)
      case namedTuple: RawSchema.NamedTuple[?] =>
        val write = namedTuple.write
        if write == null then missingWrite(namedTuple)
        val ctx = out.visitObject(namedTuple.fields.length, jsonableKeys = true, index = -1)
        var i   = 0
        while i < namedTuple.fields.length do
          writeField(
            ctx,
            namedTuple.fields(i).name,
            namedTuple.fields(i).schema,
            write.fieldValue(value, i)
          )
          i += 1
        ctx.visitEnd(-1)
      case tuple: RawSchema.Tuple[?] =>
        val write = tuple.write
        if write == null then missingWrite(tuple)
        val size = write.size(value)
        val ctx  = out.visitArray(size, -1)
        var i    = 0
        while i < size do
          ctx.narrow.visitValue(
            writeAny(tuple.slots(i), write.elementValue(value, i), subOut(ctx)),
            -1
          )
          i += 1
        ctx.visitEnd(-1)
      case RawSchema.PartialNamedTuple(base, _) =>
        writeAny(base, value, out)
      case sum: RawSchema.Sum[?] =>
        val write = sum.write
        if write == null then missingWrite(sum)
        val sumCase = sum.cases(write.caseIndex(value))
        val ctx     = out.visitObject(1, jsonableKeys = true, index = -1)
        writeField(ctx, sumCase.name, sumCase.schema, value)
        ctx.visitEnd(-1)
      case sum: RawSchema.DiscriminatorSum[?] =>
        val write = sum.write
        if write == null then missingWrite(sum)
        val sumCase = sum.cases(write.caseIndex(value))
        val ctx     =
          out.visitObject(1 + payloadFieldCount(sumCase.schema), jsonableKeys = true, index = -1)
        writeField(ctx, sum.discriminatorField, RawSchema.String, sumCase.name)
        writePayloadFields(ctx, sumCase.schema, value)
        ctx.visitEnd(-1)
      case vector: RawSchema.Vector[?, ?] =>
        writeVector(vector.element, vector.write, value, out, vector)
      case tupleOf: RawSchema.TupleOf[?, ?] =>
        writeVector(tupleOf.element, tupleOf.write, value, out, tupleOf)
      case pairSeq: RawSchema.PairSeq[?, ?, ?] =>
        val write = pairSeq.write
        if write == null then missingWrite(pairSeq)
        val ctx    = out.visitArray(write.size(value), -1)
        val values = write.iterator(value)
        while values.hasNext do
          val (key, elem) = values.next()
          val pairCtx     = subOut(ctx).visitArray(2, -1)
          pairCtx.narrow.visitValue(writeAny(pairSeq.key, key, subOut(pairCtx)), -1)
          pairCtx.narrow.visitValue(writeAny(pairSeq.value, elem, subOut(pairCtx)), -1)
          ctx.narrow.visitValue(pairCtx.visitEnd(-1), -1)
        ctx.visitEnd(-1)
      case dict: RawSchema.Dict[?, ?] =>
        val write = dict.write
        if write == null then missingWrite(dict)
        val ctx    = out.visitObject(write.size(value), jsonableKeys = true, index = -1)
        val values = write.iterator(value)
        while values.hasNext do
          val (key, elem) = values.next()
          writeField(ctx, key, dict.element, elem)
        ctx.visitEnd(-1)
      case option: RawSchema.Option[?] =>
        value.asInstanceOf[ScalaOption[Any]] match
          case Some(innerValue) => writeAny(option.inner, innerValue, out)
          case None             => out.visitNull(-1)
      case RawSchema.String =>
        out.visitString(value.asInstanceOf[String], -1)
      case RawSchema.Char =>
        out.visitChar(value.asInstanceOf[Char], -1)
      case RawSchema.Int =>
        out.visitInt32(value.asInstanceOf[Int], -1)
      case RawSchema.Long =>
        out.visitInt64(value.asInstanceOf[Long], -1)
      case RawSchema.Float =>
        out.visitFloat32(value.asInstanceOf[Float], -1)
      case RawSchema.Double =>
        out.visitFloat64(value.asInstanceOf[Double], -1)
      case RawSchema.Boolean =>
        if value.asInstanceOf[Boolean] then out.visitTrue(-1) else out.visitFalse(-1)
      case RawSchema.Null =>
        out.visitNull(-1)

  private def writeVector(
      element: RawSchema[?],
      write: RawSchema.VectorWrite | Null,
      value: Any,
      out: Visitor[Any, Any],
      schema: RawSchema[?]
  ): Any =
    if write == null then missingWrite(schema)
    val ctx    = out.visitArray(write.size(value), -1)
    val values = write.iterator(value)
    while values.hasNext do ctx.narrow.visitValue(writeAny(element, values.next(), subOut(ctx)), -1)
    ctx.visitEnd(-1)

  private def writeField(
      ctx: ObjVisitor[?, Any],
      key: String,
      schema: RawSchema[?],
      value: Any
  ): Unit =
    val keyVisitor = ctx.visitKey(-1)
    ctx.visitKeyValue(keyVisitor.visitString(key, -1))
    ctx.narrow.visitValue(writeAny(schema, value, subOut(ctx)), -1)

  private def subOut(ctx: upickle.core.ObjArrVisitor[?, Any]): Visitor[Any, Any] =
    ctx.subVisitor.asInstanceOf[Visitor[Any, Any]]

  private def payloadFieldCount(schema: RawSchema[?]): Int =
    schema match
      case RawSchema.PartialNamedTuple(base, _) => payloadFieldCount(base)
      case mapped: RawSchema.Mapped[?, ?]       => payloadFieldCount(mapped.base)
      case RawSchema.Ref(_, target)             => payloadFieldCount(target())
      case namedTuple: RawSchema.NamedTuple[?]  => namedTuple.fields.length
      case RawSchema.Null                       => 0
      case other => abort(s"Expected object payload schema, got ${other.describeSelf}", -1)

  private def writePayloadFields(ctx: ObjVisitor[?, Any], schema: RawSchema[?], value: Any): Unit =
    schema match
      case RawSchema.PartialNamedTuple(base, _) =>
        writePayloadFields(ctx, base, value)
      case mapped: RawSchema.Mapped[?, ?] =>
        val mapping = mapped.mapping.asInstanceOf[SchemaMapping[Any, Any]]
        writePayloadFields(ctx, mapped.base, mapping.mapInput(value))
      case RawSchema.Ref(_, target) =>
        writePayloadFields(ctx, target(), value)
      case namedTuple: RawSchema.NamedTuple[?] =>
        val write = namedTuple.write
        if write == null then missingWrite(namedTuple)
        var i = 0
        while i < namedTuple.fields.length do
          writeField(
            ctx,
            namedTuple.fields(i).name,
            namedTuple.fields(i).schema,
            write.fieldValue(value, i)
          )
          i += 1
      case RawSchema.Null =>
        ()
      case other =>
        abort(s"Expected object payload schema, got ${other.describeSelf}", -1)

  private def selectedRouterCase(schema: RawSchema.Router[?], value: Any): RawSchema.RouterCase[?] =
    val write = schema.write
    if write == null then missingWrite(schema)
    val selected =
      RawSchema.routerCase(
        schema,
        write.asInstanceOf[RouterSchema.Write[Any]].caseIndex(schema.router, value)
      )
    if selected == null then
      throw IllegalArgumentException(
        s"router ${schema.describeSelf} cannot select a case for value $value"
      )
    selected

  private def isOption(schema: RawSchema[?]): Boolean =
    schema match
      case _: RawSchema.Option[?]    => true
      case RawSchema.Mapped(base, _) => isOption(base)
      case RawSchema.Ref(_, target)  => isOption(target())
      case _                         => false

  private def resultOrAbort(result: Result[Any, DecodeError], index: Int): Any =
    result match
      case Result.Ok(value)  => value
      case Result.Err(error) => abort(error.format, index)

  private def missingRead(schema: RawSchema[?]): Nothing =
    abort(s"read is not available for schema ${schema.describeSelf}", -1)

  private def missingWrite(schema: RawSchema[?]): Nothing =
    throw IllegalStateException(s"write is not available for schema ${schema.describeSelf}")

  private def abort(message: String, index: Int): Nothing =
    throw AbortException(message, index, -1, -1, null)
