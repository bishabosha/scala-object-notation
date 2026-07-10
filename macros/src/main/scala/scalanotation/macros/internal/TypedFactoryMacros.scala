package scalanotation.macros.internal

import scalanotation.BuilderSlots
import scalanotation.TypedFactory

import scala.annotation.publicInBinary
import scala.compiletime
import scala.deriving.Mirror
import scala.quoted.*

@publicInBinary
private[scalanotation] object TypedFactoryMacros:

  /** runtime fallback: builds via the mirror, boxing on demand through the Product interface */
  def fromProductFactory[P](m: Mirror.ProductOf[P]): TypedFactory.OfProduct[P] =
    slots => m.fromProduct(slots)

  /** Derives a [[TypedFactory.OfProduct]] that invokes `P`'s primary constructor with each argument
    * pulled from the matching typed slot, so primitive fields are never boxed. Shapes that cannot
    * be constructed directly (inner classes, non-public or curried constructors) fall back to
    * `Mirror.fromProduct` over the slots.
    */
  inline def productFactory[P](using m: Mirror.ProductOf[P]): TypedFactory.OfProduct[P] =
    ${ productFactoryImpl[P] }

  /** derives a [[TypedFactory.OfProduct]] per structured product case, keyed by case label; nullary
    * cases decode to a fixed value and need no factory
    */
  inline def caseFactories[Labels <: Tuple, Cases <: Tuple]
      : Map[String, TypedFactory.OfProduct[?]] =
    inline compiletime.erasedValue[Labels] match
      case _: EmptyTuple        => Map.empty
      case _: (label *: labels) =>
        inline compiletime.erasedValue[Cases] match
          case _: (kase *: cases) =>
            addCaseFactory[label, kase](caseFactories[labels, cases])

  inline def addCaseFactory[Label, Case](
      rest: Map[String, TypedFactory.OfProduct[?]]
  ): Map[String, TypedFactory.OfProduct[?]] =
    compiletime.summonFrom {
      case m: Mirror.ProductOf[Case] =>
        inline compiletime.erasedValue[m.MirroredElemTypes] match
          case _: EmptyTuple => rest
          case _             =>
            rest.updated(
              compiletime.constValue[Label].asInstanceOf[String],
              productFactory[Case](using m)
            )
      case _ => rest // non-product case: leave it to the legacy build path
    }

  def productFactoryImpl[P: Type](using Quotes): Expr[TypedFactory.OfProduct[P]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[P].dealias
    val sym = tpe.typeSymbol

    def fallback(): Expr[TypedFactory.OfProduct[P]] =
      Expr.summon[Mirror.ProductOf[P]] match
        case Some(mirror) => '{ fromProductFactory[P]($mirror) }
        case None         =>
          report.errorAndAbort(
            s"Cannot derive a typed factory for ${Type.show[P]}: no Mirror.ProductOf instance"
          )

    val ctor = sym.primaryConstructor

    def termParams: List[Symbol] =
      ctor.paramSymss match
        case List(params) if !params.exists(_.isTypeParam) => params
        case List(tparams, params)
            if tparams.forall(_.isTypeParam) && !params.exists(_.isTypeParam) =>
          params
        case _ => Nil

    val fields = sym.caseFields

    // a class nested in a (non-module) class needs an outer instance the factory cannot supply;
    // module- and package-nested (or expansion-site-local) classes construct with a plain `new`
    def constructibleOwner(owner: Symbol): Boolean =
      owner == Symbol.noSymbol
        || owner.isPackageDef
        || (if owner.isClassDef then
              owner.flags.is(Flags.Module) && constructibleOwner(owner.maybeOwner)
            else true)

    val isSimpleConstructible =
      sym.isClassDef
        && sym.flags.is(Flags.Case)
        && !sym.flags.is(Flags.Module)
        && !sym.flags.is(Flags.Abstract)
        && constructibleOwner(sym.maybeOwner)
        && ctor != Symbol.noSymbol
        && !ctor.flags.is(Flags.Private)
        && !ctor.flags.is(Flags.Protected)
        && fields.nonEmpty
        && termParams.length == fields.length

    if !isSimpleConstructible then fallback()
    else
      // Resolves opaque aliases to their underlying type. Outside its defining scope an opaque
      // type neither dealiases nor compares equal to its underlying, but its erasure — and
      // therefore the typed slot a field of that type lives in — is decided by the underlying.
      // Anything unresolvable (e.g. an opaque over an abstract type) stays as is and takes the
      // ref slot.
      def dealiasOpaques(tp: TypeRepr): TypeRepr =
        tp.dealias match
          case ref: TypeRef if ref.isOpaqueAlias =>
            dealiasOpaques(ref.translucentSuperType)
          case AppliedType(ref: TypeRef, args) if ref.isOpaqueAlias =>
            dealiasOpaques(ref.translucentSuperType.appliedTo(args))
          case other => other

      // `term.asInstanceOf[target]`: re-types a typed slot pull (or field select) between an
      // opaque alias and its underlying type; both sides erase identically, so no cast or box
      // survives into bytecode
      def castTo(term: Term, target: TypeRepr): Term =
        TypeApply(Select.unique(term, "asInstanceOf"), List(Inferred(target)))

      def pullArg(slots: Expr[BuilderSlots], fieldType: TypeRepr, index: Int): Term =
        val ft                     = fieldType.dealias
        val underlying             = dealiasOpaques(ft)
        val idx                    = Expr(index)
        val typedPull: Term | Null =
          if underlying =:= TypeRepr.of[Int] then '{ $slots.getInt($idx) }.asTerm
          else if underlying =:= TypeRepr.of[Long] then '{ $slots.getLong($idx) }.asTerm
          else if underlying =:= TypeRepr.of[Float] then '{ $slots.getFloat($idx) }.asTerm
          else if underlying =:= TypeRepr.of[Double] then '{ $slots.getDouble($idx) }.asTerm
          else if underlying =:= TypeRepr.of[Boolean] then '{ $slots.getBoolean($idx) }.asTerm
          else if underlying =:= TypeRepr.of[Char] then '{ $slots.getChar($idx) }.asTerm
          else if underlying =:= TypeRepr.of[String] then '{ $slots.getString($idx) }.asTerm
          else null
        if typedPull == null then
          ft.asType match
            case '[t] => '{ $slots.getRef($idx).asInstanceOf[t] }.asTerm
        else if ft =:= underlying then typedPull.nn
        else castTo(typedPull.nn, ft)

      def construct(slots: Expr[BuilderSlots]): Expr[P] =
        val args = fields.zipWithIndex.map { (field, index) =>
          pullArg(slots, tpe.memberType(field).widen, index)
        }
        val select  = Select(New(Inferred(tpe)), ctor)
        val applied = tpe match
          case AppliedType(_, targs) => Apply(TypeApply(select, targs.map(Inferred(_))), args)
          case _                     => Apply(select, args)
        applied.asExprOf[P]

      // The write-side dual of `construct`: selects the field at `index` directly off the product
      // with a precise signature, so a primitive field is never boxed on its way to the renderer.
      // Only fields whose type matches `F` exactly — or is an opaque alias of it — are dispatched;
      // anything else falls back to the boxing Product path (the renderer never asks for a
      // mismatched kind, this is a safety net).
      def typedField[F: Type](value: Expr[Any], index: Expr[Int]): Expr[F] =
        val target            = TypeRepr.of[F]
        val fallback: Expr[F] =
          '{ $value.asInstanceOf[Product].productElement($index).asInstanceOf[F] }
        fields.zipWithIndex
          .flatMap { (field, fieldIndex) =>
            val ft = tpe.memberType(field).widen.dealias
            if !(dealiasOpaques(ft) =:= target) then None
            else
              val selected = Select('{ $value.asInstanceOf[P] }.asTerm, field)
              val typed    =
                if ft =:= target then selected.asExprOf[F]
                else castTo(selected, target).asExprOf[F]
              Some((fieldIndex, typed))
          }
          .foldRight(fallback) { case ((fieldIndex, selected), rest) =>
            '{ if $index == ${ Expr(fieldIndex) } then $selected else $rest }
          }

      '{
        new TypedFactory.OfProduct[P]:
          def fromSlots(slots: BuilderSlots): P                         = ${ construct('slots) }
          override def stringFieldValue(value: Any, index: Int): String =
            ${ typedField[String]('value, 'index) }
          override def charFieldValue(value: Any, index: Int): Char =
            ${ typedField[Char]('value, 'index) }
          override def intFieldValue(value: Any, index: Int): Int =
            ${ typedField[Int]('value, 'index) }
          override def longFieldValue(value: Any, index: Int): Long =
            ${ typedField[Long]('value, 'index) }
          override def floatFieldValue(value: Any, index: Int): Float =
            ${ typedField[Float]('value, 'index) }
          override def doubleFieldValue(value: Any, index: Int): Double =
            ${ typedField[Double]('value, 'index) }
          override def booleanFieldValue(value: Any, index: Int): Boolean =
            ${ typedField[Boolean]('value, 'index) }
      }
