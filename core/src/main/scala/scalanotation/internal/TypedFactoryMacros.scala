package scalanotation.internal

import scalanotation.BuilderSlots
import scalanotation.TypedFactory

import scala.annotation.publicInBinary
import scala.compiletime
import scala.deriving.Mirror
import scala.quoted.*

@publicInBinary
private[scalanotation] object TypedFactoryMacros:

  /** runtime fallback: builds via the mirror, boxing on demand through the Product interface */
  def fromProductFactory[P](m: Mirror.ProductOf[P]): TypedFactory =
    slots => m.fromProduct(slots)

  /** Derives a [[TypedFactory]] that invokes `P`'s primary constructor with each argument pulled
    * from the matching typed slot, so primitive fields are never boxed. Shapes that cannot be
    * constructed directly (inner classes, non-public or curried constructors) fall back to
    * `Mirror.fromProduct` over the slots.
    */
  inline def productFactory[P](using m: Mirror.ProductOf[P]): TypedFactory =
    ${ productFactoryImpl[P] }

  /** derives a [[TypedFactory]] per structured product case, keyed by case label; nullary cases
    * decode to a fixed value and need no factory
    */
  inline def caseFactories[Labels <: Tuple, Cases <: Tuple]: Map[String, TypedFactory] =
    inline compiletime.erasedValue[Labels] match
      case _: EmptyTuple        => Map.empty
      case _: (label *: labels) =>
        inline compiletime.erasedValue[Cases] match
          case _: (kase *: cases) =>
            addCaseFactory[label, kase](caseFactories[labels, cases])

  inline def addCaseFactory[Label, Case](
      rest: Map[String, TypedFactory]
  ): Map[String, TypedFactory] =
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

  def productFactoryImpl[P: Type](using Quotes): Expr[TypedFactory] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[P].dealias
    val sym = tpe.typeSymbol

    def fallback(): Expr[TypedFactory] =
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
      def pullArg(slots: Expr[BuilderSlots], fieldType: TypeRepr, index: Int): Term =
        val ft  = fieldType.dealias
        val idx = Expr(index)
        if ft =:= TypeRepr.of[Int] then '{ $slots.getInt($idx) }.asTerm
        else if ft =:= TypeRepr.of[Long] then '{ $slots.getLong($idx) }.asTerm
        else if ft =:= TypeRepr.of[Float] then '{ $slots.getFloat($idx) }.asTerm
        else if ft =:= TypeRepr.of[Double] then '{ $slots.getDouble($idx) }.asTerm
        else if ft =:= TypeRepr.of[Boolean] then '{ $slots.getBoolean($idx) }.asTerm
        else if ft =:= TypeRepr.of[Char] then '{ $slots.getChar($idx) }.asTerm
        else if ft =:= TypeRepr.of[String] then '{ $slots.getString($idx) }.asTerm
        else
          ft.asType match
            case '[t] => '{ $slots.getRef($idx).asInstanceOf[t] }.asTerm

      def construct(slots: Expr[BuilderSlots]): Expr[Any] =
        val args = fields.zipWithIndex.map { (field, index) =>
          pullArg(slots, tpe.memberType(field).widen, index)
        }
        val select  = Select(New(Inferred(tpe)), ctor)
        val applied = tpe match
          case AppliedType(_, targs) => Apply(TypeApply(select, targs.map(Inferred(_))), args)
          case _                     => Apply(select, args)
        applied.asExprOf[Any]

      '{
        new TypedFactory:
          def fromSlots(slots: BuilderSlots): Any = ${ construct('slots) }
      }
