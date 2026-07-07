package scalanotation.macros.internal

import scala.annotation.publicInBinary
import scala.compiletime
import scala.deriving.Mirror
import scala.quoted.*

@publicInBinary
private[scalanotation] object DefaultValueMacros:

  /** Gathers `P`'s parameterless constructor default values by field name, evaluated once at the
    * call site. Defaults that take parameters (they reference earlier constructor parameters or the
    * type's type parameters) cannot be gathered and are omitted.
    */
  inline def productDefaults[P]: Map[String, AnyRef] =
    ${ productDefaultsImpl[P] }

  /** gathers [[productDefaults]] per structured sum case, keyed by case label */
  inline def caseDefaults[Labels <: Tuple, Cases <: Tuple]: Map[String, Map[String, AnyRef]] =
    inline compiletime.erasedValue[Labels] match
      case _: EmptyTuple        => Map.empty
      case _: (label *: labels) =>
        inline compiletime.erasedValue[Cases] match
          case _: (kase *: cases) =>
            addCaseDefaults[label, kase](caseDefaults[labels, cases])

  inline def addCaseDefaults[Label, Case](
      rest: Map[String, Map[String, AnyRef]]
  ): Map[String, Map[String, AnyRef]] =
    val defaults = productDefaults[Case]
    if defaults.isEmpty then rest
    else rest.updated(compiletime.constValue[Label].asInstanceOf[String], defaults)

  def productDefaultsImpl[P: Type](using Quotes): Expr[Map[String, AnyRef]] =
    import quotes.reflect.*

    val tpe       = TypeRepr.of[P].dealias
    val sym       = tpe.typeSymbol
    val companion = sym.companionModule

    if !sym.isClassDef || companion == Symbol.noSymbol then '{ Map.empty }
    else
      val fields = sym.caseFields
      val pairs  = fields.zipWithIndex.flatMap { (field, index) =>
        companion.methodMember(s"$$lessinit$$greater$$default$$${index + 1}") match
          case List(defaultMethod) if defaultMethod.paramSymss.isEmpty =>
            val call = Select(Ref(companion), defaultMethod).asExpr
            Some('{ (${ Expr(field.name) }, $call.asInstanceOf[AnyRef]) })
          case _ => None // no default, or one that takes parameters
      }
      if pairs.isEmpty then '{ Map.empty } else '{ Map(${ Varargs(pairs) }*) }
