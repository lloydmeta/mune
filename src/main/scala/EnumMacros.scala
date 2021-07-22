import scala.quoted._

object EnumMacros {
  inline def findValuesImpl[T]: IndexedSeq[T] = ${ _findValuesImpl[T] }

  transparent inline def materializeEnumImpl[T] = ${ _materializeEnumImpl[T] }

  private def _findValuesImpl[T: Type](using Quotes): Expr[IndexedSeq[T]] = {
    import quotes.reflect._
    val tType    = TypeRepr.of[T]
    val selfType = This(Symbol.spliceOwner.owner.owner).tpe
    validateType(tType, selfType)
    val subclassSymbols = enclosedSubClassTreesInModule(tType, selfType)
    val subclassExprs   = subclassSymbols.map(v => Ref(v).asExprOf[T])
    '{ IndexedSeq[T](${ Varargs(subclassExprs) }: _*) }
  }

  private def enclosedSubClassTreesInModule(using
      q: Quotes
  )(tType: q.reflect.TypeRepr, enclosingType: q.reflect.TypeRepr): List[q.reflect.Symbol] = {
    import q.reflect._
    enclosingType.typeSymbol.declaredFields.flatMap { member =>
      if (isModule(member)) {
        val memberType = enclosingType.memberType(member)
        if (memberType <:< tType) {
          member :: enclosedSubClassTreesInModule(tType, memberType)
        } else {
          enclosedSubClassTreesInModule(tType, memberType)
        }
      } else {
        Nil
      }
    }
  }

  private def isModule(using Quotes)(s: quotes.reflect.Symbol) = {
    !s.moduleClass.isNoSymbol && !s.isType
  }

  def validateType(using q: Quotes)(tType: q.reflect.TypeRepr, moduleType: q.reflect.TypeRepr) = {
    import q.reflect._
    if (!tType.typeSymbol.flags.is(q.reflect.Flags.Sealed)) {
      report.throwError("You can only use findValues on sealed traits or classes")
    }
    if (moduleType.typeSymbol.moduleClass.isNoSymbol) {
      report.throwError(
        "The enum (i.e. the class containing the case objects and the call to `findValues`) must be an object"
      )
    }
  }

  private def _materializeEnumImpl[T: Type](using q: Quotes) = {
    import q.reflect._
    val tType = TypeRepr.of[T]
    tType match {
      case andType: AndOrType => {
        andType.right match {
          case apType: AppliedType => {
            val lastTypeRef     = apType.args.last
            val companionSymbol = lastTypeRef.typeSymbol.companionModule
            // report.info(s"companionSymbol [${companionSymbol}] [${lastTypeRef}] [${lastTypeRef.isSingleton}]")
            if (companionSymbol.isNoSymbol) {
              report.throwError(
                s"""
                  |
                  |  Could not find the companion object for type $companionSymbol.
                  |
                  |  If you're sure the companion object exists, you might be able to fix this error by annotating the
                  |  value you're trying to find the companion object for with a parent type (e.g. Light.Red: Light).
                  |
                  |  This error usually happens when trying to find the companion object of a hard-coded enum member, and
                  |  is caused by Scala inferring the type to be the member's singleton type (e.g. Light.Red.type instead of
                  |  Light).
                  |
                  |  To illustrate, given an enum:
                  |
                  |  sealed abstract class Light extends EnumEntry
                  |  case object Light extends Enum[Light] {
                  |    val values = findValues
                  |    case object Red   extends Light
                  |    case object Blue  extends Light
                  |    case object Green extends Light
                  |  }
                  |
                  |  and a method:
                  |
                  |  def indexOf[A <: EnumEntry: Enum](entry: A): Int = implicitly[Enum[A]].indexOf(entry)
                  |
                  |  Instead of calling like so: indexOf(Light.Red)
                  |                Call like so: indexOf(Light.Red: Light)
                """.stripMargin
              )
            } else {
              val memberType = tType.memberType(companionSymbol)
              // report.warning(s"memberType [${memberType =:= lastTypeRef}] [${lastTypeRef.show}]")
              Ref(companionSymbol).asExprOf[T]
            }
          }
          case _ =>
            report.throwError("unexpected")
        }
      }
      case _ => {
        report.throwError("meh")
      }
    }

  }
}
