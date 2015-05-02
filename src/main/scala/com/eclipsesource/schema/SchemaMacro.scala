package com.eclipsesource.schema

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object SchemaMacro {

  def deriveSchema[T]: QBClass = macro deriveSchema_impl[T]

  def deriveSchema_impl[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[QBClass] = {
    import c.universe._

    val seqSym = c.typeOf[Seq[Any]].typeSymbol

    def qbType(universeType: c.universe.Type, typeAsString: String): QBType = {
      typeAsString match {
        case "String"     => QBStringImpl(Set())
        case "Int"        => QBIntegerImpl(Set())
        case "Integer"    => QBIntegerImpl(Set())
        case "Double"     => QBNumberImpl(Set())
        case "BigDecimal" => QBNumberImpl(Set())
        case "Boolean"   => QBBooleanImpl(Set())
        case arr if universeType.baseClasses.contains(seqSym) =>
          val itemType = qbType(universeType, universeType.resultType.typeArgs.head.toString)
          QBArray(() => itemType, None)
        case cls =>

          val fields = universeType.decls.collectFirst {
            case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
          }.get.paramLists.head

          val attributes = fields.map { field =>
            val name = field.name
            val returnType = universeType.decl(name).typeSignature
            val attrType = qbType(returnType, returnType.resultType.toString)
            name.toString -> attrType
//            QBAttribute(name.toString, attrType)
          }
          obj(attributes:_*)
//          QBClass(attributes.map(attr =>
//            attr.copy(qbType = attr.qbType match {
//              case obj: QBClass => obj.copy(parent = Some(this))
//              case t => t
//            })
//          ), None)
      }
    }

    implicit val qbStringLiftable = Liftable[QBStringImpl] { s =>
      q"${symbolOf[QBStringImpl].companion}()"
    }

    implicit val qbIntegerLiftable = Liftable[QBIntegerImpl] { i =>
      q"${symbolOf[QBIntegerImpl].companion}()"
    }
    implicit val qbNumberLiftable = Liftable[QBNumberImpl] { n =>
      q"${symbolOf[QBNumberImpl].companion}()"
    }
    implicit val qbBooleanLiftable = Liftable[QBBooleanImpl] { b =>
      q"${symbolOf[QBBooleanImpl].companion}()"
    }
    lazy implicit val qbArrayLiftable: Liftable[QBArray] = Liftable[QBArray] { (arr: QBArray) =>
      val lifted = qbLiftable(arr.items)
      q"${symbolOf[QBArray].companion}($lifted)"
    }
    lazy implicit val qbClassLiftable: Liftable[QBClass] = Liftable[QBClass] { cls =>
      val lifted = cls.properties.map(attr => attr.name -> qbLiftable(attr.qbType)).toList
      q"""${symbolOf[QBClass].companion}(List(..$lifted))"""
    }

    lazy implicit val qbLiftable = Liftable[QBType] {
      case s: QBStringImpl => qbStringLiftable(s)
      case i: QBIntegerImpl => qbIntegerLiftable(i)
      case b: QBBooleanImpl => qbBooleanLiftable(b)
      case n: QBNumberImpl => qbNumberLiftable(n)
      case a: QBArray => qbArrayLiftable(a)
      case c: QBClass => qbClassLiftable(c)
    }

    val weakType = weakTypeOf[T]

    val fields = weakType.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val schemaFields = fields.map { field =>
      val name = field.name
      val decodedName = name.decodedName.toString
      val returnType = weakType.decl(name).typeSignature
      val attrType = qbType(returnType, returnType.resultType.toString)
      q"($decodedName, $attrType)"
    }

    c.Expr[QBClass] {
      q"""${symbolOf[QBClass].companion}(List(..$schemaFields))"""
    }
  }

}
