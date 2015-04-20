package com.eclipsesource.schema

import scala.annotation.StaticAnnotation
import scala.collection.SeqLike
import scala.collection.generic.SeqFactory
import scala.language.experimental.macros
import com.eclipsesource.schema._
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
          QBArrayImpl(itemType)
        case cls =>

          val fields = universeType.decls.collectFirst {
            case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
          }.get.paramLists.head

          val attributes = fields.map { field =>
            val name = field.name
            val returnType = universeType.decl(name).typeSignature
            val attrType = qbType(returnType, returnType.resultType.toString)
            QBAttribute(name.toString, attrType)
          }
          QBClassImpl(attributes)
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
    lazy implicit val qbArrayLiftable: Liftable[QBArrayImpl] = Liftable[QBArrayImpl] { (arr: QBArrayImpl) =>
      val lifted = qbLiftable(arr.items)
      q"${symbolOf[QBArrayImpl].companion}($lifted)"
    }
    lazy implicit val qbClassLiftable: Liftable[QBClassImpl] = Liftable[QBClassImpl] { cls =>
      val lifted = cls.attributes.map(attr => attr.name -> qbLiftable(attr.qbType)).toList
      q"""${symbolOf[QBClassImpl].companion}(List(..$lifted))"""
    }

    lazy implicit val qbLiftable = Liftable[QBType] {
      case s: QBStringImpl => qbStringLiftable(s)
      case i: QBIntegerImpl => qbIntegerLiftable(i)
      case b: QBBooleanImpl => qbBooleanLiftable(b)
      case n: QBNumberImpl => qbNumberLiftable(n)
      case a: QBArrayImpl => qbArrayLiftable(a)
      case c: QBClassImpl => qbClassLiftable(c)
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
      q"""${symbolOf[QBClassImpl].companion}(List(..$schemaFields))"""
    }
  }

}
