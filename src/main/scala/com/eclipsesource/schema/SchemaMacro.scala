package com.eclipsesource.schema

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object SchemaMacro {

  def deriveSchema[T]: SchemaObject = macro deriveSchema_impl[T]

  def deriveSchema_impl[T : c.WeakTypeTag](c: blackbox.Context): c.Expr[SchemaObject] = {
    import c.universe._

    val seqSym = c.typeOf[Seq[Any]].typeSymbol

    def schemaType(universeType: c.universe.Type, typeAsString: String): SchemaType = {
      typeAsString match {
        case "String"     => SchemaString()
        case "Int"        => SchemaInteger()
        case "Integer"    => SchemaInteger()
        case "Double"     => SchemaNumber()
        case "BigDecimal" => SchemaNumber()
        case "Boolean"   => SchemaBoolean()
        case arr if universeType.baseClasses.contains(seqSym) =>
          val itemType = schemaType(universeType, universeType.resultType.typeArgs.head.toString)
          SchemaArray(() => itemType)
        case cls =>

          val fields = universeType.decls.collectFirst {
            case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
          }.get.paramLists.head

          val attributes: List[(String, SchemaType)] = fields.map { field =>
            val name = field.name
            val returnType = universeType.decl(name).typeSignature
            val attrType = schemaType(returnType, returnType.resultType.toString)
            name.toString -> attrType
          }

          // TODO annotations missing
          SchemaObject(attributes.map(attr => SchemaAttribute(attr._1, attr._2)))
      }
    }

    implicit val stringLiftable = Liftable[SchemaString] { s =>
      q"${symbolOf[SchemaString].companion}()"
    }

    implicit val integerLiftable = Liftable[SchemaInteger] { i =>
      q"${symbolOf[SchemaInteger].companion}()"
    }
    implicit val numberLiftable = Liftable[SchemaNumber] { n =>
      q"${symbolOf[SchemaNumber].companion}()"
    }
    implicit val booleanLiftable = Liftable[SchemaBoolean] { b =>
      q"${symbolOf[SchemaBoolean].companion}()"
    }
    implicit val nullLiftable = Liftable[SchemaNull] { n =>
      q"${symbolOf[SchemaNull].companion}()"
    }
    implicit val compoundLiftable = Liftable[CompoundSchemaType] { c =>
      q"${symbolOf[CompoundSchemaType].companion}()"
    }
    implicit val booleanConstantLiftable = Liftable[SchemaBooleanConstant] { c =>
      q"${symbolOf[SchemaBooleanConstant].companion}()"
    }
    implicit val arrayConstantLiftable = Liftable[SchemaArrayConstant] { c =>
      q"${symbolOf[SchemaArrayConstant].companion}()"
    }
    implicit val refLiftable = Liftable[SchemaRef] { c =>
      q"${symbolOf[SchemaRef].companion}()"
    }
    implicit val tupleLiftable = Liftable[SchemaTuple] { c =>
      q"${symbolOf[SchemaTuple].companion}()"
    }
    lazy implicit val arrayLiftable: Liftable[SchemaArray] = Liftable[SchemaArray] { (arr: SchemaArray) =>
      val lifted = schemaLiftable(arr.items)
      q"${symbolOf[SchemaArray].companion}($lifted)"
    }
    lazy implicit val classLiftable: Liftable[SchemaObject] = Liftable[SchemaObject] { cls =>
      val lifted = cls.properties.map(attr => attr.name -> schemaLiftable(attr.schemaType)).toList
      q"""${symbolOf[SchemaObject].companion}(List(..$lifted))"""
    }

    lazy implicit val schemaLiftable = Liftable[SchemaType] {
      case s: SchemaString => stringLiftable(s)
      case i: SchemaInteger => integerLiftable(i)
      case b: SchemaBoolean => booleanLiftable(b)
      case n: SchemaNumber => numberLiftable(n)
      case a: SchemaArray => arrayLiftable(a)
      case c: SchemaObject => classLiftable(c)
      case n: SchemaNull => nullLiftable(n)
      case c: CompoundSchemaType => compoundLiftable(c)
      case c: SchemaArrayConstant => arrayConstantLiftable(c)
      case c: SchemaBooleanConstant => booleanConstantLiftable(c)
      case r: SchemaRef => refLiftable(r)
      case t: SchemaTuple => tupleLiftable(t)
    }

    val weakType = weakTypeOf[T]

    val fields = weakType.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val schemaFields = fields.map { field =>
      val name = field.name
      val decodedName = name.decodedName.toString
      val returnType = weakType.decl(name).typeSignature
      val attrType = schemaType(returnType, returnType.resultType.toString)
      q"($decodedName, $attrType)"
    }

    c.Expr[SchemaObject] {
      q"""${symbolOf[SchemaObject].companion}(List(..$schemaFields))"""
    }
  }

}
