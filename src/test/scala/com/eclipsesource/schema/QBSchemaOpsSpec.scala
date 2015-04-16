package com.eclipsesource.schema

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
object QBSchemaOpsSpec extends Specification {

  "Schema operations" should {

    val schema = qbClass(
      "o" -> qbClass(
        "n" -> qbClass(
          "s" -> qbString(minLength(5)),
          "t" -> qbInteger,
          "v" -> qbNumber)))

    "add an attribute" in {
      val updatedSchema = schema ++ ("o.n", "e" -> qbNumber)
      updatedSchema.resolve[QBNumber]("o.n.e") must beAnInstanceOf[QBNumber]
    }

    "add an attribute via the attribute name" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema ++ ("e" -> qbNumber)
      updatedSchema.attributes.size must beEqualTo(2)
    }

    "add multiple attributes" in {
      val updatedSchema = schema ++ ("o", "e" -> qbNumber, "xx" -> qbString)
      val resolved = updatedSchema.resolve[QBClass]("o")
      resolved.attributes.size must beEqualTo(3)
    }

    "remove an attribute" in {
      val updatedSchema = schema - "o.n"
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "remove an attribute via the attribute name" in {
      val schema = qbClass("i" -> qbString, "j" -> qbString)
      val updatedSchema = schema - "i"
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "remove multiple attributes directly" in {
      val schema = qbClass("i" -> qbString, "j" -> qbString, "x" -> qbString)
      val updatedSchema = schema -- ("i", "j")
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "remove multiple attributes" in {
      val updatedSchema = schema -- ("o.n.s", "o.n.t")
      val resolved = updatedSchema.resolve[QBClass]("o.n")
      resolved.attributes.size must beEqualTo(1)
    }

    "make an attribute optional" in {
      val updatedSchema = schema ? "o.n"
      val resolved = updatedSchema.resolve[QBClass]("o")
      val resolvedAttribute = resolved.attributes.find(_.name == "n")
      // TODO: provide convenience methods via implicit class
      resolvedAttribute.get.annotations.find(_.isInstanceOf[QBOptionalAnnotation]) must beSome
    }

    "make multiple attribute optional" in {
      val updatedSchema = schema ? ("o.n.s", "o.n.t")
      val resolved = updatedSchema.resolve[QBClass]("o.n")
      val n = resolved.attributes.find(_.name == "s")
      val t = resolved.attributes.find(_.name == "t")
      n.get.annotations.find(_.isInstanceOf[QBOptionalAnnotation]) must beSome
      t.get.annotations.find(_.isInstanceOf[QBOptionalAnnotation]) must beSome
    }

    "make an attribute read-only" in {
      val updatedSchema = schema readOnly "o.n"
      val resolved = updatedSchema.resolve[QBClass]("o")
      val n = resolved.attributes.find(_.name == "n")
      n.get.annotations.find(_.isInstanceOf[QBReadOnlyAnnotation]) must beSome
    }

    "make multiple attributes read-only" in {
      val updatedSchema = schema readOnly ("o.n.s", "o.n.t")
      val resolved = updatedSchema.resolve[QBClass]("o.n")
      val n = resolved.attributes.find(_.name == "s")
      val t = resolved.attributes.find(_.name == "t")
      n.get.annotations.find(_.isInstanceOf[QBReadOnlyAnnotation]) must beSome
      t.get.annotations.find(_.isInstanceOf[QBReadOnlyAnnotation]) must beSome
    }

    "make sub-schema read-only" in {
      val s  = schema readOnly "o"
      val subSchema = s.resolve[QBClass]("o.n")
      val updatedSchema = s readOnly subSchema
      val resolved = updatedSchema.resolve[QBClass]("o.n")
      resolved forAll (_.annotations.exists(_.isInstanceOf[QBReadOnlyAnnotation])) must beTrue
    }

    "throw a RuntimeException in case a path does not exist while subtracting an attribute" in {
      schema - "o.x" must throwA[RuntimeException]("field.does.not.exist")
    }

    "throw a RuntimeException in case a path does not exists while adding an attribute" in {
      schema + ("o.x", "e" -> qbNumber) must throwA[RuntimeException]("field.does.not.exist")
    }

    "add an attribute" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema + ("e" -> qbNumber)
      updatedSchema.attributes.size must beEqualTo(2)
    }

    "rename an attribute via a path" in {
      val updatedSchema = schema rename ("o.n", "e")
      val resolved = updatedSchema.resolve[QBClass]("o.e")
      resolved must beAnInstanceOf[QBClass]
    }

    "renamed an attribute via the attribute name" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema rename ("x", "y")
      val resolved = updatedSchema.resolve[QBString]("y")
      resolved must beAnInstanceOf[QBString]
    }

    "make an attribute optional" in {
      val schema = qbClass("x" -> qbString)
      val updatedSchema = schema ? "x"
      val attr = updatedSchema.attributes.find(_.name == "x")
      attr must beSome.which(_.annotations.exists(_.isInstanceOf[QBOptionalAnnotation]))
    }

    "throw a RuntimeException in case a attribute does not exist while trying to rename an attribute" in {
      schema rename ("o.x", "A") must throwA[RuntimeException]("field.does.not.exist")
    }

    "keep attributes specified via a path" in {
      val updatedSchema = schema keep ("o.n", List("s"))
      val resolved = updatedSchema.resolve[QBClass]("o.n")
      resolved.attributes.size must beEqualTo(1)
    }

    "keep attribute via the attribute name" in {
      val updatedSchema = schema keep ("", List("o"))
      val resolved = updatedSchema.resolve[QBClass]("o")
      resolved.attributes.size must beEqualTo(1)
    }

    "throw a RuntimeException in case a attribute does not exist while trying to rename an attribute" in {
      schema keep ("o.x", List("s")) must throwA[RuntimeException]
    }

    "keep attributes via attribute name" in {
      val schema = qbClass(
        "a" -> qbInteger,
        "b" -> qbString)
      val updatedSchema = schema keep "a"
      updatedSchema.attributes.size must beEqualTo(1)
    }

    "throw a RuntimeException when trying to remove a non existant attribute " in {
      val schema = qbClass("o" -> qbString)
      val updatedSchema = schema - "o"
      updatedSchema.resolve[QBClass]("o") must throwA[RuntimeException]
    }

    "add classes to an existing class" in {
      val schema = qbClass("o" -> qbString)
      val updated = schema ++ qbClass(
        "a" -> qbInteger)
      updated.attributes must have size 2
    }

    "override an existing attribute" in {
      val schema = qbClass("o" -> qbString)
      val temp = schema ++ qbClass("a" -> qbInteger)
      val updated = temp + ("a" -> qbString)
      updated.attributes must have size 2
      updated.attributes.exists(_.qbType.isInstanceOf[QBString]) must beTrue
    }

    "rename an attribute by type" in {
      val schema = qbClass("o" -> qbString)
      val updated = schema.updateAttributesByPredicate(_.qbType.isInstanceOf[QBString])(attr => QBAttribute("o2", attr.qbType)).asInstanceOf[QBClass]
      updated.attributes.exists(_.name == "o2") must beTrue
    }

    "rename an attribute" in {
      val schema = qbClass("o" -> qbString)
      val updated = schema.updateAttributesByPredicate(_.name == "o")(attr => QBAttribute("o2", attr.qbType)).asInstanceOf[QBClass]
      updated.attributes.exists(_.name == "o2") must beTrue
    }

    "rename an class attribute" in {
      val schema = qbClass("o" -> qbClass("s" -> qbString))
      val updated = schema.updateAttributesByPredicate(_.name == "o")(attr => QBAttribute("o2", attr.qbType)).asInstanceOf[QBClass]
      updated.attributes.exists(_.name == "o2") must beTrue
    }

    "add attribute" in {
      val schema = qbClass(
        "o" -> qbClass(
          "n" -> optional(qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger,
            "v" -> qbNumber))))

      val updated = updateByPath[QBClass](schema, List("o", "n"), obj => {
        val fields = obj.attributes
        QBClassImpl(fields :+ QBAttribute("vv", qbNumber))
      })

      val buildDesc = resolve(List("o", "n", "vv"), updated)
      buildDesc._2 must beAnInstanceOf[QBNumber]
    }

    "add attribute via updateIf" in {
      val schema = qbClass(
        "o" -> qbClass(
          "n" -> optional(qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger,
            "v" -> qbNumber))))

      // TODO
      val updated = updateIf(schema, { case cls: QBClass =>
        QBClassImpl(cls.attributes :+ QBAttribute("vv", qbNumber))
      }).as[QBClass]

      // TODO: provide convenience method for _2
      val resolved = resolvePath[QBType](updated)(List("o", "n", "vv"))
      resolved must beAnInstanceOf[QBNumber]
    }

    "support equals" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger,
            "v" -> qbNumber)))
      schema must beEqualTo(otherSchema)
      schema.isEquals(otherSchema) must beTrue
      schema.equals(otherSchema) must beTrue
    }

    "emit true when comparing equal schemas" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger,
            "v" -> qbNumber)))
      schema must beEqualTo(otherSchema)
      schema.isEquals(otherSchema) must beTrue
      schema.equals(otherSchema) must beTrue
    }

    "emit true when comparing equal schemas with arrays" in {
      val schemaA = qbClass(
        "a" -> qbString,
        "b" -> optional(qbInteger),
        "c" -> qbClass("d" -> qbBoolean),
        "e" -> qbList(qbInteger)
      )

      val schemaB = qbClass(
        "a" -> qbString,
        "b" -> optional(qbInteger),
        "c" -> qbClass("d" -> qbBoolean),
        "e" -> qbList(qbInteger)
      )
      
      schemaA.isEquals(schemaB) must beTrue
      schemaA.equals(schemaB) must beTrue
    }

    "emit false when comparing different schemas" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> qbInteger)))
      schema.isEquals(otherSchema) must beFalse
      schema.equals(otherSchema) must beFalse
    }

    "emit false when comparing equal schemas but with different annotation" in {
      val otherSchema = qbClass(
        "o" -> qbClass(
          "n" -> qbClass(
            "s" -> qbString(minLength(5)),
            "t" -> optional(qbInteger),
            "v" -> qbNumber)))
      schema.isEquals(otherSchema) must beFalse
      schema.equals(otherSchema) must beFalse
    }

    "determine whether a schema is part of another schema" in {
      val otherSchema = qbClass(
        "n" -> qbClass(
          "s" -> qbString(minLength(5)),
          "t" -> qbInteger,
          "v" -> qbNumber))
      otherSchema.isSubSetOf(schema) must beTrue
    }

    "determine whether a schema is part of itself" in {
      schema.isSubSetOf(schema) must beTrue
    }

    "determine whether a schema is not part of another schema" in {
      val otherSchema = qbClass(
        "n" -> qbClass(
          "s" -> qbString(minLength(5)),
          "t" -> qbInteger,
          "v" -> qbNumber))
      schema.isSubSetOf(otherSchema) must beFalse
    }
  }
}