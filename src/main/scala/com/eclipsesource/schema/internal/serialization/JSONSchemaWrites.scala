package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json._

trait JSONSchemaWrites {

  implicit def schemaTypeWriter: Writes[SchemaType] = Writes[SchemaType] {
    case s: SchemaString => stringWriter.writes(s)
    case i: SchemaInteger => integerWriter.writes(i)
    case n: SchemaNumber => numberWriter.writes(n)
    case b: SchemaBoolean => booleanWriter.writes(b)
    case t: SchemaTuple => tupleWriter.writes(t)
    case a: SchemaArray => arrayWriter.writes(a)
    case o: SchemaObject => objectWriter.writes(o)
    case r: SchemaRef => refWriter.writes(r)
    case c: SchemaStringConstant => JsString(c.str)
    case c: SchemaBooleanConstant => constantWriter.writes(c)
    case a: SchemaArrayConstant => Json.toJson(a.seq)
    case n: SchemaNull => nullWriter.writes(n)
    case n: CompoundSchemaType => compoundWriter.writes(n)
  }

  lazy val compoundWriter: Writes[CompoundSchemaType] = OWrites[CompoundSchemaType] { compound =>
    // TODO: cast
    compound.alternatives.map(c => schemaTypeWriter.writes(c)).foldLeft(Json.obj())((o, json) => o ++ json.asInstanceOf[JsObject])
  }

  lazy val nullWriter: Writes[SchemaNull] = OWrites[SchemaNull] { nll =>
    Json.obj("type" -> "null")
  }

  implicit val booleanWriter: Writes[SchemaBoolean] = OWrites[SchemaBoolean] { bool =>
    Json.obj("type" -> "boolean")
  }

  implicit val stringWriter: Writes[SchemaString] = OWrites[SchemaString] { str =>
    Json.obj("type" -> "string") ++ stringConstraintWriter.writes(str.constraints)
  }

  implicit val integerWriter: Writes[SchemaInteger] = OWrites[SchemaInteger] { int =>
    Json.obj("type" -> "integer") ++ numberConstraintWriter.writes(int.constraints)
  }

  implicit val numberWriter: Writes[SchemaNumber] = OWrites[SchemaNumber] { num =>
   numberConstraintWriter.writes(num.constraints)
  }

  implicit val arrayWriter: Writes[SchemaArray] = Writes[SchemaArray] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.items)
    ) ++ arrayConstraintWriter.writes(arr.constraints)

  }

  implicit val tupleWriter: Writes[SchemaTuple] = Writes[SchemaTuple] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.items)
    ) ++ arrayConstraintWriter.writes(arr.constraints)
  }

  implicit val refWriter: Writes[SchemaRef] = Writes[SchemaRef] { ref =>
    if (ref.isAttribute) {
      JsString(ref.pointer.path)
    } else {
      Json.obj("$ref" -> JsString(ref.pointer.path))
    }
  }

  val constantWriter: Writes[SchemaBooleanConstant] = Writes[SchemaBooleanConstant] { const =>
    JsBoolean(const.bool)
  }

  implicit val objectWriter: Writes[SchemaObject] = OWrites[SchemaObject] {
    obj => {
      val o = Json.obj(
        "type" -> "object",
        "properties" -> JsObject(obj.properties.map(attr => attr.name -> Json.toJson(attr.schemaType)))
      )

      o.deepMerge(objectConstraintWriter.writes(obj.constraints))
    }
  }

  lazy val objectConstraintWriter: OWrites[ObjectConstraints] = OWrites[ObjectConstraints] {
    constraints =>
      asJsObject(Keywords.Object.AdditionalProperties, constraints.additionalProps) ++
      asJsObject(Keywords.Object.PatternProperties, constraints.patternProps) ++
      asJsObject(Keywords.Object.Dependencies, constraints.dependencies) ++
      asJsObject(Keywords.Object.Required, constraints.required) ++
      anyConstraintWriter.writes(constraints.any)
  }

  lazy val arrayConstraintWriter: OWrites[ArrayConstraints] = OWrites[ArrayConstraints] {
    constraints =>
      asJsObject(Keywords.Array.AdditionalItems, constraints.additionalItems) ++
      asJsObject(Keywords.Array.MaxItems, constraints.maxItems) ++
      asJsObject(Keywords.Array.MinItems, constraints.minItems) ++
      asJsObject(Keywords.Array.UniqueItems, constraints.unique) ++
      anyConstraintWriter.writes(constraints.any)
  }

  lazy val numberConstraintWriter: OWrites[NumberConstraints] = OWrites[NumberConstraints] {
    constraints =>
      asJsObject(Keywords.Any.Type, Some(Json.obj("type" -> "number")))
      constraints.max.fold(emptyObject)(max => max.isExclusive match {
        case Some(isExclusive) => Json.obj(Keywords.Number.Max -> max.max, Keywords.Number.ExclusiveMax -> isExclusive)
        case _ => Json.obj(Keywords.Number.Max -> max.max)
      }) ++
      constraints.min.fold(emptyObject)(min => min.isExclusive match {
        case Some(isExclusive) => Json.obj(Keywords.Number.Min -> min.min, Keywords.Number.ExclusiveMin -> isExclusive)
        case _ => Json.obj(Keywords.Number.Min -> min.min)
      }) ++
      constraints.multipleOf.fold(emptyObject)(multipleOf =>
        Json.obj(Keywords.Number.MultipleOf -> multipleOf)
      ) ++ anyConstraintWriter.writes(constraints.any)
  }

  lazy val stringConstraintWriter: OWrites[StringConstraints] = OWrites[StringConstraints] {
    stringConstraints =>
      asJsObject(Keywords.String.MinLength, stringConstraints.minLength) ++
      asJsObject(Keywords.String.MaxLength, stringConstraints.maxLength) ++
      asJsObject(Keywords.String.Pattern, stringConstraints.pattern) ++
      anyConstraintWriter.writes(stringConstraints.any)
  }

  lazy val anyConstraintWriter: OWrites[AnyConstraint] = OWrites[AnyConstraint] {
    anyConstraint =>
      asJsObject(Keywords.Any.AllOf, anyConstraint.allOf) ++
      asJsObject(Keywords.Any.AnyOf, anyConstraint.anyOf) ++
      asJsObject(Keywords.Any.OneOf, anyConstraint.oneOf) ++
      asJsObject(Keywords.Any.Definitions, anyConstraint.definitions) ++
      asJsObject(Keywords.Any.Enum, anyConstraint.enum)
  }

  private def asJsObject[A : Writes](key: String, opt: Option[A]): JsObject = {
    opt.fold(emptyObject)(value => Json.obj(key -> value))
  }

  private def emptyObject = Json.obj()
}
