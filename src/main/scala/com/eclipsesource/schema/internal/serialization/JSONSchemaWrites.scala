package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json._

trait JSONSchemaWrites {

  implicit def schemaTypeWriter: Writes[SchemaType] = Writes[SchemaType] {
    case s: SchemaString       => stringWriter.writes(s)
    case i: SchemaInteger      => integerWriter.writes(i)
    case n: SchemaNumber       => numberWriter.writes(n)
    case b: SchemaBoolean      => booleanWriter.writes(b)
    case t: SchemaTuple        => tupleWriter.writes(t)
    case a: SchemaArray        => arrayWriter.writes(a)
    case o: SchemaObject       => objectWriter.writes(o)
    case n: SchemaNull         => nullWriter.writes(n)
    case n: CompoundSchemaType => compoundWriter.writes(n)
    case v: SchemaValue        => v.value
  }

  lazy val compoundWriter: Writes[CompoundSchemaType] = OWrites[CompoundSchemaType] { compound =>
    // TODO: cast
    compound.alternatives.map(schemaTypeWriter.writes)
      .foldLeft(Json.obj("type" -> Seq()))((o, json) => {
        val typesSoFar = (o \ "type").as[JsArray]
        val types = json \ "type" match {
          case JsDefined(t) => typesSoFar :+ t
          case _ => typesSoFar
        }
        o ++ json.asInstanceOf[JsObject] ++ Json.obj("type" -> types)
      })
  }

  lazy val nullWriter: Writes[SchemaNull] = OWrites[SchemaNull] { n =>
    anyConstraintWriter.writes(n.constraints.any)
  }

  implicit val booleanWriter: Writes[SchemaBoolean] = OWrites[SchemaBoolean] { b =>
    Json.obj("type" -> "boolean")
      .deepMerge(anyConstraintWriter.writes(b.constraints.any))
  }

  implicit val stringWriter: Writes[SchemaString] = OWrites[SchemaString] { s =>
    val stringConstraints = stringConstraintWriter.writes(s.constraints)
    if (stringConstraints.fields.isEmpty) Json.obj("type" -> "string")
    else stringConstraints
  }

  implicit val integerWriter: Writes[SchemaInteger] = OWrites[SchemaInteger] { i =>
    val integerConstraints = numberConstraintWriter.writes(i.constraints)
    if (integerConstraints.fields.isEmpty) Json.obj("type" -> "integer")
    else integerConstraints
  }

  implicit val numberWriter: Writes[SchemaNumber] = OWrites[SchemaNumber] { num =>
    val numberConstraints = numberConstraintWriter.writes(num.constraints)
    if (numberConstraints.fields.isEmpty) Json.obj("type" -> "number")
    else numberConstraints
  }

  implicit val arrayWriter: Writes[SchemaArray] = Writes[SchemaArray] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.item)
    ) ++ arrayConstraintWriter.writes(arr.constraints) ++
      arr.otherProps.map(obj =>
        JsObject(obj.properties.map(attr => attr.name -> Json.toJson(attr.schemaType)))
      ).getOrElse(Json.obj())
  }

  implicit val tupleWriter: Writes[SchemaTuple] = Writes[SchemaTuple] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.items)
    ) ++ arrayConstraintWriter.writes(arr.constraints)
  }

  // TODO: default is missing
  // TODO
  // 		"jsonReference": {
  //  "$ref": {
  //    "type": "string"
  //  },
  //  "additionalProperties": false,
  //  "required": ["$ref"],
  //  "type": "object"
  // }
  implicit val objectWriter: Writes[SchemaObject] = OWrites[SchemaObject] { obj =>

    val props = obj.properties.map(attr => attr.name -> Json.toJson(attr.schemaType))
    val remainingProps = obj.remainingsProps.map(attr => attr.name -> Json.toJson(attr.schemaType))

    // TODO: only write none empty seq of properties
    val o = (if (props.nonEmpty) Json.obj("properties" -> JsObject(props)) else Json.obj()).deepMerge(JsObject(remainingProps))

    // check if $ref exists
    val maybeRef = obj.properties.find(_.name == Keywords.Object.Ref)
    val jsonObj = maybeRef
      .map(ref =>
        Json.obj(Keywords.Object.Ref -> Json.toJson(ref.schemaType))
      ).getOrElse(o)

    jsonObj.deepMerge(objectConstraintWriter.writes(obj.constraints))
  }

  lazy val objectConstraintWriter: OWrites[ObjectConstraints] = OWrites[ObjectConstraints] {
    constraints =>
      asJsObject(Keywords.Object.AdditionalProperties, constraints.additionalProps) ++
      asJsObject(Keywords.Object.Dependencies, constraints.dependencies) ++
      asJsObject(Keywords.Object.MaxProperties, constraints.maxProperties) ++
      asJsObject(Keywords.Object.MinProperties, constraints.minProperties) ++
      asJsObject(Keywords.Object.PatternProperties, constraints.patternProps) ++
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
      asJsObject(Keywords.String.Format, stringConstraints.format) ++
      anyConstraintWriter.writes(stringConstraints.any)
  }

  lazy val anyConstraintWriter: OWrites[AnyConstraint] = OWrites[AnyConstraint] {
    anyConstraint =>
      asJsObject(Keywords.Any.Type, anyConstraint.schemaTypeAsString) ++
      asJsObject(Keywords.Any.Id, anyConstraint.id) ++
      asJsObject(Keywords.Any.AllOf, anyConstraint.allOf) ++
      asJsObject(Keywords.Any.AnyOf, anyConstraint.anyOf) ++
      asJsObject(Keywords.Any.OneOf, anyConstraint.oneOf) ++
      asJsObject(Keywords.Any.Definitions, anyConstraint.definitions) ++
      asJsObject(Keywords.Any.Enum, anyConstraint.enum) ++
      asJsObject(Keywords.Any.Description, anyConstraint.description)
  }

  private def asJsObject[A : Writes](key: String, opt: Option[A]): JsObject = {
    opt.fold(emptyObject)(value => Json.obj(key -> value))
  }

  private def emptyObject = Json.obj()
}
