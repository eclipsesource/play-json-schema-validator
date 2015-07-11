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
    case c: SchemaBooleanConstant => constantWriter.writes(c)
    case a: SchemaArrayConstant => Json.toJson(a.seq)
    case n: SchemaNull => nullWriter.writes(n)
  }


  lazy val nullWriter: Writes[SchemaNull] = OWrites[SchemaNull] { nll =>
    Json.obj("type" -> "null")
  }

  implicit val booleanWriter: Writes[SchemaBoolean] = OWrites[SchemaBoolean] { bool =>
    Json.obj("type" -> "boolean") ++ booleanConstraintWriter.writes(bool.constraints)
  }

  implicit val stringWriter: Writes[SchemaString] = OWrites[SchemaString] { str =>
    Json.obj("type" -> "string") ++ stringConstraintWriter.writes(str.constraints)
  }

  implicit val integerWriter: Writes[SchemaInteger] = OWrites[SchemaInteger] { int =>
    Json.obj("type" -> "integer") ++ numberConstraintWriter.writes(int.constraints)
  }

  implicit val numberWriter: Writes[SchemaNumber] = OWrites[SchemaNumber] { num =>
    Json.obj("type" -> "number") ++ numberConstraintWriter.writes(num.constraints)
  }

  // TODO: do arrays have additionalitems?
  implicit val arrayWriter: Writes[SchemaArray] = Writes[SchemaArray] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.items)
      // TODO: write any other props, too
    ).deepMerge(
        arr.id.fold(Json.obj())(id => Json.obj("id" -> id))
      ) ++ arrayConstraintWriter.writes(arr.constraints)

  }

  // TODO: duplicate code
  implicit val tupleWriter: Writes[SchemaTuple] = Writes[SchemaTuple] { arr =>
    Json.obj(
      "items" -> Json.toJson(arr.schemaTypes)
    ).deepMerge(
        arr.constraints.additionalItems.fold(Json.obj())(items =>
          Json.obj("additionalItems" -> items)
        )
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

  /**
   * Extensions whose result gets merged into the written property like, for instance, the type:
   */
  def propertyExtensions: List[SchemaAttribute => JsObject] = List()

  /**
   * Extensions which act on a object and produce a result which contains 
   * information about multiple fields. For instance, required property.
   */
  def extensions: List[SchemaObject => JsObject] = List()

  implicit val objectWriter: Writes[SchemaObject] = OWrites[SchemaObject] {
    obj => {
      val o = Json.obj(
        "type" -> "object",
        "properties" -> JsObject(obj.properties.map(attr => attr.name -> Json.toJson(attr.schemaType)))
      )


      val written = o.deepMerge(
        objectConstraintWriter.writes(obj.constraints)
      )

      val written2 = extensions.foldLeft(written)((o, extension) =>
        o.deepMerge(extension(obj)))

      written2
    }
  }

  lazy val objectConstraintWriter: OWrites[ObjectConstraints] = OWrites[ObjectConstraints] {
    constraints =>
      toJsObject(Keywords.Object.AdditionalProperties, constraints.additionalProps) ++
      toJsObject(Keywords.Object.PatternProperties, constraints.patternProps) ++
      toJsObject(Keywords.Object.Dependencies, constraints.dependencies) ++
      toJsObject(Keywords.Object.Required, constraints.required) ++
      anyConstraintWriter.writes(constraints.any)
  }

  lazy val arrayConstraintWriter: OWrites[ArrayConstraints] = OWrites[ArrayConstraints] {
    constraints =>
      toJsObject(Keywords.Array.AdditionalItems, constraints.additionalItems)
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

  lazy val booleanConstraintWriter: OWrites[BooleanConstraints] = OWrites[BooleanConstraints] {
    constraints =>
      // TODO
      Json.obj()
  }

  lazy val stringConstraintWriter: OWrites[StringConstraints] = OWrites[StringConstraints] {
    stringConstraints =>
      stringConstraints.minLength.fold(emptyObject)(minLength =>
        Json.obj(Keywords.String.MinLength -> minLength)
      ) ++
        stringConstraints.maxLength.fold(emptyObject)(maxLength =>
          Json.obj(Keywords.String.MaxLength -> maxLength)
        ) ++
        stringConstraints.format.fold(emptyObject)(format =>
          Json.obj(Keywords.String.Format -> format)
        ) ++
       anyConstraintWriter.writes(stringConstraints.any)
  }

  lazy val anyConstraintWriter: OWrites[AnyConstraint] = OWrites[AnyConstraint] {
    anyConstraint =>
      toJsObject(Keywords.Any.AllOf, anyConstraint.allOf) ++
      toJsObject(Keywords.Any.AnyOf, anyConstraint.anyOf) ++
      toJsObject(Keywords.Any.OneOf, anyConstraint.oneOf) ++
      toJsObject(Keywords.Any.Definitions, anyConstraint.definitions) ++
      toJsObject(Keywords.Any.Enum, anyConstraint.enum)
  }

  private def toJsObject[A : Writes](key: String, opt: Option[A]): JsObject = {
    opt.fold(emptyObject)(value =>
      Json.obj(key -> value)
    )
  }

  private def emptyObject = Json.obj()
}
