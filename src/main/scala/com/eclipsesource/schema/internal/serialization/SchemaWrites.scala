package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json._

trait SchemaWrites { self: SchemaVersion =>

  implicit def schemaTypeWriter: Writes[SchemaType] = Writes[SchemaType] {
    case r: SchemaRef          => refWrites.writes(r)
    case s: SchemaString       => stringWrites.writes(s)
    case i: SchemaInteger      => integerWrites.writes(i)
    case n: SchemaNumber       => numberWrites.writes(n)
    case b: SchemaBoolean      => booleanWrites.writes(b)
    case t: SchemaTuple        => tupleWrites.writes(t)
    case a: SchemaArray        => arrayWrites.writes(a)
    case o: SchemaObject       => objectWrites.writes(o)
    case n: SchemaNull         => nullWrites.writes(n)
    case n: CompoundSchemaType => compoundWrites.writes(n)
    case s: SchemaMap          => schemaMapWriter.writes(s)
    case v: SchemaValue        => v.value
    case other => additionalWrites.writes(other)
  }

  def additionalWrites: OWrites[SchemaType] = OWrites[SchemaType](other =>
    throw new RuntimeException(s"Unknown schema type encountered.\n$other")
  )
  def anyConstraintWrites: OWrites[AnyConstraints]
  def stringWrites: OWrites[SchemaString]
  def numberWrites: OWrites[SchemaNumber]
  def integerWrites: OWrites[SchemaInteger]
  def tupleWrites: OWrites[SchemaTuple]
  def arrayWrites: OWrites[SchemaArray]
  def objectWrites: OWrites[SchemaObject]

  lazy val refWrites: Writes[SchemaRef] = OWrites[SchemaRef] { ref =>
    Json.obj("$ref" -> ref.ref.value)
      .deepMerge(anyConstraintWrites.writes(ref.constraints))
  }

  lazy val schemaMapWriter: Writes[SchemaMap] = OWrites[SchemaMap] { schemaMap =>
    val props = schemaMap.members.map(attr => attr.name -> Json.toJson(attr.schemaType))
    Json.obj(schemaMap.name -> JsObject(props))
  }

  lazy val compoundWrites: Writes[CompoundSchemaType] = OWrites[CompoundSchemaType] { compound =>
    // TODO: casts
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

  lazy val nullWrites: Writes[SchemaNull] = Writes[SchemaNull] { n =>
    anyConstraintWrites.writes(n.constraints)
  }

  val booleanWrites: Writes[SchemaBoolean] = OWrites[SchemaBoolean] { b =>
    Json.obj("type" -> "boolean")
      .deepMerge(anyConstraintWrites.writes(b.constraints))
  }

  def asJsObject[A : Writes](key: String, opt: Option[A]): JsObject = {
    opt.fold(emptyJsonObject)(value => Json.obj(key -> value))
  }

  def emptyJsonObject: JsObject = Json.obj()
}