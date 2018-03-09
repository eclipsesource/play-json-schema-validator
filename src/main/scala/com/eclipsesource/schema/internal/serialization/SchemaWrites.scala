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
    compound.alternatives
      .map(schemaTypeWriter.writes)
      .foldLeft(Json.obj("type" -> Seq())) {
        case (o, json@JsObject(_)) =>
          val typesSoFar = (o \ "type").as[JsArray]
          val types = json \ "type" match {
            case JsDefined(t) => typesSoFar :+ t
            case _ => typesSoFar
          }
          o ++ json ++ Json.obj("type" -> types)
        case (o, _) => o
      }
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

  object Default {
    // TODO: default is missing
    def objectWrites(objectConstraintWrites: OWrites[ObjectConstraints]): OWrites[SchemaObject] =
      OWrites[SchemaObject] { schemaObj =>
        val props = schemaObj.properties.map(prop => prop.name -> Json.toJson(prop.schemaType))
        val remainingProps = schemaObj.otherProps.map(prop => prop._1 -> Json.toJson(prop._2))

        (if (props.nonEmpty) Json.obj("properties" -> JsObject(props)) else Json.obj()) ++ JsObject(remainingProps) ++
          objectConstraintWrites.writes(schemaObj.constraints)
      }

    def arrayWrites(arrayConstraintWrites: OWrites[ArrayConstraints]): OWrites[SchemaArray] = OWrites[SchemaArray] { arr =>
      Json.obj("items" -> Json.toJson(arr.item)) ++
        arrayConstraintWrites.writes(arr.constraints) ++
        JsObject(arr.otherProps.map(attr => attr._1 -> Json.toJson(attr._2)))
    }

    def tupleWrites(arrayConstraintWrites: OWrites[ArrayConstraints]): OWrites[SchemaTuple] = OWrites[SchemaTuple] { tuple =>
      Json.obj("items" -> Json.toJson(tuple.items)) ++
        arrayConstraintWrites.writes(tuple.constraints) ++
        JsObject(tuple.otherProps.map(attr => attr._1 -> Json.toJson(attr._2)))
    }

    def stringWrites(stringConstraintWrites: OWrites[StringConstraints]): OWrites[SchemaString] =
      OWrites[SchemaString] { s =>
        val stringConstraints = stringConstraintWrites.writes(s.constraints)
        if (stringConstraints.fields.isEmpty) Json.obj("type" -> "string")
        else stringConstraints
      }

    def integerWrites(numberConstraintWrites: OWrites[NumberConstraints]): OWrites[SchemaInteger] = OWrites[SchemaInteger] { i =>
      val integerConstraints = numberConstraintWrites.writes(i.constraints)
      if (integerConstraints.fields.isEmpty) Json.obj("type" -> "integer")
      else integerConstraints
    }

    def numberWrites(numberConstraintWrites: OWrites[NumberConstraints]): OWrites[SchemaNumber] = OWrites[SchemaNumber] { num =>
      val numberConstraints = numberConstraintWrites.writes(num.constraints)
      if (numberConstraints.fields.isEmpty) Json.obj("type" -> "number")
      else numberConstraints
    }
  }
}