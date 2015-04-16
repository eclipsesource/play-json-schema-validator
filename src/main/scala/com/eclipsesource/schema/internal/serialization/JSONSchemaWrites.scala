package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import play.api.libs.json.Json.{JsValueWrapper, toJsFieldJsValueWrapper}
import play.api.libs.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString, _}

import scala.math.BigDecimal.int2bigDecimal

trait JSONSchemaWrites {

  val MinimumId = "minimum"
  val MaximumId = "maximum"
  val PatternId = "pattern"
  val ExclusiveMinimum = "exclusiveMinimum"
  val ExclusiveMaximum = "exclusiveMaximum"
  val MultipleOf = "multipleOf"

  def writeRule(rule: ValidationRule[_]): Seq[(String, JsValueWrapper)] = rule match {
    case MinLengthRule(min) => List("minLength" -> min)
    case MaxLengthRule(max) => List("maxLength" -> JsNumber(max))
    case MinRule(min, isExclusive) => List(ExclusiveMinimum -> isExclusive, MinimumId -> JsNumber(min))
    case MaxRule(max, isExclusive) => List(ExclusiveMaximum -> isExclusive, MaximumId -> JsNumber(max))
    case MultipleOfRule(num) => List(MultipleOf -> num)
    case RegexRule(pat, _) => List(PatternId -> JsString(pat.pattern()))
    case EnumRule(values) => List("enum" -> JsArray(values.map(JsString)))
    case f: FormatRule[_] => List("format" -> JsString(f.format))
    case c: CompositeRule[_] => c.rules.toList.flatMap(writeRule(_))
    case kv: KeyValueRule[_] => List(kv.key -> kv.value)
  }

  implicit def qbTypeWriter: Writes[QBType] = OWrites[QBType] {
    case s: QBString => stringWriter.writes(s).as[JsObject]
    case i: QBInteger => integerWriter.writes(i).as[JsObject]
    case n: QBNumber => numberWriter.writes(n).as[JsObject]
    case b: QBBoolean => booleanWriter.writes(b).as[JsObject]
    case a: QBArray => arrayWriter.writes(a).as[JsObject]
    case o: QBClass => objectWriter.writes(o).as[JsObject]
  }

  implicit def booleanWriter: Writes[QBBoolean] = OWrites[QBBoolean] { bool =>
    Json.obj("type" -> "boolean") ++ Json.obj(bool.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit def stringWriter: Writes[QBString] = OWrites[QBString] { str =>
    Json.obj("type" -> "string") ++ Json.obj(str.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit def integerWriter: Writes[QBInteger] = OWrites[QBInteger] { int =>
    Json.obj("type" -> "integer") ++ Json.obj(int.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit def numberWriter: Writes[QBNumber] = OWrites[QBNumber] { num =>
    Json.obj("type" -> "number") ++ Json.obj(num.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit def arrayWriter: Writes[QBArray] = OWrites[QBArray] { arr =>
    Json.obj("type" -> "array",
      "items" -> Json.toJson(arr.items)) ++ Json.obj(arr .rules.toList.map(writeRule(_)).flatten: _*)
  }

  /**
   * Extensions whose result gets merged into the written property like, for instance, the type:
   */
  def propertyExtensions: List[QBAttribute => JsObject] = List()
  
  /**
   * Extensions which act on a object and produce a result which contains 
   * information about multiple fields. For instance, required property.
   */
  def extensions: List[QBClass => JsObject] = List()

  implicit def objectWriter: Writes[QBClass] = OWrites[QBClass] { obj =>
    {
      val written = Json.obj(
        "type" -> "object",
        "properties" -> JsObject(obj.attributes.map(attr => attr.name ->
          propertyExtensions.foldLeft(Json.toJson(attr.qbType).as[JsObject])((obj, extension) =>
            obj.deepMerge(extension(attr))))),
        "additionalProperties" -> JsBoolean(false)) ++ Json.obj(obj.rules.toList.map(writeRule(_)).flatten: _*)
      extensions.foldLeft(written)((o, extension) => 
        o.deepMerge(extension(obj)))
    }
  }

}
