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

  implicit def qbTypeWriter: Writes[QBType] = Writes[QBType] {
    case s: QBString => stringWriter.writes(s)
    case i: QBInteger => integerWriter.writes(i)
    case n: QBNumber => numberWriter.writes(n)
    case b: QBBoolean => booleanWriter.writes(b)
    case t: QBTuple2 => tupleWriter.writes(t)
    case a: QBArray => arrayWriter.writes(a)
    case o: QBClass => objectWriter.writes(o)
    case r: QBRef => refWriter.writes(r)
  }

  implicit val booleanWriter: Writes[QBBoolean] = OWrites[QBBoolean] { bool =>
    Json.obj("type" -> "boolean") ++ Json.obj(bool.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit val stringWriter: Writes[QBString] = OWrites[QBString] { str =>
    Json.obj("type" -> "string") ++ Json.obj(str.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit val integerWriter: Writes[QBInteger] = OWrites[QBInteger] { int =>
    Json.obj("type" -> "integer") ++ Json.obj(int.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit val numberWriter: Writes[QBNumber] = OWrites[QBNumber] { num =>
    Json.obj("type" -> "number") ++ Json.obj(num.rules.toList.map(writeRule(_)).flatten: _*)
  }

  implicit val arrayWriter: Writes[QBArray] = Writes[QBArray] { arr =>
    Json.toJson(arr.items)
  }

  implicit val tupleWriter: Writes[QBTuple2] = Writes[QBTuple2] { arr =>
    Json.toJson(arr.qbTypes)
  }

  implicit val refWriter: Writes[QBRef] = Writes[QBRef] { ref =>
    if (ref.isAttribute) {
      JsString(ref.pointer.path)
    } else {
      Json.obj("$ref" -> JsString(ref.pointer.path))
    }
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

  implicit val objectWriter: Writes[QBClass] = OWrites[QBClass] { obj =>
  {
   val o: JsObject = if (obj.properties.exists(_.name == "properties")) {
     Json.obj(
       "type" -> "object"
     )
   } else if (obj.properties.exists(_.name == "items")) {
     Json.obj(
       "type" -> "array"
     )
   } else {
     Json.obj()
   }

    val written = o.deepMerge(
        //          Json.obj("properties" ->
        JsObject(
          obj.properties.map(attr =>
            attr.name ->
              propertyExtensions.foldLeft(Json.toJson(attr.qbType))((json, extension) =>
                json match {
                  case o: JsObject => o.deepMerge(extension(attr))
                  case x => x
                }
              )
          )
        )
      ).deepMerge(Json.obj(obj.rules.toList.map(writeRule(_)).flatten: _*))

//    println(">>" + written)

    val written2 = extensions.foldLeft(written)((o, extension) =>
      o.deepMerge(extension(obj)))

//    println(">>" + written2)

    written2
  }
  }

}
