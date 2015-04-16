package com.eclipsesource.schema

import com.eclipsesource.schema.internal.JsValueProcessor
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaAnnotationWrites}
import play.api.data.mapping.{Rule, Path, VA}
import play.api.libs.json.{JsObject, _}

import scalaz.Scalaz._
import scalaz._


trait Validator  {

//  val visitor: ((JsValue, QBType)) => Rule[JsValue, JsValue] = (stringToNumberRule |@| validationRule) { _ compose _ } //{ _ |+| _ }
//  val visitor2 = validationRule2
//    val visitor2 = (annotationRule |@| validationRule) { _ compose _ }
  val visitor2 = annotationRule

//  Rule.applicativeRule.apply(validationRule. stringToNumberRule)
//  val rule2 = defaultRule.compose(jsValue => validationRule.validate())
  val processor = JsValueProcessor(visitor2)

  def validate(schema: QBType)(input: JsValue): VA[JsValue] =
    processor.process(schema, input, Path(), Seq.empty)

  def validate[A : Writes](schema: QBType, input: A): VA[JsObject] = {
    val writer = implicitly[Writes[A]]
    val inputJs = writer.writes(input)
    processor.process(schema, inputJs, Path(), Seq.empty).asInstanceOf[VA[JsObject]]
  }
}

object Validator extends Validator
