package com.eclipsesource.schema

import com.eclipsesource.schema.internal.{RefResolver, Context, JsValueProcessor}
import com.eclipsesource.schema.internal.serialization.{JSONSchemaReads, JSONSchemaAnnotationWrites}
import play.api.data.mapping.{Rule, Path, VA}
import play.api.libs.json.{JsObject, _}

import scalaz.Scalaz._
import scalaz._


trait Validator  {

//  val visitor2 = validationRule2
//    val visitor2 = (annotationRule |@| validationRule) { _ compose _ }
  val visitor2 = annotationRule

  val processor = JsValueProcessor()

  def validate(schema: QBType)(input: => JsValue, context: Context = Context(Path, schema, Seq.empty, Set.empty, None)): VA[JsValue] = {
    val updatedRoot = RefResolver.replaceRefs(schema, context)
    val maybeId = schema match {
      case container: QBContainer => container.id
      case _ => None
    }
    processor.process(
      updatedRoot,
      input,
      context.copy(root = updatedRoot, id = maybeId)
    )
  }

  def validate[A : Writes](schema: QBType, input: A): VA[JsValue] = {
    val writer = implicitly[Writes[A]]
    val inputJs = writer.writes(input)
    val context = Context(Path, schema, Seq.empty, Set.empty)
    val updatedRoot = RefResolver.replaceRefs(schema, context)
    processor.process(updatedRoot, inputJs, context.copy(root = updatedRoot))
  }
}

object Validator extends Validator
