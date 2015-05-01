package com.eclipsesource

import com.eclipsesource.schema.internal.SchemaUtil
import com.eclipsesource.schema.internal.serialization.{JSONSchemaAnnotationWrites, JSONSchemaReads, JSONSchemaWrites}
import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.reflect.ClassTag
import scalaz.{Success => _, Failure => _, _}
import Scalaz._

package object schema
  extends SchemaDSL
  with SchemaOps
  with JSONSchemaWrites
  with JSONSchemaAnnotationWrites
  with JSONSchemaReads {

  val isQBString = (qbType: QBType) => qbType.isInstanceOf[QBString]
  val isQBNumber = (qbType: QBType) => qbType.isInstanceOf[QBNumber]
  val isQBInteger = (qbType: QBType) => qbType.isInstanceOf[QBInteger]

  def isQBAnnotation[A <: QBAnnotation : ClassTag]: QBAnnotation => Boolean =
    (annotation: QBAnnotation) => implicitly[ClassTag[A]].runtimeClass.isInstance(annotation)

  val isQBOptionalAnnotation = isQBAnnotation[QBOptionalAnnotation]
  val isQBDefaultAnnotation = isQBAnnotation[QBDefaultAnnotation]
  val isQBReadOnlyAnnotation = isQBAnnotation[QBReadOnlyAnnotation]

  type ERRORS = List[ValidationError]

  case class RuleProvider(f: PartialFunction[(QBType, Seq[QBAnnotation]), Rule[JsValue, JsValue]]) extends PartialFunction[((QBType, Seq[QBAnnotation])), Rule[JsValue, JsValue]] {

    override def apply(data: (QBType, Seq[QBAnnotation])): Rule[JsValue, JsValue] = {
      if (f.isDefinedAt(data)) {
        f(data)
      } else {
        Rule[JsValue, JsValue] { Success(_) }
      }
    }

    override def isDefinedAt(x: (QBType, Seq[QBAnnotation])): Boolean = f.isDefinedAt(x)
  }

  val successRule = Rule[JsValue, JsValue] { Success(_) }

  val defaultRule2 = RuleProvider {
    case (_, annotations) if annotations.exists(_.isInstanceOf[QBDefaultAnnotation]) => Rule.fromMapping[JsValue, JsValue] {
      case _: JsUndefined =>
        annotations.find(isQBDefaultAnnotation).collectFirst {
          case QBDefaultAnnotation(default) => Success(default)
        }.get
      case js => Success(js)
    }
  }

  val optionalRule2: Function[((QBType, Seq[QBAnnotation])), Rule[JsValue, JsValue]] = RuleProvider {
    case (_, annotations) if { /*println(s"hey $annotations");*/ annotations.exists(_.isInstanceOf[QBOptionalAnnotation]) } => Rule.fromMapping[JsValue, JsValue] {
      case undefined: JsUndefined  =>
        annotations.find(isQBOptionalAnnotation).collectFirst {
        case QBOptionalAnnotation(maybeFallback) =>
          maybeFallback.fold(Success(JsAbsent: JsValue))(value => Success(value))
      }.get
      case js => println("!"); Success(js)
    }
  }

  val validationRule2: Function[((QBType, Seq[QBAnnotation])), Rule[JsValue, JsValue]]  = RuleProvider {
    case (qbType: ValidationRule[_], _) => qbType.rule
  }

//  val rule: Function[(QBType, Seq[QBAnnotation]), Rule[JsValue, JsValue]] = defaultRule2.orElse(optionalRule2)

  val annotationRule: ((QBType, Seq[QBAnnotation])) => Rule[JsValue, JsValue] = {
    (optionalRule2 |@| validationRule2) { _ compose _}
  }

//  val annotationRule2: ((QBType, Seq[QBAnnotation])) => Rule[JsValue, JsValue] = { foo =>
//    val x = rule(foo)
//    val y = validationRule2(foo)
//    x.compose(y)
//  }

   case object JsAbsent extends JsUndefined("qb.accepted")

  implicit class JsObjectExtensions(jsObject: JsObject) {
    def get(fieldName: String): Option[JsValue] = {
      val jsValue = jsObject \ fieldName
      if (jsValue.isInstanceOf[JsUndefined]) {
        None
      } else {
        Some(jsValue)
      }
    }
  }

  implicit class QBTypeExtensionOps(qbType: QBType) {
    def prettyPrint: String = SchemaUtil.prettyPrint(qbType)
  }

  implicit class QBClassExtensionOps(qbClass: QBClass) {
    def hasAttribute(attributeName: String) ={
      qbClass.attributes.exists(_.name == attributeName)
    }
  }
}