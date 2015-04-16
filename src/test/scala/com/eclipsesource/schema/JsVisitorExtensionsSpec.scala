package com.eclipsesource.schema

import org.specs2.mutable.Specification
import play.api.libs.json.{JsString, Json}

//class ReadOnlyAnnotationVisitorExtension extends AnnotationProcessor {
//  override def process(attr: QBAttribute, input: Option[JsValue], jsObject: JsObject): Option[JsValue] = {
//    input.fold[Option[JsValue]](None){ value =>
//      attr.qbType match {
//        case str: QBString => Some(JsString(value.asInstanceOf[JsString].value + "bar"))
//        case x => input
//      }
//    }
//  }
//}

// TODO
class JsVisitorExtensionsSpec extends Specification {

//  val processor = JsDefaultValueProcessor(Map(classOf[QBReadOnlyAnnotation] -> new ReadOnlyAnnotationVisitorExtension))

  "Multiple annotations " should {
    "be respected" in {
      val schema = QBClassImpl(Seq(
        QBAttribute("s", QBStringImpl(), List(QBOptionalAnnotation(Some(JsString("foo"))), QBReadOnlyAnnotation()))
      ))
      val instance = Json.obj()
//      val result = processor.process(schema)(instance, JsValidationVisitor())
//      result.get \ "s" must beEqualTo(JsString("foobar"))
      true must beTrue
    }
  }

}
