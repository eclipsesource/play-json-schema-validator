//package com.eclipsesource.schema
//
//import org.specs2.mutable.Specification
//import play.api.libs.json.{JsString, Json}
//
//// TODO
//class JsVisitorExtensionsSpec extends Specification {
//
////  val processor = JsDefaultValueProcessor(Map(classOf[QBReadOnlyAnnotation] -> new ReadOnlyAnnotationVisitorExtension))
//
//  "Multiple annotations " should {
//    "be respected" in {
//      val schema = QBClassImpl(Seq(
//        QBAttribute("s", QBStringImpl(), List(QBOptionalAnnotation(Some(JsString("foo"))), QBReadOnlyAnnotation()))
//      ))
//      val instance = Json.obj()
////      val result = processor.process(schema)(instance, JsValidationVisitor())
////      result.get \ "s" must beEqualTo(JsString("foobar"))
//      true must beTrue
//    }
//  }
//
//}
