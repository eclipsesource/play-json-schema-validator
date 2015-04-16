package com.eclipsesource.schema.internal

import com.eclipsesource.schema._
import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json._

/**
 * <p>
 * A trait that encapsulates the logic of traversing an
 * JSON AST based on a given schema.
 * </p>
 *
 * <p>
 * The JsValueProcessor trait allows to specify a visitor that defines the actual behavior
 * to be performed on the nodes.
 * </p>
 */
case class JsValueProcessor(ruleProvider: ((QBType, Seq[QBAnnotation])) => Rule[JsValue, JsValue]) {

  /**
   * Processor dispatch method.
   *
   * @param schema
   *             the current schema
   * @param path
   *             the current path
   * @param input
   *             the current JsValue
   * @return a JsResult containing a result of type O
   */
  def process(schema: QBType, input: JsValue, path: Path, annotations: Seq[QBAnnotation]): VA[JsValue] = {
    (input, schema) match {
      case (jsArray:  JsArray, qbArray: QBArray)   =>
        atArray(qbArray, jsArray, path, annotations)
      case (jsObject: JsObject, qbObject: QBClass)   =>
        atObject(qbObject, jsObject, path, annotations)
      case (_: JsObject, _: QBConstrainedClass) =>
        validate(schema, input, path, annotations)
      case (j: JsNumber, n: QBNumber) =>
        validate(schema, input, path, annotations)
      case (j: JsNumber, i: QBInteger) =>
        validate(schema, input, path, annotations)
      case (j: JsBoolean, b: QBBoolean) =>
        validate(schema, input, path, annotations)
      case (j: JsString, s: QBString) =>
        validate(schema, input, path ,annotations)
      case (_: JsUndefined | JsNull, _) =>
        validate(schema, input, path, annotations)
      case _ =>
        Failure(List(path -> List(ValidationError("qb.diff.types", Json.obj("schema" -> schema.prettyPrint, "instance" -> input)))))
    }
  }

  def validate(schema: QBType, input: JsValue, path: Path, annotations: Seq[QBAnnotation]): VA[JsValue] = {
    ruleProvider(schema, annotations).repath(p => path.compose(p)).validate(input)  //r.validate(input)
  }

  /**
   * Process an object.
   *
   * @param path
   * @param schema
   *             the schema of the object
   *             the current path
   * @param obj
   *             the matched JsObject
   * @return a JsResult containing a result of type O
   */
  def atObject(schema: QBClass, obj: JsObject, path: Path, annotations: Seq[QBAnnotation]): VA[JsValue] = {

    val fields: Seq[(String, VA[JsValue])] = schema.attributes.map { attr =>
      val attrPath = path \ attr.name
      val value = obj \ attr.name
      attr.name -> process(attr.qbType, value, attrPath, attr.annotations)
    }

    val (successFields, undefineds) = fields.foldLeft(List.empty[(String, JsValue)], Seq.empty[(Path, Seq[ValidationError])])((acc, f) => f._2 match {
      case Failure(err) => (acc._1, err ++ acc._2)
      case Success(JsAbsent) => acc
      case Success(undefined: JsUndefined) => (acc._1, acc._2 :+ (path \ f._1, Seq(ValidationError(undefined.error))))
      case Success(value) => ((f._1 -> value) :: acc._1, acc._2)
    })


    if (undefineds.nonEmpty) {
      Failure(undefineds)
    } else {
      val updatedObj = JsObject(successFields)
      validate(schema, updatedObj, path, annotations)
    }
  }

  /**
   * Process an array.
   *
   * @param schema
   *             the schema of the array
   * @param path
   *             the current path
   * @param arr
   *             the matched JsArray
   * @return a JsResult containing a result of type O
   */
  def atArray(schema: QBArray, arr: JsArray, path: Path, annotations: Seq[QBAnnotation]): VA[JsValue] = {

    val result: VA[JsValue] = arr.value.zipWithIndex.map(item => process(schema.items, item._1, path \ item._2, List.empty))
      .foldLeft[VA[JsValue]](Success(JsArray())) {
      (res: VA[JsValue], itemRes: VA[JsValue]) => (res, itemRes) match {
        case (f1@Failure(_), f2@Failure(_)) => Failure.merge(f1, f2)
        case (_, f@Failure(_)) => f
        case (Success(s: JsArray), _) => Success(JsArray(s.value :+ itemRes.get))
      }
    }

    result.fold(errors => Failure(errors), a => validate(schema, a, path, annotations))

//    val elements: Seq[VA[JsValue]] = arr.value.zipWithIndex.map { case (jsValue, idx) =>
//      process(schema.items, jsValue, path \ idx, Seq.empty)
//    }
//    if (elements.exists(_.isFailure)) {
//      Failure(elements.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
//    } else {
//      val updatedArr = JsArray(elements.collect { case Success(js) => js })
//      validate(schema, updatedArr, path, annotations)
//    }
  }
}