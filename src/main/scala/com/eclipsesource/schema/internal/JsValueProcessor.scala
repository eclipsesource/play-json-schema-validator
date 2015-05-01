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
   * @param input
   *             the current JsValue
   * @return a JsResult containing a result of type O
   */
  def process(schema: QBType, input: JsValue, context: Context): VA[JsValue] = {
    (input, schema) match {
      case (jsObject: JsObject, qbObject: QBClass)   =>
        atObject(qbObject, jsObject, context)
      case (jsArray:  JsArray, qbArray: QBArray)   =>
        atArray(qbArray, jsArray, context)
        // TODO handle generic case with n-arity tuples
      case (jsArray: JsArray, qbTuple: QBTuple2) =>
        atTuple(qbTuple, jsArray, context)
      case (_: JsObject, _: QBConstrainedClass) =>
        validate(schema, input, context)
      case (j: JsNumber, n: QBNumber) =>
        validate(schema, input, context)
      case (j: JsNumber, i: QBInteger) =>
        validate(schema, input, context)
      case (j: JsBoolean, b: QBBoolean) =>
        validate(schema, input, context)
      case (j: JsString, s: QBString) =>
        validate(schema, input, context)
      case (_, ref: QBRef) =>
        val isRecursive = context.visited.contains(ref) || ref.pointer.path == "#"
        println(s"reference is $ref")
        val resolvedSchema = context.parent.flatMap(p => p.resolveRef(ref))
//        println("Resolved schema:" + resolvedSchema)
        resolvedSchema.fold[VA[JsValue]](
          Failure(List(context.path -> List(ValidationError(s"Ref at ${context.path} did not resolve"))))
        )( r => (r, input) match {
            // TODO: we need to handle arrays here too
          case (_: QBClass, JsObject(_)) => process(r, input, context.copy(visited = context.visited + ref))
          case (_: QBClass, _) if isRecursive => Success(input)
          case _ => process(r, input, context.copy(visited = context.visited + ref))
        })
      case (_: JsUndefined | JsNull, _) =>
        validate(schema, input, context)
      case _ =>
        Failure(List(context.path -> List(ValidationError("qb.diff.types", Json.obj("schema" -> schema.prettyPrint, "instance" -> input)))))
    }
  }

  def validate(schema: QBType, input: => JsValue, context: Context): VA[JsValue] = {
    val annotations = context.annotations
    ruleProvider(schema, annotations).repath(p => context.path.compose(p)).validate(input)  //r.validate(input)
  }

  /**
   * Process an object.
   *
   * @param schema
   *             the schema of the object
   *             the current path
   * @param obj
   *             the matched JsObject
   * @return a JsResult containing a result of type O
   */
  def atObject(schema: QBClass, obj: => JsObject, context: Context): VA[JsValue] = {

    val fields: Seq[(String, VA[JsValue])] = schema.attributes.map { attr =>
      val value = obj \ attr.name
      attr.name -> process(attr.qbType, value, context.copy(
        path = context.path \ attr.name,
        parent = Some(schema),
        annotations = attr.annotations
        )
      )
    }

    val (successFields, undefineds) = fields.foldLeft(List.empty[(String, JsValue)], Seq.empty[(Path, Seq[ValidationError])])((acc, f) => f._2 match {
      case Failure(err) => (acc._1, err ++ acc._2)
      case Success(JsAbsent) => acc
      case Success(undefined: JsUndefined) => (acc._1, acc._2 :+ (context.path \ f._1, Seq(ValidationError(undefined.error))))
      case Success(value) => ((f._1 -> value) :: acc._1, acc._2)
    })


    if (undefineds.nonEmpty) {
      Failure(undefineds)
    } else {
      val updatedObj = JsObject(successFields)
      validate(schema, updatedObj, context)
    }
  }

  /**
   * Process an array.
   *
   * @param schema
   *             the schema of the array
   * @param arr
   *             the matched JsArray
   * @return a JsResult containing a result of type O
   */
  def atArray(schema: QBArray, arr: JsArray, context: Context): VA[JsValue] = {
    val elements: Seq[VA[JsValue]] = arr.value.zipWithIndex.map { case (jsValue, idx) =>
      process(schema.items, jsValue, context.copy(path = context.path \ idx, parent = Some(schema)))
    }
    if (elements.exists(_.isFailure)) {
      Failure(elements.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
    } else {
      val updatedArr = JsArray(elements.collect { case Success(js) => js })
      validate(schema, updatedArr, context)
    }
  }

  def atTuple(schema: QBTuple2, array: JsArray, context: Context): VA[JsValue] = {

    val p1: VA[JsValue] = process(schema.items._1, array.value(0), context.copy(path = context.path \ 0, parent = Some(schema)))
    val p2: VA[JsValue] = process(schema.items._2, array.value(1), context.copy(path = context.path \ 1, parent = Some(schema)))
//    println(s"p1: $p1")
//    println(s"p2: $p2")
    val elements = Seq(p1, p2)

    if (elements.exists(_.isFailure)) {
      Failure(elements.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
    } else {
      val updatedArr = JsArray(elements.collect { case Success(js) => js })
      validate(schema, updatedArr, context)
    }
  }
}