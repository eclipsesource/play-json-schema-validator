package com.eclipsesource.schema.internal

import java.util.regex.Pattern

import com.eclipsesource.schema
import com.eclipsesource.schema._
import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scalaz.{Failure => _, Source => _, Success => _, _}

case class JsValueProcessor(/*ruleProvider: ((QBType, Seq[QBAnnotation])) => Rule[JsValue, JsValue]*/) {

  // provide semigroup for unit for use with ReaderWriterState
  implicit def UnitSemigroup: Semigroup[Unit] = new Semigroup[Unit] {
    override def append(f1: Unit, f2: => Unit): Unit = ()
  }

  /**
   * Type params in this order are:
   * - reader
   * - writer
   * - state
   * - value
   */
  type ValidationStep[A] = ReaderWriterState[Context, Unit, ValidationStatus, A]
  type Props = Seq[(String, JsValue)]
  type PropertyValidationResult = (String, VA[JsValue])
  type ValidProperty = (String, JsValue)
  type ValidProperties = Seq[(String, JsValue)]
  type InvalidProperty = (Path, Seq[ValidationError])
  type InvalidProperties = Seq[(Path, Seq[ValidationError])]

  case class ValidationStatus(validProps: ValidProperties = List.empty, invalidProps: InvalidProperties = List.empty) {

    private[schema] def add(otherStatus: ValidationStatus) =
      ValidationStatus(validProps ++ otherStatus.validProps, invalidProps ++ otherStatus.invalidProps)

    private[schema] def addToInvalidProps(invalidProperties: InvalidProperties) =
      ValidationStatus(validProps, invalidProps ++ invalidProperties)

    private[schema] def addToInvalidProps(invalidProp: InvalidProperty) =
      ValidationStatus(validProps, invalidProps :+ invalidProp)

    private[schema] def addToValidProps(validProperties: ValidProperties) =
      ValidationStatus(validProps ++ validProperties, invalidProps)

    private[schema] def addToValidProps(validProp: ValidProperty) =
      ValidationStatus(validProps :+ validProp, invalidProps)
  }

  object ValidationStatus {
    def empty = ValidationStatus()
  }

  private def failure(propName: String, path: Path, errorMsg: String): PropertyValidationResult = {
    (propName, Failure(Seq(path -> Seq(ValidationError(errorMsg)))))
  }

  private def failure(path: Path, errorMsg: String): InvalidProperty = {
    path -> List(ValidationError(errorMsg))
  }

  private def success(prop: (String, JsValue)): PropertyValidationResult = {
    prop._1 -> Success(prop._2)
  }

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
      case (json, qbObject: QBClass) =>
        atObject(qbObject, json, context)
//      case (json, qbObject: QBClass) =>
//        validate(schema, input, context)
      case (jsArray: JsArray, qbArray: QBArray) =>
        atArray(qbArray, jsArray, context)
      case (jsArray: JsArray, qbTuple: QBTuple) =>
        atTuple(qbTuple, jsArray, context)
      case (j: JsNumber, n: QBNumber) =>
        validate(schema, input, context)
      case (j: JsNumber, i: QBInteger) =>
        validate(schema, input, context)
      case (j: JsBoolean, b: QBBoolean) =>
        validate(schema, input, context)
      case (j: JsString, s: QBString) =>
        validate(schema, input, context)
      case (JsNull, _) => //: QBNull) =>
        Success(JsNull)
//        validate(schema, input, context)
      case (_: JsUndefined, _) =>
        validate(schema, input, context)
      case _ =>
        Failure(List(context.path -> List(ValidationError("qb.diff.types", Json.obj("schema" -> schema.prettyPrint, "instance" -> input)))))
    }
  }

  def validate(schema: QBType, input: => JsValue, context: Context): VA[JsValue] = {
    val status: ValidationResult = schema.rules.foldLeft(ValidationResult(schema, Success(input), context))((acc, rule) => {
      val result: ValidationResult = rule.apply(acc.schema, input, acc.context)
      result.copy(result = ResultAggregator.toVA(Seq(acc.result, result.result)))
    })

    status.result
  }

  private def atObject(schema: QBClass, json: => JsValue, context: Context): VA[JsValue] = {

//    def applyRules = {
//      schema.rules.foldLeft((schema, context, Seq.empty[VA[JsValue]]))((acc, rule) => {
//        val results: Seq[VA[JsValue]] = acc._3
//        val result: ValidationResult = rule.apply(acc._1, json.asInstanceOf[JsObject], acc._2)
//        (result._1, context, results :+ result._3)
//      })
//    }

    // check if any property is a ref
    val reference = schema.properties.collectFirst { case QBAttribute("$ref", ref@QBRef(_, _, _, _), _) => ref }
    reference.flatMap(ref => RefResolver.resolveRef(ref, context)).map {
//      case cls: QBClass => validateJson(cls, context)
      case x => println("RESOLVED"); process(x, json, context)
    }.getOrElse(validate(schema, json, context))
  }


//  private def validateProps(schema: QBClass, obj: => JsObject, context: Context): ValidationResult = {
//    val validated: Seq[(String, ValidationResult)] = schema.props.map { attr =>
//      val value = obj \ attr.name
//      attr.name -> process(
//        attr.qbType,
//        value,
//        context.copy(
//          path = context.path \ "properties" \ attr.name,
//          annotations = attr.annotations
//        )
//      )
//    }
//
//    val validationStatus = ResultAggregator.aggregateResults(validated)
//    val unvalidatedProps: Props = obj.fields.filterNot(field =>
//      validationStatus.validProps.map(_._1).contains(field._1)
//    )
//    ((), unvalidatedProps, status.add(validationStatus))
//  }
//
//
//  private def validatePatternProps(schema: QBClass, props: Props): ValidationStep[Props] =
//    ReaderWriterState { (context, status) =>
//      // find all matching properties and validate them
//      val validated = props.flatMap {
//        prop => {
//          val matchedPPs = schema.patternProperties.filter(pp => {
//            val pattern = Pattern.compile(pp.name)
//            val matcher = pattern.matcher(prop._1)
//            matcher.find()
//          })
//          matchedPPs.map(pp =>
//            prop._1 -> process(pp.qbType, prop._2, context)
//          )
//        }
//      }
//      val validationStatus = aggregateResults(validated, context)
//      val unmatchedProps = props.filterNot(prop =>
//        validationStatus.validProps.map(_._1).contains(prop._1)
//      )
//      ((), unmatchedProps, status.add(validationStatus))
//    }
//
//  private def validateAdditionalProps(schema: QBClass, unmatchedFields: Props): ValidationStep[Unit] = {
//
//    def validateUnmatched(qbType: QBType, context: Context): ValidationStatus = {
//      val validated = unmatchedFields.map { attr =>
//        attr._1 -> process(
//          qbType, attr._2, context.copy(
//            path = context.path \ attr._1
//          )
//        )
//      }
//      aggregateResults(validated, context)
//    }
//
//    ReaderWriterState { (context, status) =>
//      if (unmatchedFields.isEmpty) {
//        ((), (), status)
//      } else {
//        schema.additionalProperties.schema match {
//          case QBBooleanConstant(enabled) =>
//            if (enabled) {
//              ((), (), status.addToValidProps(unmatchedFields))
//            } else {
//              ((), (), status.addToInvalidProps(failure(context.path, s"patternProperties: $unmatchedFields")))
//            }
//          case _ => val additionalPropsSchema = schema.additionalProperties.schema
//            val validationStatus = validateUnmatched(additionalPropsSchema, context)
//            if (validationStatus.invalidProps.nonEmpty) {
//              ((), (), status.addToInvalidProps(
//                failure(context.path, s"additional properties: ${validationStatus.invalidProps}")))
//            } else {
//              ((), (), status.add(validationStatus))
//            }
//        }
//      }
//    }
//  }

  private def atArray(schema: QBArray, arr: JsArray, context: Context): VA[JsValue] = {
    val elements: Seq[VA[JsValue]] = arr.value.zipWithIndex.map { case (jsValue, idx) =>
      process(schema.items, jsValue, context.copy(path = context.path \ idx))
    }
    if (elements.exists(_.isFailure)) {
      Failure(elements.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
    } else {
      val updatedArr = JsArray(elements.collect { case Success(js) => js })
      validate(schema, updatedArr, context)
    }
  }

  private def atTuple(schema: QBTuple, array: JsArray, context: Context): VA[JsValue] = {

    //    val instanceSize = array.value.size
    //    val schemaSize = schema.qbTypes.size
    //
    //    val results: Seq[(String, VA[JsValue])] = if (instanceSize > schemaSize) {
    //      val additionalInstanceValues: Seq[JsValue] = array.value.takeRight(instanceSize - schemaSize)
    //      val additionalItemsSchema: Option[QBType] = schema.additionalItems
    //      val result: Seq[VA[JsValue]] = additionalItemsSchema.fold[Seq[VA[JsValue]]](
    //        Seq(Failure(Seq(context.path -> Seq(ValidationError("Too many items during tuple validation.")))))
    //      ) {
    //        case items =>
    //          val instanceValuesValidated: Seq[VA[JsValue]] = schema.items().zipWithIndex.map { case (item, idx) =>
    //            process(item, array.value(idx), context.copy(path = context.path \ idx))
    //          }
    //          val additionalInstanceValuesValidated: Seq[VA[JsValue]] = additionalInstanceValues.zipWithIndex.map {
    //            case (jsValue, idx) =>
    //              process(items, jsValue, context.copy(path = context.path \ idx))
    //          }
    //          instanceValuesValidated ++ additionalInstanceValuesValidated
    //      }
    //      result
    //    } else {
    //      array.value.zipWithIndex.map { case (jsValue, idx) =>
    //        process(schema.items()(idx), jsValue, context.copy(path = context.path \ idx))
    //      }
    //    }

    val results: Seq[VA[JsValue]] = array.value.zipWithIndex.map { case (jsValue, idx) =>
      Validator.validate(schema.items()(idx))(jsValue, context.copy(path = context.path \ idx))
    }

    if (results.exists(_.isFailure)) {
      Failure(results.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
    } else {
      val updatedArr = JsArray(results.collect { case Success(js) => js })
      validate(schema, updatedArr, context)
    }
  }

//    if (results.exists(_.isFailure)) {
//      Failure(results.collect { case Failure(err) => err }.reduceLeft(_ ++ _))
//    } else {
//      val updatedArr = JsArray(results.collect { case Success(js) => js })
//      validate(schema, updatedArr, context)
//    }
//  }
}