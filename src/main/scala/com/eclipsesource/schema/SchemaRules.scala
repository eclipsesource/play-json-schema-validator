package com.eclipsesource.schema

import java.util.regex.Pattern

import akka.actor.Props
import com.eclipsesource.schema.internal._
import play.api.data.mapping
import play.api.data.mapping._
import play.api.data.mapping.json.Rules
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.collection.immutable.HashSet
import scalaz.{Id, IndexedReaderWriterStateT, ReaderWriterState}


//----------------------------------------------------------
// 	Rule definitions
//----------------------------------------------------------

/**
 * Definition of a basic validation rule.
 *
 */
case class ValidationResult(schema: QBType, result: VA[JsValue], context: Context)
case class ValidationState(schema: QBType, validated: ValidatedProperties, context: Context)

trait ValidationRule {

  // TODO
  def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = ValidationResult(schema, Success(json), context)

  //    def rule: Rule[JsValue, JsValue]
}

sealed trait SchemaBasedValidationRule extends ValidationRule {
  def schemas: Seq[QBType]
  def copy(schemas: Seq[QBType]): SchemaBasedValidationRule
}

/**
 * Definition of a validation rule that consists of multiple different validation rules.
 *
 */
trait CompositeRule extends ValidationRule {

  /**
   * Returns all rules that make up the composite rule.
   *
   * @return a set of validation rules
   */
  def rules: Seq[ValidationRule]
}

//  val successRule = Rule[JsValue, JsValue] { js => Success(js)}

//  override lazy val rule: Rule[JsValue, JsValue] = {
//    if (rules.isEmpty) {
//      successRule
//    } else {
//      rules.map(_.rule).reduceLeft(_ |+| _)
//    }
//  }

//  def rule: Rule[A, A] = rules.map(_.rule).reduceLeft(_ |+| _)

//override def validate(path: QBPath, a: A): VA[A] = rule.validate(a)
//}

//----------------------------------------------------------
// 	Number Rules
//----------------------------------------------------------

/**
 * Rule that checks whether a given number is a multiple of another number.
 *
 * @param factor
 *           a factor of the number to be validated, if this rule should validate successfully
 */
case class MultipleOfRule(factor: Double) extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule {
    case number: JsNumber =>
      if (number.value.toDouble % factor == 0) {
        Success(number)
      } else {
        Failure(
          List(ValidationError("qb.number.multiple-of",
            Json.obj("factor" -> factor), "number" -> number)
          )
        )
      }
  }
}

case class IsIntegerRule() extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule {
    case number: JsNumber =>
      if (number.value.isValidInt) {
        Success(number)
      } else {
        Failure(
          List(ValidationError("qb.invalid.int"))
        )
      }
  }
}

case class CatchUndefinedRule(pf: PartialFunction[JsValue, Validation[ValidationError, JsValue]]) extends Rule[JsValue, JsValue]{
  override def validate(data: JsValue): VA[JsValue] = {
    Rule.fromMapping[JsValue, JsValue] {
      if (pf.isDefinedAt(data)) {
        case _ => pf(data)
      } else {
        case _ =>
          data match {
            //            case undefined: JsUndefined => Failure(List(ValidationError("uNDEF")))
            case js => Success(js)
          }
      }
    }.validate(data)
  }
}

/**
 * Rule that checks whether a given number is greater than or equal to a given minimum.
 *
 * @param min
 *            the minimum which needs to be less than or equal to the number that is validated
 * @param isExclusive
 *            if true, the check also succeeds if the number to be checked is equal to the minimum
 */
case class MinRule(min: BigDecimal, isExclusive: Boolean) extends ValidationRule {


  override def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = {
    println("MINRULE " + json)
    val res = ValidationResult(schema, validate(json), context)
    println("MINRULE result " + res)
    res
  }

  def validate(json: JsValue): VA[JsValue] = json match {
    case number: JsNumber =>
      if (isValid(number)) {
        Success(number)
      } else {
        Failure(Seq(Path ->
          List(
            ValidationError("qb.number.min",
              Json.obj("min" -> min, "number" -> number)
            )
          )
        )
        )
      }
    case _ => Success(json)
  }

  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule {
    case number: JsNumber =>
      if (isValid(number)) {
        Success(number)
      } else {
        Failure(
          List(
            ValidationError("qb.number.min",
              Json.obj("min" -> min, "number" -> number)
            )
          )
        )
      }
  }

  private def isValid(n: JsNumber) = {
    if (isExclusive) {
      n.value.toDouble > min
    } else {
      n.value.toDouble >= min
    }
  }
}

/**
 * Rule that checks whether a given number is less than or equal to a given minimum.
 *
 * @param max
 *            the maximum which needs to be greater than or equal to the number that is validated
 * @param isExclusive
 *            if true, the check also succeeds if the number to be checked is equal to the maximum
 */
case class MaxRule(max: BigDecimal, isExclusive: Boolean) extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case number: JsNumber =>
      if (isValid(number)) {
        Success(number)
      } else {
        Failure(
          List(
            ValidationError("qb.number.max",
              Json.obj("max" -> max, "number" -> number)
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.number.expected")))
  }

  private def isValid(n: JsNumber) = {
    if (isExclusive) {
      n.value.toDouble < max
    } else {
      n.value.toDouble <= max
    }
  }
}

/**
 * Utility wrapper class that is used in the DSL in order
 * to use the same rules for integers as for doubles.
 *
 * @param rule
*             the wrapped number rule
 */
case class DoubleRuleWrapper(rule: ValidationRule)

//----------------------------------------------------------
// 	String Rules
//----------------------------------------------------------

/**
 * Rule that checks whether a string has a minimum length.
 *
 * @param minLength
 *           the minimum length of the string
 */
case class MinLengthRule(minLength: Int) extends ValidationRule {


  // TODO
  override def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = {
    println("MINLENGTH " + json)
    ValidationResult(schema, validated(json), context)
  }


  def validated(json: JsValue): VA[JsValue] = json match {
    case string: JsString =>
      if(string.value.length >= minLength) {
        Success(string)
      } else {
        Failure(Seq(Path ->
          List(
            ValidationError("qb.string.min-length",
              Json.obj("minLength" -> minLength, "string" -> string)
            )
          )
        ))
      }
    case _ => Success(json)
  }

  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule {
    case string: JsString =>
      if(string.value.length >= minLength) {
        Success(string)
      } else {
        Failure(
          List(
            ValidationError("qb.string.min-length",
              Json.obj("minLength" -> minLength, "string" -> string)
            )
          )
        )
      }
  }
}

/**
 * Rule that checks whether a string does not exceeds a maximum length.
 *
 * @param maxLength
 *           the maximum length of the string that must not be exceeded
 */
case class MaxLengthRule(maxLength: Int) extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule {
    case string: JsString  =>
      if(string.value.length < maxLength) {
        Success(string)
      } else {
        Failure(
          List(
            ValidationError("qb.string.max-length",
              Json.obj("maxLength" -> maxLength, "string" -> string)
            )
          )
        )
      }
  }
}

/**
 * Rule that checks whether a string matches regular expression.
 *
 * @param pattern
 *           the regular expression to be matched
 */
case class RegexRule(pattern: Pattern, errorMessage: String = "") extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping({
    case string: JsString =>
      if(pattern.matcher(string.value).matches()) {
        Success(string)
      } else {
        Failure(
          List(
            ValidationError("qb.string.regex",
              Json.obj("pattern" -> pattern.pattern(), "string" -> string)
                .deepMerge(if (errorMessage.isEmpty) Json.obj() else Json.obj("error" -> errorMessage))
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.string.expected")))
  })
}

object RegexRule {
  def apply(regex: String) = new RegexRule(Pattern.compile(regex))
}

/**
 * Rule that checks whether a string matches is contained in a set of predefined strings.
 *
 * @param enum
 *           the valid strings
 */
case class EnumRule(enum: List[String]) extends ValidationRule {
  val values = HashSet(enum:_ *)

  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule{
    case string: JsString =>
      if (values.contains(string.value)) {
        Success(string)
      } else {
        Failure(
          List(
            ValidationError("qb.string.enum",
              Json.obj("enum" -> enum, "string" -> string)
            )
          )
        )
      }
  }
}

//----------------------------------------------------------
// 	Array Rules
//----------------------------------------------------------

/**
 * Rule that checks whether all items of an array are unique.
 */
case class UniquenessRule() extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case array: JsArray =>
      if (array.value.distinct.size == array.value.size) {
        Success(array)
      } else {
        Failure(
          List(
            ValidationError("qb.array.unique",
              Json.obj("array" -> array)
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.array.expected")))
  }
}

/**
 * Array rule that checks whether an array has at least the specified number of elements.
 */
case class MinItemsRule(minItems: Int) extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case array: JsArray =>
      if (array.value.size >= minItems) {
        Success(array)
      } else {
        Failure(
          List(
            ValidationError("qb.array.minItems",
              Json.obj("minItems" -> minItems, "array" -> array)
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.array.expected")))
  }
}

/**
 * Array rule that checks whether an array has at most the specified number of elements.
 */
case class MaxItemsRule(maxItems: Int) extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case array: JsArray =>
      if (array.value.size <= maxItems) {
        Success(array)
      } else {
        Failure(
          List(
            ValidationError("qb.arr.maxItems.violated",
              Json.obj("maxItems" -> maxItems, "array" -> array)
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.array.expected")))
  }

}

//----------------------------------------------------------
// 	Any instance types rules
//----------------------------------------------------------

/**
 * Validates an instance successfully if at least one schema is matched.
 *
 * @param schemas
 *                all possible schemas
 */
// TODO: include validation result in validation error
case class QBAnyOfRule(schemas: Seq[QBType]) extends SchemaBasedValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case json =>
      val allValidationResults = schemas.map(Validator.validate(_)(json))
      val maybeSuccess = allValidationResults.find(_.isSuccess)
      maybeSuccess.map(success => Success(json)).getOrElse(
        Failure(
          List(
            ValidationError("qb.obj.any-of",
              Json.obj("schemas" -> Json.arr(schemas.map(_.prettyPrint)), "object" -> json)
            )
          )
        )
      )
  }

  override def copy(schemas: Seq[QBType]): SchemaBasedValidationRule = QBAnyOfRule(schemas)
}

/**
 * Validates an instance successfully if exactly one schema is matched.
 *
 * @param schemas
 *                all possible schemas
 */
case class QBOneOfRule(schemas: Seq[QBType]) extends SchemaBasedValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case obj: JsObject =>
      val allValidationResults = schemas.map(schema => Validator.validate(schema)(obj))
      allValidationResults.count(_.isSuccess) match {
        case 0 => Failure(List(
          ValidationError("qb.obj.one-of.none",
            Json.obj("schemas" -> Json.arr(schemas.map(_.prettyPrint)), "object" -> obj)
          )
        ))
        case 1 => Success(obj)
        case _ =>
          Failure(List(
            ValidationError("qb.obj.one-of.many",
              Json.obj("schemas" -> Json.arr(schemas.map(_.prettyPrint)), "object" -> obj)
            )
          ))
      }
    case _ => Failure(List(ValidationError("qb.obj.expected")))
  }

  override def copy(schemas: Seq[QBType]): SchemaBasedValidationRule = QBOneOfRule(schemas)
}

/**
 * Validates an instance successfully if all schemas are matched.
 *
 * @param schemas
 *                all possible schemas
 */
// TODO ValidationRule type parameter seems to be obsolete anyways..
case class QBAllOfRule(schemas: Seq[QBType]) extends SchemaBasedValidationRule {


  // TODO
  override def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = {
    ValidationResult(schema, validated(json), context)
  }

  def validated(json: JsValue): VA[JsValue] = {
    println("allOf triggered")
    val allValidationResults = schemas.map(schema => Validator.validate(schema)(json))
    val allMatch = allValidationResults.forall(_.isSuccess)
    if (allMatch) {
      Success(json)
    } else {
      Failure(Seq(Path ->
        List(
          ValidationError("qb.obj.all-of",
            Json.obj("schemas" -> Json.toJson(schemas.map(_.prettyPrint)), "object" -> json)
          )
        )
      ))
    }
  }

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case json =>
      val allValidationResults = schemas.map(schema => Validator.validate(schema)(json))
      val allMatch = allValidationResults.forall(_.isSuccess)
      if (allMatch) {
        Success(json)
      } else {
        Failure(
          List(
            ValidationError("qb.obj.all-of",
              Json.obj("schemas" -> Json.toJson(schemas.map(_.prettyPrint)), "object" -> json)
            )
          )
        )
      }
  }

  override def copy(schemas: Seq[QBType]): SchemaBasedValidationRule = QBAllOfRule(schemas)
}

//----------------------------------------------------------
// 	Object validation keywords
//----------------------------------------------------------

/**
 * Rule that checks whether a given object has a minimum number of properties.
 *
 * @param minProperties
 *            the minimum number of properties
 */
case class MinPropertiesRule(minProperties: Int) extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case obj: JsObject =>
      if (obj.fieldSet.size >= minProperties) {
        Success(obj)
      } else {
        Failure(
          List(
            ValidationError("qb.obj.min-props",
              Json.obj("minProperties" -> minProperties, "object" -> obj)
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.object.expected")))
  }
}

/**
 * Rule that checks whether the number of properties of a given object does not exceed the given maximum number
 * of properties.
 *
 * @param maxProperties
 *            the minimum number of properties
 */
case class MaxPropertiesRule(maxProperties: Int) extends ValidationRule {

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case obj: JsObject =>
      if (obj.fieldSet.size <= maxProperties) {
        Success(obj)
      } else {
        Failure(
          List(
            ValidationError("qb.obj.max-props",
              Json.obj("maxProperties" -> maxProperties, "object" -> obj)
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.object.expected")))
  }
}

// TODO: make won type for additionalItems
case class AdditionalItemsRule(additionalItems: QBType) extends ValidationRule {

  override def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = (schema, json) match {
    case (tuple: QBTuple, JsArray(array)) =>

      println("SERUCVUS")

      val instanceSize = array.size
      val schemaSize = tuple.qbTypes.size

      val results: Seq[VA[JsValue]] = if (instanceSize > schemaSize) {
        val additionalInstanceValues: Seq[JsValue] = array.takeRight(instanceSize - schemaSize)
        additionalItems match {
          // TODO: return values
          case QBBooleanConstant(false) => Seq(Failure(Seq(context.path -> Seq(ValidationError(s"Too many items during validation. (instance $instanceSize, schema: $schemaSize)")))))
          case QBBooleanConstant(true) => Seq(Success(json))
          case QBClass(props, _, _, _) if props.isEmpty => Seq(Success(json))
          case items =>
            val instanceValuesValidated: Seq[VA[JsValue]] = tuple.items().zipWithIndex.map { case (item, idx) =>
             Validator.validate(items)(array(idx), context.copy(path = context.path \ idx))
            }
            val additionalInstanceValuesValidated: Seq[VA[JsValue]] = additionalInstanceValues.zipWithIndex.map {
              case (jsValue, idx) =>
               Validator.validate(items)(jsValue, context.copy(path = context.path \ idx))
            }
            instanceValuesValidated ++ additionalInstanceValuesValidated
        }
      } else {
        Seq(Success(json))
      }


      ValidationResult(tuple, ResultAggregator.toVA(results), context)
  }
}

//case class OptionalRule(required: Seq[String]) extends ValidationRule {
//  override def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = (schema, json) match {
//    case (cls: QBClass, undef: JsUndefined) =>
//  }
//
//    case undefined: JsUndefined  =>
//      annotations.find(isQBOptionalAnnotation).collectFirst {
//        case QBOptionalAnnotation(maybeFallback) =>
//          maybeFallback.fold(Success(JsAbsent: JsValue))(value => Success(value))
//      }.get
//    case js => Success(js)
//  }
//}

case class PropertiesRule(patternProps: Option[PatternProperties], additionalProps: AdditionalProperties) extends ValidationRule {

  override def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = (schema, json) match {
    case (cls: QBClass, obj: JsObject) =>
      val validation = for {
        remaining <- validateProps(cls, obj)
        unmatched <- validatePatternProps(cls, remaining)
        _ <- validateAdditionalProps(cls, unmatched)
      } yield ()

      // TODO: review params
      val (_, _, status) = validation.run(context, ValidationState(schema, ValidatedProperties.empty, context))
      ValidationResult(status.schema, ResultAggregator.toVA(status.validated), status.context)
//    case (cls: QBClass, _) => println("cls "+  cls); println(json); ValidationResult(schema, Validator.processor.validate(schema, json, context), context)
    case _ => ValidationResult(schema, Success(json), context)

  }

  private def validateProps(schema: QBClass, obj: => JsObject): ValidationStep[Seq[(String, JsValue)]] =
    ReaderWriterState { (context, validationResult) =>

      val validated: Seq[(String, VA[JsValue])] = schema.props.map { attr => {
        val value = obj \ attr.name
        if (attr.name == "minLength") {
          println(attr.name)
          println(attr.qbType)
          val reference = attr.qbType.asInstanceOf[QBClass].properties.collectFirst { case QBAttribute("$ref", ref@QBRef(_, _, _, _), _) => ref }
          println(RefResolver.resolveRef(reference.get, context))
          println("XX"  + reference)
          println(value)
        }
        attr.name -> Validator.processor.process(
          attr.qbType,
            value,
            context.copy(
              path = context.path \ "properties" \ attr.name,
              annotations = attr.annotations
            )
          )
      }
      }

      println(validated)

      val validated2: Seq[(String, VA[JsValue])] = validated.filter {
        case (name, Success(undef: JsUndefined)) => schema.required.contains(name)
        case _ => true
      }

      println("validated2 is " + validated2)

        val validatedProps: ValidatedProperties = ResultAggregator.aggregateResults(validated2)

        val unvalidatedProps: Seq[(String, JsValue)] = obj.fields.filterNot(field =>
          validatedProps.valid.map(_._1).contains(field._1)
        )

        // TODO: status parameter is not updated/considered here
        ((), unvalidatedProps, ValidationState(schema, validatedProps, context))
    }


  private def validatePatternProps(schema: QBClass, remaining: Seq[(String, JsValue)]): ValidationStep[Seq[(String, JsValue)]] =
    ReaderWriterState { (context, status) =>
      // find all matching properties and validate them
      val validated = remaining.flatMap {
        prop => {
          // TODO: naming
          val matchedPPs = patternProps.map(x => x.attrs.filter(pp => {
            val pattern = Pattern.compile(pp.name)
            val matcher = pattern.matcher(prop._1)
            matcher.find()
          })).getOrElse(Seq.empty)
          matchedPPs.map(pp =>
            prop._1 -> Validator.processor.process(pp.qbType, prop._2, context)
          )
        }
      }
      val validatedProps: ValidatedProperties = ResultAggregator.aggregateResults(validated)
      val unmatchedProps: Seq[(String, JsValue)] = remaining.filterNot(prop =>
        validatedProps      .valid.contains(prop._1)
      )
      ((), unmatchedProps, status.copy(validated = status.validated + validatedProps))
    }


  private def validateAdditionalProps(schema: QBClass, unmatched: Seq[(String, JsValue)]): ValidationStep[Unit] = {

    def validateUnmatched(qbType: QBType, context: Context): ValidatedProperties = {
      val validated: Seq[(String, VA[JsValue])] = unmatched.map { attr =>
        attr._1 -> Validator.processor.process(qbType, attr._2, context.copy(
          path = context.path \ attr._1
        )
        )
      }
      ResultAggregator.aggregateResults(validated)
    }

    ReaderWriterState { (context, status) =>
      println(unmatched)
      if (unmatched.isEmpty) {
        ((), (), status)
      } else {
        val schema = additionalProps.schema
        schema match {
          case QBBooleanConstant(enabled) =>
            if (enabled) {
              // TODO: syntax
              ((), (), status.copy(validated = ValidatedProperties(status.validated.valid ++ unmatched, status.validated.invalid)))
            } else {
              // TODO: empty path
              ((), (), status.copy(validated = ValidatedProperties(status.validated.valid, status.validated.invalid :+ Path -> failure(s"additionalProperties: $unmatched"))))
            }
          case _ =>
            // TODO
            val additionalPropsSchema = schema
            val validationStatus: ValidatedProperties = validateUnmatched(additionalPropsSchema, context)
            if (validationStatus.invalid.nonEmpty) {
              // TODO syntax
              ((), (), status.copy(validated = ValidatedProperties(status.validated.valid, status.validated.invalid :+ Path -> failure(s"additional properties: ${validationStatus.invalid}"))))
            } else {
              ((), (), status.copy(validated = status.validated.+(validationStatus)))
            }
        }
      }
    }
  }
}

// TODO: model dependencies as own type
case class DependenciesRule(dependencies: QBClass) extends ValidationRule {

  override def apply(schema: QBType, json: JsValue, context: Context): ValidationResult = (schema, json) match {
    case (cls: QBClass, obj: JsObject) =>
      validateDependencies(cls, obj, context)
    // TODO: empty string
    case _ => ValidationResult(schema, Success(json), context)
  }

  def extendSchema(baseSchema: QBType, schemaDep: QBAttribute): QBType = {
    (baseSchema, schemaDep.qbType) match {
      case (cls: QBClass, extension@QBClass(_, _, _, _)) => cls ++ extension
      case _ => baseSchema
    }
  }

  def validatePropertyDependency(obj: JsObject, propName: String, dependencies: Seq[String], context: Context): ValidatedProperties = {

    // check if property is present at all
    val mandatoryProps = obj.fields.find(_._1 == propName)
      .map(_ => dependencies)
      .getOrElse(Seq.empty[String])

    // if present, make sure all dependencies are fulfilled
    // TODO: review
    val result: Seq[(String, VA[JsValue])] = mandatoryProps.map(prop => obj.fields.find(_._1 == prop)
      .fold[(String, VA[JsValue])](
        prop -> Failure(Seq(Path -> failure(s"Missing property dependency $prop.")))
      )(s => prop -> success(s._2))
    )

    ResultAggregator.aggregateResults(result)
  }

  private def validateDependencies(schema: QBClass, obj: JsObject, context: Context): ValidationResult = {
    val res = dependencies.properties.foldLeft((schema, ValidatedProperties.empty): (QBType, ValidatedProperties))((acc, dep) => dep match {
      case QBAttribute(name, arr: QBArrayConstant, _) =>
        val result: ValidatedProperties = validatePropertyDependency(obj, name, arr.seq, context)
        (acc._1, acc._2 + result)
      case attr@QBAttribute(_, cls: QBClass, _) => (extendSchema(acc._1, attr), acc._2)
    })
    ValidationResult(res._1, ResultAggregator.toVA(res._2), context)
  }

}


//case class AdditionalPropertiesRule(schema: QBType) extends SchemaBasedValidationRule {
//  override val schemas: Seq[QBType] = Seq(schema)
//
//  override def copy(schemas: Seq[QBType]): SchemaBasedValidationRule = AdditionalPropertiesRule(schemas.head)
//
//  override def rule: Rule[JsValue, JsValue] = Rule.fromMapping{
//    case JsObject(fields) =>
//
//    case _ => Failure(List(ValidationError("qb.object.expected")))
//  }
//}

//----------------------------------------------------------
// 	Format Rules
//----------------------------------------------------------
/**
 * Format rule definition.
 *
 * @tparam A
 *           the type to be validated
 */
trait FormatRule[A <: JsValue] extends ValidationRule {
  def format: String
}

/**
 * Rule that checks whether a string matches the format of a datetime, that is, whether it can be parsed the
 * Joda DateTime class.
 */
//object DateTimeRule extends FormatRule[JsString] {
//  val format = "date-time"
//
//  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule {
//    case string: JsString =>
//      Rules.date().validate(string.value) match {
//        case Success(_) => Success(string)
//        case Failure(_) => Failure(List(
//          ValidationError("qb.format." + format,
//            Json.obj("string" -> string, "format" -> format)
//          )
//        ))
//      }
//  }
//}
//
///**
// * Rule that checks whether a string matches the format of a POSIX time, that is, whether it number
// * is whole at positive.
// */
//object PosixTimeRule extends FormatRule[JsNumber] {
//  val format = "posix-time"
//
//  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
//    case date: JsNumber =>
//      if (date.value.isWhole && date.value > 0) {
//        Success(date)
//      } else {
//        Failure(
//          List(
//            ValidationError("qb.format." + format,
//              Json.obj("number" -> date, "format" -> format)
//            )
//          )
//        )
//      }
//    case _ => Failure(List(ValidationError("qb.number.expected")))
//  }
//}
