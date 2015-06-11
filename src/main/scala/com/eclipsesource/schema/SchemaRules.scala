package com.eclipsesource.schema

import java.util.regex.Pattern

import com.eclipsesource.schema.internal.RefResolver
import play.api.data.mapping._
import play.api.data.mapping.json.Rules
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.collection.immutable.HashSet

//----------------------------------------------------------
// 	Rule definitions
//----------------------------------------------------------

/**
 * Definition of a basic validation rule.
 *
 */
trait ValidationRule {

  /**
   * Validates the given object and returns the validation status
   *
   * @param data
   *         the object to be validated
   * @return
   *         a validation result that may succeed or fail
   */
  def validate(data: JsValue): VA[JsValue] = rule.validate(data)

  //  def rule: Rule[A, A]


  def rule: Rule[JsValue, JsValue]
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
  def rules: Set[ValidationRule]

  val successRule = Rule[JsValue, JsValue] { js => Success(js)}

  override lazy val rule: Rule[JsValue, JsValue] = {
    if (rules.isEmpty) {
      successRule
    } else {
      rules.map(_.rule).reduceLeft(_ |+| _)
    }
  }

  //  def rule: Rule[A, A] = rules.map(_.rule).reduceLeft(_ |+| _)

  //override def validate(path: QBPath, a: A): VA[A] = rule.validate(a)
}

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
// 	Object Rules
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



case class KeyValueRule[A <: JsValue](key: String, value: String) extends ValidationRule {

  override val rule: Rule[JsValue, JsValue] = Rule.fromMapping { Success(_) }

}

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
object DateTimeRule extends FormatRule[JsString] {
  val format = "date-time"

  val rule: Rule[JsValue, JsValue] = CatchUndefinedRule {
    case string: JsString =>
      Rules.date().validate(string.value) match {
        case Success(_) => Success(string)
        case Failure(_) => Failure(List(
          ValidationError("qb.format." + format,
            Json.obj("string" -> string, "format" -> format)
          )
        ))
      }
  }
}

/**
 * Rule that checks whether a string matches the format of a POSIX time, that is, whether it number
 * is whole at positive.
 */
object PosixTimeRule extends FormatRule[JsNumber] {
  val format = "posix-time"

  val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
    case date: JsNumber =>
      if (date.value.isWhole && date.value > 0) {
        Success(date)
      } else {
        Failure(
          List(
            ValidationError("qb.format." + format,
              Json.obj("number" -> date, "format" -> format)
            )
          )
        )
      }
    case _ => Failure(List(ValidationError("qb.number.expected")))
  }
}
