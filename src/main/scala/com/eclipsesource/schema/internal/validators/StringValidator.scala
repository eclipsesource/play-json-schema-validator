package com.eclipsesource.schema.internal.validators

import java.util.regex.Pattern

import com.eclipsesource.schema.SchemaString
import com.eclipsesource.schema.internal.constraints.Constraints.StringConstraints
import com.eclipsesource.schema.internal.{Context, Results}
import play.api.data.mapping.{Failure, Rule, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsString, JsValue, Json}

object StringValidator {

  def validate(schema: SchemaString, json: => JsValue, context: Context): VA[JsValue] = {
    val constraints = schema.constraints
    Results.merge(
      (
        validateMinLength(constraints) |+|
          validateMaxLength(constraints) |+|
          validateFormat(constraints)
        ).validate(json),
      AnyConstraintValidator.validate(json, constraints.any, context)
    )
  }

  def validateFormat(constraints: StringConstraints): Rule[JsValue, JsValue] = {
    val format: Option[String] = constraints.format
    Rule.fromMapping {
        case json@JsString(string) => format match {
          case Some(pattern) =>
          // TODO: matcher
          if(pattern.matches(string)) {
            Success(json)
          } else {
            Failure(
              Seq(
                ValidationError("format violated",
                  Json.obj("pattern" -> pattern, "string" -> string)
                )
              )
            )
          }
          case None => Success(json)
        case _ => expectedString
      }
    }
  }

  def validateMinLength(constraints: StringConstraints): Rule[JsValue, JsValue] = {
    val minLength = constraints.minLength.getOrElse(0)
    Rule.fromMapping {
      case json@JsString(string) =>
        if (lengthOf(string) >= minLength) {
          Success(json)
        } else {
          Failure(
            Seq(
              ValidationError("minLength violated",
                Json.obj("minLength" -> minLength, "string" -> string)
              )
            )
          )
        }
      case _ => expectedString
    }
  }

  def validateMaxLength(constraints: StringConstraints): Rule[JsValue, JsValue] = {
    val maxLength = constraints.maxLength
    Rule.fromMapping {
      case json@JsString(string) => maxLength match {
        case None => Success(json)
        case Some(max) =>
          if (lengthOf(string) <= max) {
            Success(json)
          } else {
            Failure(
              Seq(
                ValidationError("maxLength violated",
                  Json.obj("maxLength" -> maxLength, "string" -> string)
                )
              )
            )
          }
        case _ => expectedString
      }
    }
  }

  private def expectedString = Failure(Seq(ValidationError("Expected string")))

  private def lengthOf(str: String): Int = {
    val pattern = Pattern.compile("\u0B95\u0BCD\u0BB7\\p{M}?|\\p{L}\\p{M}?")
    val matcher = pattern.matcher(str)
    val chars = Iterator.continually(matcher.find()).takeWhile(_ == true)
    chars.size
  }
}
