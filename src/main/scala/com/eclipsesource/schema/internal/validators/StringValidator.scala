package com.eclipsesource.schema.internal.validators

import java.util.regex.Pattern

import com.eclipsesource.schema.SchemaString
import com.eclipsesource.schema.internal.constraints.Constraints.StringConstraints
import com.eclipsesource.schema.internal.{Context, Results}
import play.api.data.mapping.{Failure, Rule, Success, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsString, JsValue, Json}

object StringValidator extends SchemaTypeValidator[SchemaString] {

  def validate(schema: SchemaString, json: => JsValue, context: Context): VA[JsValue] = {
    val constraints = schema.constraints
    Results.merge(
      (
        validateMinLength(constraints, context) |+|
          validateMaxLength(constraints, context) |+|
          validatePattern(constraints, context)
        ).validate(json),
      AnyConstraintValidator.validate(json, constraints.any, context)
    )
  }

  def validatePattern(constraints: StringConstraints, context: Context): Rule[JsValue, JsValue] = {
    val format: Option[String] = constraints.pattern
    Rule.fromMapping {
      case json@JsString(string) => format match {
        case Some(pattern) =>
          val compiled = Pattern.compile(pattern)
          val matcher = compiled.matcher(string)
          if(matcher.find()) {
            Success(json)
          } else {
            Results.failure(
              s"$string does not match pattern $pattern.",
              context.schemaPath.toString(),
              context.instancePath.toString(),
              context.root,
              json
            )
          }
        case None => Success(json)
      }
      case json => expectedString(json, context)
    }
  }

  def validateMinLength(constraints: StringConstraints, context: Context): Rule[JsValue, JsValue] = {
    val minLength = constraints.minLength.getOrElse(0)
    Rule.fromMapping {
      case json@JsString(string) =>
        if (lengthOf(string) >= minLength) {
          Success(json)
        } else {
          Results.failure(
            s"$string violates min length of $minLength",
            context.schemaPath.toString(),
            context.instancePath.toString(),
            context.root,
            json
          )
        }
      case json => expectedString(json, context)
    }
  }

  def validateMaxLength(constraints: StringConstraints, context: Context): Rule[JsValue, JsValue] = {
    val maxLength = constraints.maxLength
    Rule.fromMapping {
      case json@JsString(string) => maxLength match {
        case None => Success(json)
        case Some(max) =>
          if (lengthOf(string) <= max) {
            Success(json)
          } else {
            Results.failure(
              s"$string violates max length of $max",
              context.schemaPath.toString(),
              context.instancePath.toString(),
              context.root,
              json
            )
          }
      }
      case json => expectedString(json, context)
    }
  }

  private def expectedString(json: JsValue, context: Context) =
    Results.failure(
      "Expected string",
      context.schemaPath.toString(),
      context.instancePath.toString(),
      context.root,
      json
    )

  private def lengthOf(str: String): Int = {
    val pattern = Pattern.compile("\u0B95\u0BCD\u0BB7\\p{M}?|\\p{L}|\\p{Nd}\\p{M}?")
    val matcher = pattern.matcher(str)
    val chars = Iterator.continually(matcher.find()).takeWhile(_ == true)
    chars.size
  }
}
