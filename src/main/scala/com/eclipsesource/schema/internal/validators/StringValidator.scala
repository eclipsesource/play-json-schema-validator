package com.eclipsesource.schema.internal.validators

import java.util.regex.Pattern

import com.eclipsesource.schema.SchemaString
import com.eclipsesource.schema.internal.constraints.Constraints.StringConstraints
import com.eclipsesource.schema.internal.{Context}
import play.api.data.mapping.{Rule, Success, VA}
import play.api.libs.json.{JsString, JsValue}

import scala.util.matching.Regex

object StringValidator extends SchemaTypeValidator[SchemaString] {

  def validate(schema: SchemaString, json: => JsValue, context: Context): VA[JsValue] = {
    val reader = for {
      minLength <- validateMinLength
      maxLength <- validateMaxLength
      pattern   <- validatePattern
      format    <- validateFormat
    } yield minLength |+| maxLength |+| pattern |+| format
    reader.run((schema.constraints, context))
      .repath(_.compose(context.instancePath))
      .validate(json)
  }

  val validatePattern: scalaz.Reader[(StringConstraints, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val format: Option[String] = constraints.pattern
      Rule.fromMapping {
        case json@JsString(string) => format match {
          case Some(pattern) =>
            val compiled = Pattern.compile(pattern)
            val matcher = compiled.matcher(string)
            if (matcher.find()) {
              Success(json)
            } else {
              failure(
                s"$string does not match pattern $pattern",
                context.schemaPath,
                context.instancePath,
                json
              )
            }
          case None => Success(json)
        }
        case json => expectedString(json, context)
      }
    }

  val validateMinLength: scalaz.Reader[(StringConstraints, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val minLength = constraints.minLength.getOrElse(0)
      Rule.fromMapping {
        case json@JsString(string) =>
          if (lengthOf(string) >= minLength) {
            Success(json)
          } else {
            failure(
              s"$string violates min length of $minLength",
              context.schemaPath,
              context.instancePath,
              json
            )
          }
        case json => expectedString(json, context)
      }
    }

  val validateMaxLength: scalaz.Reader[(StringConstraints, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val maxLength = constraints.maxLength
      Rule.fromMapping {
        case json@JsString(string) => maxLength match {
          case None => Success(json)
          case Some(max) =>
            if (lengthOf(string) <= max) {
              Success(json)
            } else {
              failure(
                s"$string violates max length of $max",
                context.schemaPath,
                context.instancePath,
                json
              )
            }
        }
        case json => expectedString(json, context)
      }
    }


  val validateFormat: scalaz.Reader[(StringConstraints, Context), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>

      val format = for {
        formatName <- constraints.format
        f <- Formats.get(formatName)
      } yield f

      Rule.fromMapping {
        case json@JsString(string) if constraints.format.isDefined =>
          format match {
            // format found
            case Some(f) =>
              if (f.validate(string)) {
                Success(json)
              } else {
                failure(
                  s"$string does not match format $f",
                  context.schemaPath,
                  context.instancePath,
                  json
                )
              }
            // unknown format
            case None => unknownFormat(json, context, constraints.format.getOrElse(""))
          }
        case json@JsString(string) => Success(json)
        case json => expectedString(json, context)
      }
    }

  private def unknownFormat(json: JsValue, context: Context, format: String) =
    failure(
      s"Unknown format $format",
      context.schemaPath,
      context.instancePath,
      json
    )

  private def expectedString(json: JsValue, context: Context) =
    failure(
      "Expected string",
      context.schemaPath,
      context.instancePath,
      json
    )

  private def lengthOf(str: String): Int = {
    val pattern = Pattern.compile("\u0B95\u0BCD\u0BB7\\p{M}?|\\p{L}|\\p{Nd}\\p{M}?")
    val matcher = pattern.matcher(str)
    val chars = Iterator.continually(matcher.find()).takeWhile(_ == true)
    chars.size
  }
}
