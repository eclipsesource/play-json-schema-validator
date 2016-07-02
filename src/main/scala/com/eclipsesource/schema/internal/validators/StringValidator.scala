package com.eclipsesource.schema.internal.validators

import java.text.BreakIterator
import java.util.regex.Pattern

import com.eclipsesource.schema.SchemaString
import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema.internal.constraints.Constraints.StringConstraints
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import play.api.libs.json.{JsString, JsValue}

import scalaz.Success

object StringValidator extends SchemaTypeValidator[SchemaString] {

  def validate(schema: SchemaString, json: => JsValue, context: SchemaResolutionContext): VA[JsValue] = {
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

  val validatePattern: scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
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
      }
    }

  val validateMinLength: scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
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
      }
    }

  val validateMaxLength: scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
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
      }
    }


  val validateFormat: scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>

      val format = for {
        formatName <- constraints.format
        f <- context.formats.get(formatName)
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
                  s"$string does not match format ${f.name}",
                  context.schemaPath,
                  context.instancePath,
                  json
                )
              }
            // unknown format
            case None => unknownFormat(json, context, constraints.format.getOrElse(""))
          }
        case json@JsString(string) => Success(json)
      }
    }

  private def unknownFormat(json: JsValue, context: SchemaResolutionContext, format: String) =
    failure(
      s"Unknown format $format",
      context.schemaPath,
      context.instancePath,
      json
    )

  private def lengthOf(text: String, locale: java.util.Locale = java.util.Locale.ENGLISH): Int = {
    val charIterator = java.text.BreakIterator.getCharacterInstance(locale)
    charIterator.setText(text)
    var length = 0
    while (charIterator.next() != BreakIterator.DONE) length += 1
    length
  }
}
