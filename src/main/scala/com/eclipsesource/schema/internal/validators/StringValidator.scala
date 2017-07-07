package com.eclipsesource.schema.internal.validators

import java.text.BreakIterator

import com.eclipsesource.schema.SchemaString
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.SchemaRefResolver._
import com.eclipsesource.schema.internal.constraints.Constraints.StringConstraints
import com.eclipsesource.schema.internal.validation.{Rule, VA}
import com.osinka.i18n.{Lang, Messages}
import jdk.nashorn.internal.runtime.regexp.RegExpFactory
import play.api.libs.json.{JsString, JsValue}

import scala.util.Try
import scalaz.Success

object StringValidator extends SchemaTypeValidator[SchemaString] {

  def validate(schema: SchemaString, json: => JsValue, context: SchemaResolutionContext)
              (implicit lang: Lang): VA[JsValue] = {
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

  def validatePattern(implicit lang: Lang): scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val format: Option[String] = constraints.pattern
      Rule.fromMapping {
        case json@JsString(string) => format match {
          case Some(pattern) =>
            Try {
              new RegExpFactory().compile(pattern, "")
            }.map(regex => {
                val matcher = regex.`match`(string)
                if (matcher.search(0)) {
                  Success(json)
                } else {
                  failure(
                    Keywords.String.Pattern,
                    Messages("str.pattern", string, pattern),
                    context.schemaPath,
                    context.instancePath,
                    json
                  )
                }
            }).getOrElse(failure(
              Keywords.String.Pattern,
              Messages("str.invalid.pattern", pattern),
              context.schemaPath,
              context.instancePath,
              json
            ))
          case None => Success(json)
        }
      }
    }

  def validateMinLength(implicit lang: Lang): scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
    scalaz.Reader { case (constraints, context) =>
      val minLength = constraints.minLength.getOrElse(0)
      Rule.fromMapping {
        case json@JsString(string) =>
          if (lengthOf(string) >= minLength) {
            Success(json)
          } else {
            failure(
              Keywords.String.MinLength,
              Messages("str.min.length", string, minLength),
              context.schemaPath,
              context.instancePath,
              json
            )
          }
      }
    }

  def validateMaxLength(implicit lang: Lang): scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
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
                Keywords.String.MaxLength,
                Messages("str.max.length", string, max),
                context.schemaPath,
                context.instancePath,
                json
              )
            }
        }
      }
    }


  def validateFormat(implicit lang: Lang): scalaz.Reader[(StringConstraints, SchemaResolutionContext), Rule[JsValue, JsValue]] =
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
              if (f.validate(json)) {
                Success(json)
              } else {
                failure(
                  Keywords.String.Format,
                  Messages("str.format", string, f.name),
                  context.schemaPath,
                  context.instancePath,
                  json
                )
              }
            // validation of unknown format should succeed
            case None => Success(json)
          }
        case json@JsString(_) => Success(json)
      }
    }

  private def lengthOf(text: String, locale: java.util.Locale = java.util.Locale.ENGLISH): Int = {
    val charIterator = java.text.BreakIterator.getCharacterInstance(locale)
    charIterator.setText(text)
    var length = 0
    while (charIterator.next() != BreakIterator.DONE) length += 1
    length
  }
}
