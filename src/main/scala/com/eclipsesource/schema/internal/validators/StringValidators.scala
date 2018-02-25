package com.eclipsesource.schema.internal.validators

import java.text.BreakIterator

import com.eclipsesource.schema.SchemaResolutionContext
import com.eclipsesource.schema.internal.{Keywords, SchemaUtil}
import com.eclipsesource.schema.internal.validation.Rule
import com.osinka.i18n.{Lang, Messages}
import jdk.nashorn.internal.runtime.regexp.RegExpFactory
import play.api.libs.json.{JsString, JsValue}

import scala.util.Try
import scalaz.Success

object StringValidators {
  def validatePattern(pat: Option[String])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      val format: Option[String] = pat
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
                  SchemaUtil.failure(
                    Keywords.String.Pattern,
                    Messages("str.pattern", string, pattern),
                    context.schemaPath,
                    context.instancePath,
                    json
                  )
                }
            }).getOrElse(
              SchemaUtil.failure(
                Keywords.String.Pattern,
                Messages("str.invalid.pattern", pattern),
                context.schemaPath,
                context.instancePath,
                json
              )
            )
          case None => Success(json)
        }
      }
    }

  def validateMinLength(min: Option[Int])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      val minLength = min.getOrElse(0)
      Rule.fromMapping {
        case json@JsString(string) =>
          if (lengthOf(string) >= minLength) {
            Success(json)
          } else {
            SchemaUtil.failure(
              Keywords.String.MinLength,
              Messages("str.min.length", string, minLength),
              context.schemaPath,
              context.instancePath,
              json
            )
          }
      }
    }

  def validateMaxLength(maxLength: Option[Int])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>
      Rule.fromMapping {
        case json@JsString(string) => maxLength match {
          case None => Success(json)
          case Some(max) =>
            if (lengthOf(string) <= max) {
              Success(json)
            } else {
              SchemaUtil.failure(
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


  def validateFormat(f: Option[String])(implicit lang: Lang): scalaz.Reader[SchemaResolutionContext, Rule[JsValue, JsValue]] =
    scalaz.Reader { context =>

      val format = for {
        formatName <- f
        f <- context.formats.get(formatName)
      } yield f

      Rule.fromMapping {
        case json@JsString(string) if f.isDefined =>
          format match {
            // format found
            case Some(f) =>
              if (f.validate(json)) {
                Success(json)
              } else {
                SchemaUtil.failure(
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
