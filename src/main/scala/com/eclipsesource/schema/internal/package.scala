package com.eclipsesource.schema

import com.eclipsesource.schema.internal.validation.{VA, Validated}
import play.api.libs.json._

import scala.language.reflectiveCalls
import scalaz.Failure
import scalaz.{ReaderWriterState, Semigroup}

import scala.util.{Success => ScalaSuccess, Failure => ScalaFailure, Try}

package object internal {

  def failure(keyword: String,
              msg: String,
              schemaPath: JsPath,
              instancePath: JsPath,
              instance: JsValue,
              additionalInfo: JsObject = Json.obj()
             ): Validated[JsonValidationError, JsValue] = {

    def dropSlashIfAny(path: String) = if (path.startsWith("/#")) path.substring(1) else path

    Failure(
      Seq(
        JsonValidationError(msg,
          Json.obj(
            "keyword" -> keyword,
            "schemaPath" -> dropSlashIfAny(schemaPath.toString()),
            "instancePath" -> instancePath.toString(),
            "value" -> instance,
            "errors" ->  additionalInfo
          )
        )
      )
    )
  }


  // provide semigroup for unit for use with ReaderWriterState
  implicit def UnitSemigroup: Semigroup[Unit] = new Semigroup[Unit] {
    override def append(f1: Unit, f2: => Unit): Unit = ()
  }

  implicit class TryExtensions[A](t: Try[A]) {
    def toJsonEither: Either[JsonValidationError, A] =  t match {
      case ScalaSuccess(result) => Right(result)
      case ScalaFailure(throwable) => Left(JsonValidationError(throwable.getMessage))
    }
  }

  implicit class EitherExtensions[A, B](either: Either[A, B]) {
    def orElse(e: => Either[A, B]): Either[A, B] = either match {
      case r@Right(_) => r
      case Left(_)  => e
    }
  }

  def using[T <: { def close() }, A](resource: T)(block: T => A): A = {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }

  /**
   * Type params in this order are:
   * - reader
   * - writer
   * - state
   * - value
   */

  type ValidationStep[A] = ReaderWriterState[SchemaResolutionContext, Unit, VA[JsValue], A]
  type Props = Seq[(String, JsValue)]
  type PropertyValidationResult = (String, VA[JsValue])
  type ValidProperties = Seq[(String, JsValue)]
  type InvalidProperties = Seq[VA[JsValue]]

}
