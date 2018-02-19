package com.eclipsesource.schema

import com.eclipsesource.schema.internal.validation.VA
import play.api.libs.json._

import scala.language.reflectiveCalls
import scala.util.{Failure, Success, Try}
import scalaz.{ReaderWriterState, Semigroup}

package object internal {

  // provide semigroup for unit for use with ReaderWriterState
  implicit def UnitSemigroup: Semigroup[Unit] = new Semigroup[Unit] {
    override def append(f1: Unit, f2: => Unit): Unit = ()
  }

  implicit class TryExtensions[A](t: Try[A]) {
    def toJsonEither: Either[JsonValidationError, A] =  t match {
      case Success(result) => Right(result)
      case Failure(throwable) => Left(JsonValidationError(throwable.getMessage))
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
