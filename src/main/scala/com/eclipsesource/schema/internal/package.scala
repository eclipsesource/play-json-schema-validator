package com.eclipsesource.schema

import com.eclipsesource.schema.internal.validation.VA
import play.api.libs.json._

import scalaz.{ReaderWriterState, Semigroup}



package object internal {

  // provide semigroup for unit for use with ReaderWriterState
  implicit def UnitSemigroup: Semigroup[Unit] = new Semigroup[Unit] {
    override def append(f1: Unit, f2: => Unit): Unit = ()
  }

  /**
   * Type params in this order are:
   * - reader
   * - writer
   * - state
   * - value
   */

  type ValidationStep[A] = ReaderWriterState[ResolutionContext, Unit, VA[JsValue], A]
  type Props = Seq[(String, JsValue)]
  type PropertyValidationResult = (String, VA[JsValue])
  type ValidProperties = Seq[(String, JsValue)]
  type InvalidProperties = Seq[VA[JsValue]]

}
