package com.eclipsesource.schema

import play.api.data.mapping.{VA}
import play.api.libs.json.{JsUndefined, JsValue}

import scalaz.{Semigroup, ReaderWriterState}

package object internal {

  // provide semigroup for unit for use with ReaderWriterState
  implicit def UnitSemigroup: Semigroup[Unit] = new Semigroup[Unit] {
    override def append(f1: Unit, f2: => Unit): Unit = ()
  }

  case object JsAbsent extends JsUndefined("optional")

  /**
   * Type params in this order are:
   * - reader
   * - writer
   * - state
   * - value
   */
  type ValidationStep[A] = ReaderWriterState[Context, Unit, VA[JsValue], A]
  type Props = Seq[(String, JsValue)]
  type PropertyValidationResult = (String, VA[JsValue])
  type ValidProperties = Seq[(String, JsValue)]
  type InvalidProperties = Seq[VA[JsValue]]

}
