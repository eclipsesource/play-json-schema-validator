package com.eclipsesource.schema

import play.api.data.mapping.{Success, Failure, VA, Path}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsUndefined, JsValue}

import scalaz.{Semigroup, ReaderWriterState}

/**
 * Created by Edgar on 24.06.2015.
 */
package object internal {

  // provide semigroup for unit for use with ReaderWriterState
  implicit def UnitSemigroup: Semigroup[Unit] = new Semigroup[Unit] {
    override def append(f1: Unit, f2: => Unit): Unit = ()
  }

  case object JsAbsent extends JsUndefined("qb.accepted")

  /**
   * Type params in this order are:
   * - reader
   * - writer
   * - state
   * - value
   */
  type ValidationStep[A] = ReaderWriterState[Context, Unit, ValidationStatus, A]
  type Props = Seq[(String, JsValue)]
  type PropertyValidationResult = (String, VA[JsValue])
  type ValidProperty = (String, JsValue)
  type ValidProperties = Seq[(String, JsValue)]
  type InvalidProperty = (Path, Seq[ValidationError])
  type InvalidProperties = Seq[(Path, Seq[ValidationError])]

  case class ValidationStatus(validProps: ValidProperties = List.empty, invalidProps: InvalidProperties = List.empty) {

    private[schema] def add(otherStatus: ValidationStatus) =
      ValidationStatus(validProps ++ otherStatus.validProps, invalidProps ++ otherStatus.invalidProps)

    private[schema] def addToInvalidProps(invalidProperties: InvalidProperties) =
      ValidationStatus(validProps, invalidProps ++ invalidProperties)

    private[schema] def addToInvalidProps(invalidProp: InvalidProperty) =
      ValidationStatus(validProps, invalidProps :+ invalidProp)

    private[schema] def addToValidProps(validProperties: ValidProperties) =
      ValidationStatus(validProps ++ validProperties, invalidProps)

    private[schema] def addToValidProps(validProp: ValidProperty) =
      ValidationStatus(validProps :+ validProp, invalidProps)
  }

  object ValidationStatus {
    def empty = ValidationStatus()
  }

  def failure(propName: String, path: Path, errorMsg: String): PropertyValidationResult = {
    (propName, Failure(Seq(path -> Seq(ValidationError(errorMsg)))))
  }

  def failure(path: Path, errorMsg: String): InvalidProperty = {
    path -> List(ValidationError(errorMsg))
  }

  def success(prop: (String, JsValue)): PropertyValidationResult = {
    prop._1 -> Success(prop._2)
  }

}
