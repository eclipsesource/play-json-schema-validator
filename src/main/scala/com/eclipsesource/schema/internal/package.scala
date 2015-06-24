package com.eclipsesource.schema

import play.api.data.mapping
import play.api.data.mapping._
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsArray, JsObject, JsUndefined, JsValue}

import scalaz.{ReaderWriterState, Semigroup}

package object internal {

  case object JsAbsent extends JsUndefined("qb.accepted")


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
  type ValidationStep[A] = ReaderWriterState[Context, Unit, ValidationState, A]
//  type PropertyValidationResult = (String, VA[JsValue])
  type PropertyValidationResult = VA[JsValue]
  type ValidProperty = (String, JsValue)
  type ValidProperties = Seq[(String, JsValue)]
  type InvalidProperty = (Path, Seq[ValidationError])
  type InvalidProperties = Seq[(Path, Seq[ValidationError])]

  case class ValidatedProperties(valid:Seq[(String, JsValue)] = Seq.empty, invalid: Seq[(Path, Seq[ValidationError])] = Seq.empty) {
    def +(props: ValidatedProperties): ValidatedProperties = {
      ValidatedProperties(valid ++ props.valid, invalid ++ props.invalid)
    }
    // TODO we should be able to provide a single method handling both cases
    def withNewError(invalidProperties: (Path, Seq[ValidationError])): ValidatedProperties = {
      copy(invalid = invalid :+ invalidProperties)
    }

    def withNewErrors(invalidProperties: Seq[(Path, Seq[ValidationError])]): ValidatedProperties = {
      copy(invalid = invalid ++ invalidProperties)
    }

    def withNewValidProperty(validProperty: (String, JsValue)): ValidatedProperties = {
      copy(valid = valid :+ validProperty)
    }
  }

  case object ValidatedProperties {
    def empty = ValidatedProperties()
  }

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

  def failure(errorMsg: String): Seq[ValidationError] = {
    // TODO empty path
    Seq(ValidationError(errorMsg))
  }

  def failure(path: Path, errorMsg: String): VA[JsValue] = {
    Failure(Seq(path -> Seq(ValidationError(errorMsg))))
  }

  def success(prop: JsValue): VA[JsValue] = {
    Success(prop)
  }

  object ResultAggregator {
    // TODO: prop._1 is ignored
    def aggregateResults(validatedProps: Seq[(String, VA[JsValue])]): ValidatedProperties = {
      validatedProps.foldLeft(ValidatedProperties.empty)((status, prop) => prop._2 match {
        case failure@Failure(err) => status.withNewErrors(err)
        case Success(JsAbsent) => status
        // TODO: empty path
        case Success(undefined: JsUndefined) => status.withNewError(
          Path(prop._1) -> Seq(ValidationError(undefined.error))
        )
        case Success(value) => status.withNewValidProperty(prop._1 -> value)
      })
    }

    def toVA(validated: Seq[VA[JsValue]]): VA[JsValue] = {
      val failures: Seq[Seq[(Path, Seq[mapping.ValidationError])]] = validated.collect { case f@Failure(err) => err }
      if (failures.nonEmpty) {
        Failure(failures.flatten)
      } else {
        Success(JsArray(validated.collect { case Success(succ) => succ}))
      }
    }

    def toVA(validated: ValidatedProperties): VA[JsValue] = {
      val allFailures: Seq[(Path, Seq[ValidationError])] = validated.invalid
      val successFields: Seq[(String, JsValue)] = validated.valid

      if (allFailures.nonEmpty) {
        Failure(allFailures)
      } else {
        Success(JsObject(successFields))
        //        validate(updatedSchema, JsObject(successFields), c)
      }
    }
  }

}
