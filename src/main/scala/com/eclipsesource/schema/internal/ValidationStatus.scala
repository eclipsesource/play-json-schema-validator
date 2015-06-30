package com.eclipsesource.schema.internal

import play.api.data.mapping.{Success, Failure, VA}
import play.api.libs.json.{JsObject, JsValue}

case class ValidationStatus(validProps: ValidProperties = List.empty, invalidProps: InvalidProperties = List.empty) {

  def +(otherStatus: ValidationStatus) =
    ValidationStatus(validProps ++ otherStatus.validProps, invalidProps ++ otherStatus.invalidProps)

  def addToInvalidProps(invalidProperties: InvalidProperties) =
    ValidationStatus(validProps, invalidProps ++ invalidProperties)

  def addToInvalidProps(invalidProp: VA[JsValue]) =
    ValidationStatus(validProps, invalidProps :+ invalidProp)

  def addToValidProps(validProperties: ValidProperties) =
    ValidationStatus(validProps ++ validProperties, invalidProps)

  def addToValidProps(validProp: (String, JsValue)) =
    ValidationStatus(validProps :+ validProp, invalidProps)


}

object ValidationStatus {

  def empty = ValidationStatus()


}