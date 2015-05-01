package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

trait JSONSchemaReads {

  object UnReadable extends Reads[QBType] {
    def reads(qb: JsValue) = JsError("unmatched.reader")
  }

  implicit def valueReader: Reads[QBType] = (__ \ "type").read[String].flatMap {
    case "boolean" => booleanReader.asInstanceOf[Reads[QBType]]
    case "string" => stringReader.asInstanceOf[Reads[QBType]]
    case "integer" => integerReader.asInstanceOf[Reads[QBType]]
    case "number" => numberReader.asInstanceOf[Reads[QBType]]
    case "array" => arrayReader.asInstanceOf[Reads[QBType]]
    case "object" => objectReader.asInstanceOf[Reads[QBType]]
    case _ => UnReadable.asInstanceOf[Reads[QBType]]
  }

  implicit def booleanReader: Reads[QBBoolean] = {
    (__ \ "type").read(equals("boolean")).map(_ => QBBooleanImpl())
  }

  implicit def numberReader: Reads[QBNumber] = {
    (__ \ "type").read(equals("number")) andKeep (
      (__ \ "minimum").readNullable[Double] and
      (__ \ "maximum").readNullable[Double] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[Double]).tupled
      .map(opts => {
        val isMinExclusive = opts._3.getOrElse(false)
        val isMaxExclusive = opts._4.getOrElse(false)
        val minRule = opts._1.map(MinRule(_, isMinExclusive))
        val maxRule = opts._2.map(MaxRule(_, isMaxExclusive))
        val multipleOf = opts._5.map(MultipleOfRule)
        QBNumberImpl(Set(minRule, maxRule, multipleOf).filterNot(_.isEmpty).map(_.get))
      })
  }

  implicit def integerReader: Reads[QBInteger] = {
    (__ \ "type").read(equals("integer")) andKeep (
      (__ \ "minimum").readNullable[Int] and
      (__ \ "maximum").readNullable[Int] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[Int]).tupled
      .map(opts => {
        val isMinExclusive = opts._3.getOrElse(false)
        val isMaxExclusive = opts._4.getOrElse(false)
        val minRule = opts._1.map(MinRule(_, isMinExclusive))
        val maxRule = opts._2.map(MaxRule(_, isMaxExclusive))
        val multipleOf = opts._5.map(MultipleOfRule(_))
        QBIntegerImpl(Set(minRule, maxRule, multipleOf).filterNot(_.isEmpty).map(_.get))
      })
  }

  implicit def stringReader: Reads[QBString] = {
    (__ \ "type").read(equals("string")) andKeep (
      (__ \ "minLength").readNullable[Int] and
      (__ \ "maxLength").readNullable[Int]).tupled
      .map(opts => {
        val minRule = opts._1.map(MinLengthRule)
        val maxRule = opts._2.map(MaxLengthRule)
        QBStringImpl(Set(minRule, maxRule).filterNot(_.isEmpty).map(_.get))
      })
  }

  implicit def arrayReader: Reads[QBArray] = {
    // TODO: parent?
    (__ \ "type").read(equals("array")) andKeep
      (__ \ "items").read[QBType].map(i => QBArray(() => i, None))
  }

  implicit def objectReader: Reads[QBClass] = {
    (__ \ "type").read(equals("object")) andKeep (
      (__ \ "properties").read[Map[String, QBType]] and
      ((__ \ "required").read[List[String]] orElse Reads.pure(List.empty))).tupled
      .map { objectSchema =>
        val (properties, required) = objectSchema
        properties.map { property =>
          if (required.contains(property._1)) {
            (property._1, property._2, List())
          } else {
            (property._1, property._2, List(QBOptionalAnnotation()))
          }
        }.toList
      }.map((tuples: List[(String, QBType, List[QBOptionalAnnotation])]) => obj(tuples))
  }

}
