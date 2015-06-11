package com.eclipsesource.schema.internal

import java.util.regex.Pattern

import com.eclipsesource.schema._
import play.api.data.mapping.{Failure, Path, VA}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsObject, JsValue}

import scalaz.ReaderWriterState

object ObjectValidator extends SchemaValidator {

  def validateObject(schema: QBClass, json: => JsValue, context: Context): VA[JsValue] = {

    def validateJson(schema: QBClass, c: Context) = {
      json match {
        case jsObject@JsObject(props) =>
          val validation = for {
            updSchema <- validateDependencies(schema, jsObject)
            remaining <- validateProps(updSchema, jsObject)
            unmatched <- validatePatternProps(updSchema, remaining)
            _ <- validateAdditionalProps(updSchema, unmatched)
          } yield updSchema

          val (_, updatedSchema, status) = validation.run(c, ValidationStatus.empty)

          val allFailures: Seq[(Path, Seq[ValidationError])] = status.invalidProps
          val successFields: Seq[(String, JsValue)] = status.validProps

          if (allFailures.nonEmpty) {
            Failure(allFailures)
          } else {
            Validator.processor.validate(updatedSchema, JsObject(successFields), c)
          }
        case _ => Validator.processor.validate(schema, json, c)
      }
    }

    // check if any property is a ref
    val reference = schema.properties.collectFirst { case QBAttribute("$ref", ref@QBRef(_, _, _), _) => ref}

    // TODO: Validator
    reference.flatMap(ref => RefResolver.resolveRef(ref, context)).map {
      case cls: QBClass => validateJson(cls, context)
      case x => Validator.processor.process(x, json, context)
    }.getOrElse(validateJson(schema, context))
  }

  private def validateProps(schema: QBClass, obj: => JsObject): ValidationStep[Props] =
    ReaderWriterState { (context, status) =>
      val validated = schema.props.map { attr =>
        val value = obj \ attr.name
        // TODO: Validator
        attr.name -> Validator.processor.process(
          attr.qbType,
          value,
          context.copy(
            path = context.path \ "properties" \ attr.name,
            annotations = attr.annotations
          )
        )
      }

      val validationStatus = ResultAggregator.aggregateResults(validated, context)
      val unvalidatedProps: Props = obj.fields.filterNot(field =>
        validationStatus.validProps.map(_._1).contains(field._1)
      )
      ((), unvalidatedProps, status.add(validationStatus))
    }


  private def validatePatternProps(schema: QBClass, props: Props): ValidationStep[Props] =
    ReaderWriterState { (context, status) =>
      // find all matching properties and validate them
      val validated = props.flatMap {
        prop => {
          val matchedPPs = schema.patternProperties.filter(pp => {
            val pattern = Pattern.compile(pp.name)
            val matcher = pattern.matcher(prop._1)
            matcher.find()
          })
          matchedPPs.map(pp =>
            // TODO: Validator
            prop._1 -> Validator.processor.process(pp.qbType, prop._2, context)
          )
        }
      }
      val validationStatus = ResultAggregator.aggregateResults(validated, context)
      val unmatchedProps = props.filterNot(prop =>
        validationStatus.validProps.map(_._1).contains(prop._1)
      )
      ((), unmatchedProps, status.add(validationStatus))
    }

  private def validateAdditionalProps(schema: QBClass, unmatchedFields: Props): ValidationStep[Unit] = {

    def validateUnmatched(qbType: QBType, context: Context): ValidationStatus = {
      val validated = unmatchedFields.map { attr =>
        attr._1 -> Validator.processor.process(
          qbType, attr._2, context.copy(
            path = context.path \ attr._1
          )
        )
      }
      ResultAggregator.aggregateResults(validated, context)
    }

    ReaderWriterState { (context, status) =>
      if (unmatchedFields.isEmpty) {
        ((), (), status)
      } else {
        schema.additionalProperties.schema match {
          case QBBooleanConstant(enabled) =>
            if (enabled) {
              ((), (), status.addToValidProps(unmatchedFields))
            } else {
              ((), (), status.addToInvalidProps(failure(context.path, s"patternProperties: $unmatchedFields")))
            }
          case _ => val additionalPropsSchema = schema.additionalProperties.schema
            val validationStatus = validateUnmatched(additionalPropsSchema, context)
            if (validationStatus.invalidProps.nonEmpty) {
              ((), (), status.addToInvalidProps(
                failure(context.path, s"additional properties: ${validationStatus.invalidProps}")))
            } else {
              ((), (), status.add(validationStatus))
            }
        }
      }
    }
  }

  private def validateDependencies(schema: QBClass, obj: JsObject): ValidationStep[QBClass] = {

    def extendSchemaByDependency(baseSchema: QBClass, schemaDep: QBAttribute): QBClass = {
      schemaDep.qbType match {
        case extension@QBClass(_, _, _) => baseSchema ++ extension
        case _ => baseSchema
      }
    }

    def validatePropertyDependency(propName: String, dependencies: Seq[String], context: Context): ValidationStatus = {

      // check if property is present at all
      val mandatoryProps = obj.fields.find(_._1 == propName)
        .map(_ => dependencies)
        .getOrElse(Seq.empty[String])

      // if present, make sure all dependencies are fulfilled
      val result = mandatoryProps.map(prop => obj.fields.find(_._1 == prop)
        .fold[PropertyValidationResult](
          failure(prop, context.path \ prop, s"Missing property dependency $prop.")
        )(success)
      )

      ResultAggregator.aggregateResults(result, context)
    }

    ReaderWriterState { (context, status) =>
      val dependencies = schema.dependencies
      val (updatedSchema, updatedStatus) = dependencies.foldLeft((schema, status))((acc, dep) => dep match {
        case QBAttribute(name, arr: QBArrayConstant, _) => {
          val status = validatePropertyDependency(name, arr.seq, context)
          (acc._1, acc._2.addToValidProps(status.validProps).addToInvalidProps(status.invalidProps))
        }
        case QBAttribute(_, cls: QBClass, _) => (extendSchemaByDependency(acc._1, dep), acc._2)
      })
      ((), updatedSchema, updatedStatus)
    }
  }
}

//  private def validatePropertiesCount(schema: QBClass, json: JsObject): ValidationStep[Unit] = {
//
//
//    ReaderWriterState { (context, status) =>
//    }
//  }
//
//
// private def validateMinProperties()
//        if (obj.fieldSet.size >= minProperties) {
//          Success(obj)
//        } else {
//          Failure(
//            List(
//              ValidationError("qb.obj.min-props",
//                Json.obj("minProperties" -> minProperties, "object" -> obj)
//              )
//            )
//          )
//        }
//      case _ => Failure(List(ValidationError("qb.object.expected")))
//    }
//  }
//
//  case class MaxPropertiesRule(maxProperties: Int) extends ValidationRule {
//
//    val rule: Rule[JsValue, JsValue] = Rule.fromMapping {
//      case obj: JsObject =>
//        if (obj.fieldSet.size <= maxProperties) {
//          Success(obj)
//        } else {
//          Failure(
//            List(
//              ValidationError("qb.obj.max-props",
//                Json.obj("maxProperties" -> maxProperties, "object" -> obj)
//              )
//            )
//          )
//        }
//      case _ => Failure(List(ValidationError("qb.object.expected")))
//    }
//
//  }
//}
