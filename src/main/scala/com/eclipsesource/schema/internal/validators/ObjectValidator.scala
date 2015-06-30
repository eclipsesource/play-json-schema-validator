package com.eclipsesource.schema.internal.validators

import java.util.regex.Pattern

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal._
import play.api.data.mapping._
import play.api.libs.json.{Json, JsUndefined, JsObject, JsValue}

import scalaz.ReaderWriterState

object ObjectValidator {

  def validateObject(schema: SchemaObject, json: => JsValue, context: Context): VA[JsValue] = {

    def validateJson(schema: SchemaObject, c: Context): VA[JsValue] = {
      json match {
        case jsObject@JsObject(props) =>
          val validation = for {
            updSchema <- validateDependencies(schema, jsObject)
            remaining <- validateProps(updSchema, jsObject)
            unmatched <- validatePatternProps(updSchema, remaining)
            _ <- validateAdditionalProps(updSchema, unmatched)
            // TODO: validate min, max properties etc.
          } yield updSchema

          val (_, updatedSchema, va1) = validation.run(c, Success(json))
          // TODO: figure out parameters
          val (_, _, va2) = anyValidation(updatedSchema, jsObject).run(c, Success(jsObject))
          Results.merge(va1, va2)

        case _  =>  anyValidation(schema, json).run(c, Success(json))._3
      }
    }

    // check if any property is a ref
    val reference = schema.properties.collectFirst { case SchemaAttribute("$ref", ref@SchemaRef(_, _, _), _) => ref}

    // TODO: Validator
    reference.flatMap(ref => RefResolver.resolveRef(ref, context)).map {
      case _ if context.visited.contains(reference.get) => Success(json)
      case cls: SchemaObject =>
        // TODO: get
        if (schema.isSubSetOf(cls) && !json.isInstanceOf[JsObject]) {
          Success(json)
        } else {
          val z = if (context.root == schema) context.copy(root = cls, visited = context.visited + reference.get) else context.copy( visited = context.visited + reference.get)
          validateJson(cls, z)
        }
      case x =>
        val z = if (context.root == schema) context.copy(root = x) else context
        Validator.process(x, json, z)
    }.getOrElse(validateJson(schema, context))
  }

  private def validateProps(schema: SchemaObject, obj: => JsObject): ValidationStep[Props] =
    ReaderWriterState { (context, status) =>

      val required = schema.constraints.required.getOrElse(List.empty[String])

      val validated: Seq[(String, VA[JsValue])] = schema.properties.map { attr =>
        obj \ attr.name match {
          case _: JsUndefined => if (required.contains(attr.name)) {
            attr.name -> Results.failure(s"Property ${attr.name} missing")
          } else {
            attr.name -> Success(JsAbsent)
          }
          case value => attr.name -> Validator.process(
            attr.schemaType,
            value,
            context.copy(
              path = context.path \ "properties" \ attr.name,
              annotations = attr.annotations
            )
          )
        }
      }

      val validatedProperties = validated.map(_._1)
      val unvalidatedProps: Props = obj.fields.filterNot(field =>
        validatedProperties.contains(field._1)
      )

      ((), unvalidatedProps, Results.merge(status, Results.aggregateAsObject(validated.filterNot( prop => prop._2 match {
        case Success(JsAbsent) => true
        case _ => false
      } ), context)))
    }


  private def validatePatternProps(schema: SchemaObject, props: Props): ValidationStep[Props] =
    ReaderWriterState { (context, status) =>

      // find all matching properties and validate them
      val validated: Seq[(String, VA[JsValue])] = props.flatMap {
        prop => {
          val matchedPPs = schema.constraints.patternProps.getOrElse(Seq.empty).filter(pp => {
            val pattern = Pattern.compile(pp._1)
            val matcher = pattern.matcher(prop._1)
            matcher.find()
          })
          matchedPPs.map(pp =>
            // TODO: Validator
            prop._1 -> Validator.process(pp._2, prop._2, context)
          )
        }
      }

      val validatedProperties = validated.map(_._1)
      val unmatchedProps = props.filterNot(prop =>
        validatedProperties.contains(prop._1)
      )

      ((), unmatchedProps, Results.merge(status, Results.aggregateAsObject(validated, context)))
    }

  private def validateAdditionalProps(schema: SchemaObject, unmatchedFields: Props): ValidationStep[Unit] = {

    def validateUnmatched(schemaType: SchemaType, context: Context): VA[JsValue] = {
      val validated = unmatchedFields.map { attr =>
        attr._1 -> Validator.process(
          schemaType, attr._2, context.copy(path = context.path \ attr._1)
        )
      }
      Results.aggregateAsObject(validated, context)
    }

    ReaderWriterState { (context, status) =>

      if (unmatchedFields.isEmpty) {
        ((), (), status)
      } else {
        schema.constraints.additionalPropertiesOrDefault match {
          case SchemaBooleanConstant(enabled) =>
            if (enabled) {
              ((), (), Results.merge(status, Success(JsObject(unmatchedFields))))
            } else {
              ((), (), Results.merge(status, Results.failure(context.path, s"additionalProperties: $unmatchedFields")))
            }
          case additionalProp =>
            val validationStatus = validateUnmatched(additionalProp, context)
            ((), (), Results.merge(status, validationStatus))
        }
      }
    }
  }

  private def validateDependencies(schema: SchemaObject, obj: JsObject): ValidationStep[SchemaObject] = {

    def extendSchemaByDependency(baseSchema: SchemaObject, schemaType: SchemaType): SchemaObject = {
      schemaType match {
        case extension@SchemaObject(_, _, _) => val s = baseSchema ++ extension
          println(Json.prettyPrint(Json.toJson(s)))
          s
        case _ => baseSchema
      }
    }

    def validatePropertyDependency(propName: String, dependencies: Seq[String], context: Context): VA[JsValue] = {

      // check if property is present at all
      val mandatoryProps = obj.fields.find(_._1 == propName)
        .map(_ => dependencies)
        .getOrElse(Seq.empty[String])

      // if present, make sure all dependencies are fulfilled
      val result = mandatoryProps.map(prop => obj.fields.find(_._1 == prop).fold(
          prop -> Results.failure(context.path \ prop, s"Missing property dependency $prop.")
        )((field: (String, JsValue)) => Results.success(field))
      )

      Results.aggregateAsObject(result, context)
    }

    ReaderWriterState { (context, status) =>

      val dependencies = schema.constraints.dependencies.getOrElse(Seq.empty)
      val (updatedSchema, updatedStatus) = dependencies.foldLeft((schema, status))((acc, dep) => dep match {
        case (name, arr: SchemaArrayConstant) =>
          val validated = validatePropertyDependency(name, arr.seq, context)
          (acc._1, Results.merge(acc._2, validated))
        case (name, cls: SchemaObject) if obj.keys.contains(name) => (extendSchemaByDependency(acc._1, dep._2), acc._2)
        case _ => acc
      })

      ((), updatedSchema, updatedStatus)
    }
  }

  def anyValidation(schema: SchemaObject, obj: JsValue): ReaderWriterState[Context, Unit, VA[JsValue], VA[JsValue]] = {
    ReaderWriterState { (context, status) =>
      ((),
        AnyConstraintValidator.validate(obj, schema.constraints.any, context),
        AnyConstraintValidator.validate(obj, schema.constraints.any, context))
    }
  }
}

//  private def validatePropertiesCount(schema: SchemaObject, json: JsObject): ValidationStep[Unit] = {
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
//              ValidationError("minProperties violated",
//                Json.obj("minProperties" -> minProperties, "object" -> obj)
//              )
//            )
//          )
//        }
//      case _ => Failure(List(ValidationError("object expected")))
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
//              ValidationError("maxProperties violated",
//                Json.obj("maxProperties" -> maxProperties, "object" -> obj)
//              )
//            )
//          )
//        }
//      case _ => Failure(List(ValidationError("object expected")))
//    }
//
//  }
//}
