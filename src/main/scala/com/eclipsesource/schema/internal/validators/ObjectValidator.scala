package com.eclipsesource.schema.internal.validators

import java.util.regex.Pattern
import com.eclipsesource.schema._
import com.eclipsesource.schema.internal._
import play.api.data.mapping._
import play.api.libs.json._
import scalaz.ReaderWriterState

object ObjectValidator extends SchemaTypeValidator[SchemaObject] {

  override def validate(schema: SchemaObject, json: => JsValue, context: Context): VA[JsValue] = {

    def validateJson(schema: SchemaObject, c: Context): VA[JsValue] = {
      json match {
        case jsObject@JsObject(props) =>
          val validation = for {
            updatedSchema <- validateDependencies(schema, jsObject)
            remaining <- validateProps(updatedSchema, jsObject)
            unmatched <- validatePatternProps(updatedSchema, jsObject.fields)
            _ <- validateAdditionalProps(updatedSchema, unmatched.intersect(remaining))
            _ <- validateMinProperties(updatedSchema, jsObject)
            _ <- validateMaxProperties(updatedSchema, jsObject)
          } yield updatedSchema

          val (_, _, result) = validation.run(c, Success(json))
          result
        case _ =>
          val (_, _, result) = validationAny(schema, json).run(c, Success(json))
          result
      }
    }

    // check if any property is a ref
    val reference = schema.properties.collectFirst { case SchemaAttribute("$ref", ref@SchemaRef(_, _, _)) => ref }

    val validatedOpt: Option[VA[JsValue]] = for {
      ref <- reference
      resolved <- RefResolver.resolveRef(ref, context)
    } yield resolved match {
        case _ if context.visited.contains(ref) => Success(json)
        case obj: SchemaObject =>
          // TODO: isInstanceOf
          if (schema.isSubSetOf(obj) && !json.isInstanceOf[JsObject]) {
            Success(json)
          } else {
            val updatedContext = if (context.root == schema) {
              context.copy(root = obj, visited = context.visited + ref)
            } else {
              context.copy(visited = context.visited + ref)
            }
            validateJson(obj, updatedContext)
          }
        case other =>
          val updatedContext = if (context.root == schema) {
            context.copy(root = other)
          } else {
            context
          }
          SchemaValidator.process(other, json, updatedContext)

      }

    validatedOpt.getOrElse(validateJson(schema, context))
  }

  private def validateProps(schema: SchemaObject, obj: => JsObject): ValidationStep[Props] =
    ReaderWriterState { (context, status) =>

      val required = schema.constraints.required.getOrElse(List.empty[String])

      val validated = schema.properties.foldLeft(List.empty[(String, VA[JsValue])])((props, attr) =>
        obj \ attr.name match {
          case _: JsUndefined => if (required.contains(attr.name)) {
            (attr.name -> Results.failure(s"Property ${attr.name} missing")) :: props
          } else {
            props
          }
          case JsDefined(value) => (attr.name -> SchemaValidator.process(
            attr.schemaType,
            value,
            context.copy(
              path = context.path \ "properties" \ attr.name
            )
          )) :: props
        }
      )

      val validatedProperties = validated.map(_._1)
      val unvalidatedProps: Props = obj.fields.filterNot(field =>
        validatedProperties.contains(field._1)
      )

      ((), unvalidatedProps, Results.merge(status, Results.aggregateAsObject(validated, context)))
    }


  private def validatePatternProps(schema: SchemaObject, props: Props): ValidationStep[Props] =
    ReaderWriterState { (context, status) =>

      // find all matching properties and validate them
      val validated: Seq[(String, VA[JsValue])] = props.flatMap {
        prop => {
          val matchedPatternProperties = schema.constraints.patternProps.getOrElse(Seq.empty).filter(pp => {
            val pattern = Pattern.compile(pp._1)
            val matcher = pattern.matcher(prop._1)
            matcher.find()
          })
          matchedPatternProperties.map(pp =>
            prop._1 -> SchemaValidator.process(pp._2, prop._2, context)
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
        attr._1 -> SchemaValidator.process(
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
              ((), (), Results.merge(status, Results.failure(context.path, s"Additional properties are not allowed, " +
                s"but found ${unmatchedFields.map(f => s"'${f._1}'").mkString(", ")}.")))
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
        case extension@SchemaObject(_, _, _) => baseSchema ++ extension
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
          // collecting strings should not be necessary at this point
          val validated = validatePropertyDependency(name, arr.seq.collect { case JsString(str) => str}, context)
          (acc._1, Results.merge(acc._2, validated))
        case (name, cls: SchemaObject) if obj.keys.contains(name) => (extendSchemaByDependency(acc._1, dep._2), acc._2)
        case _ => acc
      })

      ((), updatedSchema, updatedStatus)
    }
  }

  def validationAny(schema: SchemaObject, obj: JsValue): ReaderWriterState[Context, Unit, VA[JsValue], VA[JsValue]] = {
    ReaderWriterState { (context, status) =>
      val result = AnyConstraintValidator.validate(obj, schema.constraints.any, context)
      ((), result, result)
    }
  }

  def validateMaxProperties(schema: SchemaObject, obj: JsObject): ReaderWriterState[Context, Unit, VA[JsValue], Unit] = {
    ReaderWriterState { (context, status) =>
      val size = obj.fields.size
      val result: VA[JsValue] = schema.constraints.maxProperties match {
        case None => Success(obj)
        case Some(max) => if (size <= max) {
          Success(obj)
        } else {
          Results.failure(s"Found $size properties, but only a maximum of $max properties is allowed.")
        }
      }
      ((), (), Results.merge(status, result))
    }
  }

  def validateMinProperties(schema: SchemaObject, obj: JsObject): ReaderWriterState[Context, Unit, VA[JsValue], Unit] = {
    ReaderWriterState { (context, status) =>
      val size = obj.fields.size
      val result: VA[JsValue] = schema.constraints.minProperties match {
        case None => Success(obj)
        case Some(min) => if (size >= min) {
          Success(obj)
        } else {
          Results.failure(s"Found $size properties, but at least $min ${if (min == 1) "property needs" else "properties need"} to be present.")
        }
      }
      ((), (), Results.merge(status, result))
    }
  }
}

