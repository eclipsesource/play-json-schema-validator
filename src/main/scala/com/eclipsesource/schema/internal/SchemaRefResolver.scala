package com.eclipsesource.schema.internal

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.refs.{GenResolutionScope, GenResolutionContext, CanHaveRef, GenRefResolver}
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.util.Try

object SchemaRefResolver {

  implicit val schemaRefInstance = new CanHaveRef[SchemaType] {

    // TODO make inner method
    private def resolveConstraint(schema: SchemaType, constraint: String): Either[ValidationError, SchemaType] = {
      schema.constraints.resolvePath(constraint).fold[Either[ValidationError, SchemaType]](
        Left(ValidationError(s"Could not resolve $constraint"))
      )(schema => Right(schema))
    }

    override def resolve(schema: SchemaType, fragment: String): Either[ValidationError, SchemaType] = schema match {
      case obj@SchemaObject(props, _, _) => fragment match {
        case Keywords.Object.Properties => Right(obj)
        case other => props.find(_.name == other).map(_.schemaType).fold(
          resolveConstraint(obj, fragment)
        )(Right(_))
      }

      case arr@SchemaArray(items, _, _) => fragment match {
        case Keywords.Array.Items => Right(items)
        case other => resolveConstraint(arr, fragment)
      }

      case tuple@SchemaTuple(items, _, _) =>

        def isValidIndex(idx: String) = {
          Try {
            val n = idx.toInt
            n <= items.size && n >= 0
          }.toOption.getOrElse(false)
        }

        fragment match {
          case Keywords.Array.Items => Right(tuple)
          case idx if isValidIndex(idx) => Right(items(idx.toInt))
          // TODO
          case other => resolveConstraint(tuple, fragment)
        }

      case schemaValue@SchemaValue(value) => (value, fragment) match {
        case (arr: JsArray, index) if Try {
          index.toInt
        }.isSuccess =>
          val idx = index.toInt
          if (idx > 0 && idx < arr.value.size) {
            Right(SchemaValue(arr.value(idx)))
          } else {
            Left(ValidationError(s"Array index $index out of bounds"))
          }
        case other => Left(ValidationError(s"Invalid array index $fragment"))
      }

      case p: PrimitiveSchemaType => resolveConstraint(p, fragment)
    }

    override def findRef(schema: SchemaType): Option[(String, String)] = schema match {
      case SchemaObject(props, _, _) =>
        props.find(prop => prop.name == "$ref" && (prop.schemaType match {
          case SchemaValue(JsString(pointer)) => true
          case _ => false
        }))
          // TODO
          .map(prop => prop.name -> prop.schemaType.asInstanceOf[SchemaValue].value.as[JsString].value)
      case _ => None
    }

    override def findScopeRefinement(schema: SchemaType): Option[String] = schema match {
      case SchemaObject(_, _, id) => id
      case SchemaArray(_, _, id) => id
      case _ => None
    }

    override def isResolvable(schema: SchemaType): Boolean = schema match {
      case _: SchemaObjectLike => true
      case _: SchemaArrayLike => true
      case _: SchemaValue => true
      case _: PrimitiveSchemaType => true
      case _ => false
    }
  }

  type SchemaResolutionContext = GenResolutionContext[SchemaType]
  type SchemaResolutionScope = GenResolutionScope[SchemaType]
  type SchemaRefResolver = GenRefResolver[SchemaType]
}
