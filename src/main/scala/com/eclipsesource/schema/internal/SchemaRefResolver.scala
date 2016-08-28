package com.eclipsesource.schema.internal


import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.refs._
import com.eclipsesource.schema.internal.validators.DefaultFormats
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsArray, JsString}

import scala.util.Try

object SchemaRefResolver {

  implicit val schemaRefInstance = new CanHaveRef[SchemaType] {

    // TODO make inner method
    private def resolveConstraint(schema: SchemaType, constraint: String): Either[ValidationError, SchemaType] = {
      schema.constraints.resolvePath(constraint).fold[Either[ValidationError, SchemaType]](
        Left(ValidationError(s"Could not resolve $constraint"))
      )(schema => Right(schema))
    }

    private def findAttribute(maybeObj: Option[SchemaObject], prop: String): Option[SchemaType] =
      maybeObj.flatMap(_.properties.collectFirst { case attr if attr.name == prop => attr.schemaType})

    private def findSchemaAttribute(props: Seq[SchemaAttribute], propName: String): Either[ValidationError, SchemaType] = {
      props.collectFirst {
        case SchemaAttribute(name, s) if name == propName => s
      }.toRight(ValidationError(s"Could not find property $propName"))
    }

    override def resolve(schema: SchemaType, fragment: String): Either[ValidationError, SchemaType] = {

      schema match {

        case obj@SchemaObject(props, _, remainingProps) => fragment match {
          case Keywords.Object.Properties => Right(obj)
          case _ =>
            resolveConstraint(obj, fragment) orElse
            findSchemaAttribute(props, fragment) orElse
              findSchemaAttribute(remainingProps, fragment)
        }

        case arr@SchemaArray(items, _, maybeObject) => fragment match {
          case Keywords.Array.Items => Right(items)
          case other =>
            findAttribute(maybeObject, other)
              .map(Right(_))
              .getOrElse(resolveConstraint(arr, fragment))
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
    }

    override def findRef(schema: SchemaType): Option[(String, String)] = schema match {
      case SchemaObject(props, _, _) =>
        props.collectFirst {
          case SchemaAttribute("$ref", SchemaValue(JsString(pointer))) => "$ref" -> pointer
        }
      case _ => None
    }

    override def findScopeRefinement(schema: SchemaType): Option[Pointer] =
      schema.constraints.any.id.map(Pointer)

    override def anchors(a: SchemaType): Map[Pointer, SchemaType] = a.constraints.any.anchors
  }

  case class SchemaResolutionContext(refResolver: SchemaRefResolver,
                                     scope: SchemaResolutionScope,
                                     formats: Map[String, SchemaStringFormat] = DefaultFormats.formats) extends GenResolutionContext[SchemaType] {
    def updateScope(scopeUpdateFn: SchemaResolutionScope => SchemaResolutionScope): SchemaResolutionContext =
      copy(scope = scopeUpdateFn(scope))
    def updateResolutionScope(schema: SchemaType) = copy(scope = refResolver.updateResolutionScope(scope, schema))
  }
  type SchemaResolutionScope = GenResolutionScope[SchemaType]
  type SchemaRefResolver = GenRefResolver[SchemaType]
}
