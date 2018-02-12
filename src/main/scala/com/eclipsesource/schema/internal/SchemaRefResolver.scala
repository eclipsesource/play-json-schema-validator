package com.eclipsesource.schema.internal

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.refs._
import com.eclipsesource.schema.internal.validators.DefaultFormats
import com.osinka.i18n.{Lang, Messages}
import play.api.libs.json.{JsArray, JsString, JsonValidationError}

import scala.util.Try

object SchemaRefResolver {

  implicit val schemaRefInstance = new CanHaveRef[SchemaType] {

    // TODO make inner method
    private def resolveConstraint(schema: SchemaType, constraint: String)
                                 (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
      schema.constraints.resolvePath(constraint).fold[Either[JsonValidationError, SchemaType]](
        Left(JsonValidationError(Messages("err.unresolved.ref", constraint)))
      )(schema => Right(schema))
    }

    private def findAttribute(maybeObj: Option[SchemaObject], prop: String): Option[SchemaType] =
      maybeObj.flatMap(_.properties.collectFirst { case attr if attr.name == prop => attr.schemaType})

    private def findSchemaAttribute(props: Seq[SchemaAttribute], propName: String)
                                   (implicit lang: Lang): Either[JsonValidationError, SchemaType] = {
      props.collectFirst {
        case SchemaAttribute(name, s) if name == propName => s
      }.toRight(JsonValidationError(Messages("err.prop.not.found", propName)))
    }

    override def resolve(schema: SchemaType, fragmentPart: String)
                        (implicit lang: Lang = Lang.Default): Either[JsonValidationError, SchemaType] = {

      schema match {

        case SchemaMap(name, members) =>
            members.find(_.name == fragmentPart).map(_.schemaType).toRight(JsonValidationError(Messages(s"err.$name.not.found")))

        case obj@SchemaObject(props, _, remainingProps) => fragmentPart match {
          case Keywords.Object.Properties => Right(obj)
          case _ =>
            resolveConstraint(obj, fragmentPart) orElse
            findSchemaAttribute(props, fragmentPart) orElse
              findSchemaAttribute(remainingProps, fragmentPart)
        }

        case arr@SchemaArray(items, _, maybeObject) => fragmentPart match {
          case Keywords.Array.Items => Right(items)
          case other =>
            findAttribute(maybeObject, other)
              .map(Right(_))
              .getOrElse(resolveConstraint(arr, fragmentPart))
        }

        case tuple@SchemaTuple(items, _, _) =>

          def isValidIndex(idx: String) = {
            Try {
              val n = idx.toInt
              n <= items.size && n >= 0
            }.toOption.getOrElse(false)
          }

          fragmentPart match {
            case Keywords.Array.Items => Right(tuple)
            case idx if isValidIndex(idx) => Right(items(idx.toInt))
            case _ => resolveConstraint(tuple, fragmentPart)
          }

        case SchemaValue(value) => (value, fragmentPart) match {
          case (arr: JsArray, index) if Try {
            index.toInt
          }.isSuccess =>
            val idx = index.toInt
            if (idx > 0 && idx < arr.value.size) {
              Right(SchemaValue(arr.value(idx)))
            } else {
              Left(JsonValidationError(Messages("arr.out.of.bounds", index)))
            }
          case _ => Left(JsonValidationError(Messages("arr.invalid.index", fragmentPart)))
        }

        case p: PrimitiveSchemaType => resolveConstraint(p, fragmentPart)
      }
    }

    override def findRef(schema: SchemaType): Option[Ref] = schema match {
      case SchemaObject(props, _, _) =>
        props.collectFirst {
          case SchemaAttribute("$ref", SchemaValue(JsString(ref))) => Ref(ref)
        }
      case _ => None
    }

    override def findScopeRefinement(schema: SchemaType): Option[Ref] =
      schema.constraints.any.id.map(Ref(_))
  }

  case class SchemaResolutionContext(refResolver: SchemaRefResolver,
                                     scope: SchemaResolutionScope,
                                     formats: Map[String, SchemaFormat] = DefaultFormats.formats) extends GenResolutionContext[SchemaType] {

    def updateScope(scopeUpdateFn: SchemaResolutionScope => SchemaResolutionScope): SchemaResolutionContext =
      copy(scope = scopeUpdateFn(scope))

  }
  type SchemaResolutionScope = GenResolutionScope[SchemaType]
  type SchemaRefResolver = GenRefResolver[SchemaType]
}
