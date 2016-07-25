package com.eclipsesource.schema.internal.refs

import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.util.Try

object JsValueRefResolver {

  implicit val jsValueRefInstance = new CanHaveRef[JsValue] {
    def findScopeRefinement(json: JsValue) = None
    def findRef(json: JsValue) = None

    override def resolve(json: JsValue, fragment: String): Either[ValidationError, JsValue] = {
      def fragmentIsInt = Try(fragment.toInt).isSuccess
      json match {
        case obj: JsObject => (obj \ fragment).toEither
        case arr: JsArray if fragmentIsInt => (arr \ fragment.toInt).toEither
        case arr: JsArray => Left(ValidationError(s"Invalid array index $fragment"))
        case _ => Left(ValidationError(s"Fragment $fragment could not be resolved"))
      }
    }
  }

  type JsValueResolutionContext = GenResolutionContext[JsValue]
  type JsValueResolutionScope =  GenResolutionScope[JsValue]
  type JsValueRefResolver = GenRefResolver[JsValue]
  type Errors = ValidationError

  def resolve(path: String, root: JsValue): Either[Errors, JsValue] = {
    val resolver = new JsValueRefResolver
    resolver.resolve(path, new JsValueResolutionScope(root)).right.map(_.resolved)
  }


  private[schema] def normalizeRelativeRef(relativeRef: String, resolveFrom: String): Either[Errors, String] = {

    def longestContinuousSeq: Either[Errors, (Int, String)] = {
      val seq = relativeRef.takeWhile(_.isDigit)
      if (seq.isEmpty)
        Left(ValidationError("Invalid relative JSON Pointer"))
      else
        Right((seq.toInt, relativeRef.drop(seq.length)))
    }

    def normalize(idxWithRest: (Int, String)): Either[Errors, String] = idxWithRest._1 match {
      case n if n < 0 => Left (ValidationError ("Invalid relative JSON Pointer"))
      case n if idxWithRest._2 == "#" =>
        Right (resolveFrom.split ("/").dropRight(n).mkString ("/") + "#")
      case n if idxWithRest._2.endsWith("#")=>
        val splitted = resolveFrom.split("/")
        if (n + 1 >= splitted.size) {
          Left(ValidationError("ERROR"))
        } else {
          Right(splitted.dropRight(n + 1).mkString("/") + idxWithRest._2)
        }
      case n =>
        val splitted = resolveFrom.split("/")
        if (n >= splitted.size) {
          Left(ValidationError("ERROR"))
        } else {
          Right(splitted.dropRight(n).mkString("/") + idxWithRest._2)
        }
    }

    for {
      idxWithRest <- longestContinuousSeq.right
      normalized  <- normalize(idxWithRest).right
    } yield {
      normalized
    }
  }

  // TODO: what about error reporting, e.g. schema & instance path
  // TODO: root value could also be a non-object/array
  def resolveRelative(relativePointer: String, resolveFrom: String, obj: JsObject): Either[Errors, JsValue] = {
    normalizeRelativeRef(relativePointer, resolveFrom) match {
      case Left(errors) => Left(errors)
      case Right(ref) if ref == "#" =>
        resolve(ref, obj)
      case Right(ref) if ref == "##" =>
        Left(ValidationError("ERROR"))
      case Right(ref) if ref.endsWith("#") =>
        val last = ref.split("/").last
        Right(JsString(last.substring(0, last.length - 1)))
      case Right(ref) =>
        resolve(ref, obj)
    }
  }
}
