package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.draft4.constraints.ObjectConstraints4
import com.eclipsesource.schema.internal.validation.VA
import com.osinka.i18n.Lang
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

trait SchemaReads {
  self: SchemaVersion =>

  implicit val schemaReads: Reads[SchemaType] =
    schemaReadsSeq.reduceLeft(_ or _) orElse
      Reads.apply(json => JsError(s"Invalid JSON schema. Could not read\n${Json.prettyPrint(json)}\n"))

  def schemaReadsSeq: Seq[Reads[SchemaType]]
  def anyConstraintReads: Reads[AnyConstraints]
  def stringReads: Reads[SchemaString]
  def numberReads: Reads[SchemaNumber]
  def integerReads: Reads[SchemaInteger]
  def tupleReads: Reads[SchemaTuple]
  def arrayReads: Reads[SchemaArray]
  def objectReads: Reads[SchemaObject]

  def arrayKeywords: Set[String]
  def objectKeywords: Set[String]

  val withSchemaValueReader: Reads[SchemaType] = schemaReads or jsValueReader.map(asSchemaType)

  lazy val jsValueReader: Reads[SchemaValue] = {
    case bool@JsBoolean(_) => JsSuccess(SchemaValue(bool))
    case s@JsString(_) => JsSuccess(SchemaValue(s))
    case a@JsArray(_) => JsSuccess(SchemaValue(a))
    case other => JsError(s"Expected either Json boolean, string or array, got $other.")
  }

  lazy val nullReader: Reads[SchemaNull] =
    anyConstraintReads.flatMap(any => Reads.pure(SchemaNull(any)))

  lazy val booleanReader: Reads[SchemaBoolean] =
    anyConstraintReads.flatMap(any => Reads.pure(SchemaBoolean(any)))

  lazy val compoundReader: Reads[CompoundSchemaType] = {
    case obj@JsObject(fields) =>
      obj \ "type" match {
        case JsDefined(JsArray(values)) =>
          val jsResults: Seq[JsResult[SchemaType]] = values.map(value =>
            schemaReads.reads(JsObject(List("type" -> value) ++ fields.filterNot(_._1 == "type")))
          )
          val successes = jsResults.collect { case JsSuccess(success, _) => success }
          JsSuccess(CompoundSchemaType(successes))
        case _ => JsError("Expected Json array while reading compound type.")
      }
    case _ => JsError("Expected Json object while reading compound type.")
  }

  def createDelegateReader[A <: SchemaType with HasProps[A]](delegateReads: Reads[A], keywords: Set[String]): Reads[A] = {
    case json@JsObject(props) =>
      delegateReads.reads(json).map(schema => {
        addRemainingProps(schema, props.toList, keywords)
      })
    case err => JsError(s"Expected Json object during read, got $err")
  }

  private def addRemainingProps[A <: HasProps[A]](init: A, props: Iterable[(String, JsValue)], keywords: Set[String]): A = {
    val remainingProps = props.filterNot { case (propName, _) => keywords.contains(propName) }
    val remaining: Iterable[(String, SchemaType)] = remainingProps.map { case (name, value) =>
      name -> schemaReads.reads(value).asOpt.fold[SchemaType](SchemaValue(value))(x => x)
    }
    init.withProps(remaining.toSeq)
  }

  lazy val typeReader: Reads[SchemaType] = {
    (__ \ "type").read[String].flatMap {
      case "boolean" => booleanReader.map(asSchemaType)
      case "string" => stringReads.map(asSchemaType)
      case "integer" => integerReads.map(asSchemaType)
      case "number" => numberReads.map(asSchemaType)
      case "array" => delegatingArrayReader.map(asSchemaType) orElse delegatingTupleReader.map(asSchemaType)
      case "object" => delegatingObjectReader.map(asSchemaType)
      case "null" => nullReader.map(asSchemaType)
      case other => Reads.apply(_ => JsError(s"Invalid JSON schema. Unknown $other type.")).map(asSchemaType)
    }
  }

  lazy val delegatingTupleReader: Reads[SchemaTuple] = createDelegateReader(tupleReads, arrayKeywords)
  lazy val delegatingArrayReader: Reads[SchemaArray] = createDelegateReader(arrayReads, arrayKeywords)
  lazy val delegatingObjectReader: Reads[SchemaObject] = createDelegateReader(objectReads, objectKeywords)

  lazy val refReader: Reads[SchemaType] = {
    (
      (__ \ Keywords.Ref).readNullable[String] and
        anyConstraintReads
      ).tupled.flatMap { case (ref, anyConstraints) =>
      ref.fold[Reads[SchemaType]](
        Reads.apply(_ => JsError("No ref found"))
      )(r => {
        // TODO: should be Reads.pure(SchemaRef(r, anyConstraints)        )
        // TODO constraints make no sense for refs
        Reads.pure(
          SchemaObject(
            Seq(SchemaProp("$ref", SchemaValue(JsString(r)))),
            new ObjectConstraints {
              override def any: AnyConstraints = anyConstraints
              override def subSchemas: Set[SchemaType] = anyConstraints.subSchemas
              override def resolvePath(path: String): Option[SchemaType] = any.resolvePath(path)
              override def validate(schemaType: SchemaType, json: JsValue, context: SchemaResolutionContext)(implicit lang: Lang): VA[JsValue] =
                any.validate(schemaType, json, context)
            }
          )
        )
      }
      )
    }
  }

  def readJsNull(path: JsPath): Reads[Option[JsValue]] =
    Reads[Option[JsValue]] { json =>
      path.applyTillLast(json).fold(identity, _.fold(
        // const not found
        _ => JsSuccess(None),
        js => JsSuccess(Some(js))
      ))
    }

  def optional(keyword: Option[String]): Reads[Option[SchemaType]] =
    keyword.fold[Reads[Option[SchemaType]]](Reads.pure(None))(_if => (__ \ _if).lazyReadNullable(schemaReads))

  val schemaTypeMapReader: Reads[Map[String, SchemaType]] =
    (json: JsValue) => json.validate[Map[String, SchemaType]]

  val mapReadsInstanceWithJsValueReader: Reads[Map[String, SchemaType]] =
    (json: JsValue) => json.validate[Map[String, SchemaType]](mapReads(withSchemaValueReader))

  /**
    * Read a JsArray of JsObjects as a Seq of SchemaType.
    */
  def schemaTypeSeqReader(check: ElementCheck = (_ => true, None)): Reads[Seq[SchemaType]] = {
    case JsArray(els) if !els.exists(check._1) =>
      JsError(check._2.getOrElse("Error while reading JsArray."))
    case JsArray(els) =>
      // ignore all non-objects
      val results: Seq[JsResult[SchemaType]] = els.filter(check._1).map(Json.fromJson[SchemaType])
      if (results.exists(_.isError)) mergeErrors(results)
      else JsSuccess(results.collect { case JsSuccess(s, _) => s })
    case other => JsError(s"Expected array of Json objects, got $other.")
  }

  private def mergeErrors(results: Seq[JsResult[SchemaType]]): JsResult[Seq[SchemaType]] =
    results.collect {
      case err@JsError(_) => err
    }.reduceLeft[JsError] {
      case (e1, e2) => JsError.merge(e1, e2)
    }

  def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints4())

  def tuples2Attributes(props: Iterable[(String, SchemaType)]): List[SchemaProp] =
    props.map { case (name, schema) => SchemaProp(name, schema) }.toList

  type ElementCheck = ((JsValue) => Boolean, Option[ErrorMessage])
  type ErrorMessage = String

  def anyJsValue: ElementCheck = ((_: JsValue) => true, None)

  def asSchemaType[A <: SchemaType](s: A): SchemaType = s
}
