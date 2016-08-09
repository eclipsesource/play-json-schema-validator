package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.immutable.Iterable

trait JSONSchemaReads {

  private def asSchemaType[A <: SchemaType](s: A): SchemaType = s

  implicit val valueReader: Reads[SchemaType] = (__ \ "type").read[String].flatMap {
    case "boolean" => booleanReader.map(asSchemaType)
    case "string"  => stringReader.map(asSchemaType)
    case "integer" => integerReader.map(asSchemaType)
    case "number"  => numberReader.map(asSchemaType)
    case "array"   => delegatingArrayReader.map(asSchemaType) orElse delegatingTupleReader.map(asSchemaType)
    case "object"  => delegatingObjectReader.map(asSchemaType)
    case "null"    => nullReader.map(s => s : SchemaType)
  }.or {
    tupleReader.map(asSchemaType)
  }.or {
    arrayReader.map(asSchemaType)
  }.or {
    stringReader.map(asSchemaType)
  }.or {
    numberReader.map(asSchemaType)
  }.or {
    delegatingObjectReader.map(asSchemaType)
  }.or {
    compoundReader.map(asSchemaType)
  }.orElse {
    Reads.apply(_ => JsError("Invalid JSON schema"))
  }

  val withSchemaValueReader = valueReader.or {
    jsValueReader.map(asSchemaType)
  }

  lazy val numberReader: Reads[SchemaNumber] = {
    ((__ \ Keywords.Number.Min).readNullable[BigDecimal] and
      (__ \ Keywords.Number.Max).readNullable[BigDecimal] and
      (__ \ Keywords.Number.ExclusiveMin).readNullable[Boolean] and
      (__ \ Keywords.Number.ExclusiveMax).readNullable[Boolean] and
      (__ \ Keywords.Number.MultipleOf).readNullable[BigDecimal] and
      anyConstraintReader
      ).tupled.flatMap { read =>

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, anyConstraints) = read
      val minimum = min.map(Minimum(_, exclusiveMin))
      val maximum = max.map(Maximum(_, exclusiveMax))
      val typeAsString = anyConstraints.schemaTypeAsString

      if (typeAsString.exists(_ != "number")
        || (typeAsString.isEmpty
        && min.isEmpty
        && max.isEmpty
        && exclusiveMin.isEmpty
        && exclusiveMax.isEmpty
        && multipleOf.isEmpty)) {
        Reads.apply(_ => JsError("Expected number"))
      } else {
        Reads.pure(SchemaNumber(NumberConstraints(minimum, maximum, multipleOf, anyConstraints)))
      }
    }
  }

  lazy val integerReader: Reads[SchemaInteger] = {
    ((__ \ Keywords.Number.Min).readNullable[BigDecimal] and
      (__ \ Keywords.Number.Max).readNullable[BigDecimal] and
      (__ \ Keywords.Number.ExclusiveMin).readNullable[Boolean] and
      (__ \ Keywords.Number.ExclusiveMax).readNullable[Boolean] and
      (__ \ Keywords.Number.MultipleOf).readNullable[BigDecimal] and
      anyConstraintReader
      ).tupled.flatMap(read => {

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, anyConstraints) = read
      val minimum = min.map(Minimum(_, exclusiveMin))
      val maximum = max.map(Maximum(_, exclusiveMax))
      val typeAsString = anyConstraints.schemaTypeAsString

      if (typeAsString.exists(_ != "integer")
        || (typeAsString.isEmpty
        && min.isEmpty
        && max.isEmpty
        && exclusiveMin.isEmpty
        && exclusiveMax.isEmpty
        && multipleOf.isEmpty)) {
        Reads.apply(_ => JsError("Expected integer."))
      } else {
        Reads.pure(SchemaInteger(NumberConstraints(minimum, maximum, multipleOf, anyConstraints)))
      }
    })
  }

  lazy val stringReader: Reads[SchemaString] = {
    ((__ \ Keywords.String.MinLength).readNullable[Int] and
      (__ \ Keywords.String.MaxLength).readNullable[Int] and
      (__ \ Keywords.String.Pattern).readNullable[String] and
      (__ \ Keywords.String.Format).readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap(read => {

      val (minLength, maxLength, pattern, format, anyConstraints) = read

      if (anyConstraints.schemaTypeAsString.exists(_ != "string") ||
        (anyConstraints.schemaTypeAsString.isEmpty && List(minLength, maxLength, format, pattern).forall(_.isEmpty))) {
        Reads.apply(_ => JsError("Expected string."))
      } else {
        Reads.pure(SchemaString(StringConstraints(minLength, maxLength, pattern, format, anyConstraints)))
      }
    })
  }

  lazy val jsValueReader: Reads[SchemaValue] = new Reads[SchemaValue] {
    override def reads(json: JsValue): JsResult[SchemaValue] = json match {
      case bool@JsBoolean(_) => JsSuccess(SchemaValue(bool))
      case s@JsString(_) => JsSuccess(SchemaValue(s))
      case a@JsArray(els) => JsSuccess(SchemaValue(a))
      case other => JsError(s"Expected either Json boolean, string or array, got $other.")
    }
  }

  lazy val nullReader: Reads[SchemaNull] =
    anyConstraintReader.flatMap(any => {
      // TODO: null constraint type could be removed
      Reads.pure(SchemaNull(NoConstraints(any)))
    })

  lazy val booleanReader: Reads[SchemaBoolean] =
    anyConstraintReader.flatMap(any => {
      // TODO: boolean constraint type could be removed
      Reads.pure(SchemaBoolean(NoConstraints(any)))
    })

  lazy val compoundReader: Reads[CompoundSchemaType] = new Reads[CompoundSchemaType] {
    override def reads(json: JsValue): JsResult[CompoundSchemaType] = json match {
      case obj@JsObject(fields) =>
        obj \ "type" match {
          case JsDefined(JsArray(values)) =>
            val jsResults: Seq[JsResult[SchemaType]] = values.map(value =>
              valueReader.reads(JsObject(List("type" -> value) ++ fields.filterNot(_._1 == "type")))
            )
            val successes = jsResults.collect { case JsSuccess(success, _) => success }
            JsSuccess(CompoundSchemaType(successes))
          case _ => JsError("Expected Json array while reading compound type.")
        }
      case _ => JsError("Expected Json object while reading compound type.")
    }
  }

  private def createDelegateReader[A <: SchemaType with HasProps[A]](delegateReads: Reads[A], keywords: List[String]): Reads[A] = {
    new Reads[A] {
      override def reads(json: JsValue): JsResult[A] = {
        json match {
          case JsObject(props) =>
            delegateReads.reads(json).map(schemaObject => {
              addRemainingProps(schemaObject, props.toList, keywords)
            })
          case err => JsError(s"Expected Json object during read, got $err")
        }
      }
    }
  }

  lazy val delegatingTupleReader: Reads[SchemaTuple] = createDelegateReader(tupleReader, Keywords.ofTuple)
  lazy val delegatingArrayReader: Reads[SchemaArray] = createDelegateReader(arrayReader, Keywords.ofArray)
  lazy val delegatingObjectReader: Reads[SchemaObject] = createDelegateReader(objectReader, Keywords.ofObject)

  private def addRemainingProps[A <: HasProps[A]](init: A, props: Iterable[(String, JsValue)], keywords: List[String]): A = {
    val remainingProps = props.filterNot { case (propName, _)  => keywords.contains(propName) }
    val objWithProps = remainingProps.foldLeft(SchemaObject()) { (obj, prop) =>
      obj ++ valueReader.reads(prop._2).asOpt.fold[SchemaObject](SchemaObject())(value =>
        SchemaObject(Seq(SchemaAttribute(prop._1, value)))
      )
    }
    init.withProps(objWithProps)
  }

  lazy val arrayReader: Reads[SchemaArray] = {
    ((__ \ Keywords.Array.Items).lazyReadNullable[SchemaType](valueReader) and
      (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](withSchemaValueReader) and
      (__ \ Keywords.Array.MinItems).readNullable[Int] and
      (__ \ Keywords.Array.MaxItems).readNullable[Int] and
      (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
      (__ \ Keywords.Id).readNullable[String] and
      anyConstraintReader
      ).tupled.flatMap(
        read => {
          val (items, additionalItems, minItems, maxItems, uniqueItems, id, any) = read

          if (any.schemaTypeAsString.exists(_ != "array") ||
            (any.schemaTypeAsString.isEmpty
              && items.isEmpty
              && additionalItems.isEmpty
              && minItems.isEmpty
              && maxItems.isEmpty
              && uniqueItems.isEmpty)) {
            Reads.apply(_ => JsError("Expected array."))
          } else {
            Reads.pure(
              SchemaArray(
                items.getOrElse(emptyObject),
                ArrayConstraints(maxItems, minItems, additionalItems, uniqueItems, any), id)
            )
          }
        })
  }

  lazy val tupleReader: Reads[SchemaTuple] = {
    ((__ \ Keywords.Array.Items).lazyReadNullable[Seq[SchemaType]](readJsValueArray) and
        (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](withSchemaValueReader) and
        (__ \ Keywords.Array.MinItems).readNullable[Int] and
        (__ \ Keywords.Array.MaxItems).readNullable[Int] and
        (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
        (__ \ Keywords.Id).readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap { read =>
      val (items, additionalItems, minItems, maxItems, uniqueItems, id, any) = read

      if (any.schemaTypeAsString.exists(_ != "array") ||
        ((any.schemaTypeAsString.isEmpty
          && items.isEmpty
          && additionalItems.isEmpty
          && minItems.isEmpty
          && maxItems.isEmpty) || items.isEmpty)) {
        Reads.apply(_ => JsError("Expected tuple."))
      } else {
        Reads.pure(
          SchemaTuple(items.getOrElse(Seq.empty),
            ArrayConstraints(maxItems, minItems, additionalItems, uniqueItems, any),
            id
          )
        )
      }
    }
  }

  def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints())

  // TODO: extract into objectConstraintReader
  lazy val objectReader: Reads[SchemaObject] = {
    ((__ \ Keywords.Object.Properties).lazyReadNullable[Map[String, SchemaType]](mapReadsInstance) and
        (__ \ Keywords.Object.PatternProperties).lazyReadNullable[Map[String, SchemaType]](mapReadsInstance) and
        (__ \ Keywords.Object.AdditionalProperties).lazyReadNullable[SchemaType](withSchemaValueReader) and
        (__ \ Keywords.Object.Required).readNullable[List[String]] and
        (__ \ Keywords.Object.Dependencies).lazyReadNullable[Map[String, SchemaType]](mapReadsInstanceWithJsValueReader) and
        (__ \ Keywords.Object.MinProperties).readNullable[Int] and
        (__ \ Keywords.Object.MaxProperties).readNullable[Int] and
        (__ \ Keywords.Object.Ref).readNullable[String] and
        (__ \ Keywords.Id).readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap { read =>

      val (properties, patternProperties, additionalProperties, required, dependencies, minProperties, maxProperties, ref, id, anyConstraints) = read
      val props: List[SchemaAttribute] = properties.map(tuples2Attributes).getOrElse(List.empty)

      Reads.pure(
        SchemaObject(
          props ++ ref.map(path => Seq(SchemaAttribute("$ref", SchemaValue(JsString(path))))).getOrElse(Seq.empty),
          ObjectConstraints(
            additionalProperties,
            dependencies,
            patternProperties,
            required,
            minProperties,
            maxProperties,
            anyConstraints
          ),
          id
        )
      )
    }
  }

  lazy val anyConstraintReader: Reads[AnyConstraint] = {
    ((__ \ Keywords.Any.Type).readNullable[String] and
      (__ \ Keywords.Any.AllOf).lazyReadNullable[Seq[SchemaType]](readJsObjectArray) and
        (__ \ Keywords.Any.AnyOf).lazyReadNullable[Seq[SchemaType]](readJsObjectArray) and
        (__ \ Keywords.Any.OneOf).lazyReadNullable[Seq[SchemaType]](readJsObjectArray) and
        (__ \ Keywords.Any.Definitions).lazyReadNullable(mapReadsInstance) and
        (__ \ Keywords.Any.Enum).readNullable[Seq[JsValue]] and
        (__ \ Keywords.Any.Not).lazyReadNullable(valueReader)
      ).tupled.map(
        read => {
          val (schemaType, allOf, anyOf, oneOf, definitions, enum, not) = read
          AnyConstraint(schemaType, allOf, anyOf, oneOf, definitions, enum, not)
        }
      )
  }

  private val mapReadsInstance: Reads[Map[String, SchemaType]] =
    new Reads[Map[String, SchemaType]] {
      def reads(json: JsValue) = json.validate[Map[String, SchemaType]]
    }

  private val mapReadsInstanceWithJsValueReader: Reads[Map[String, SchemaType]] =
    new Reads[Map[String, SchemaType]] {
      def reads(json: JsValue) = json.validate[Map[String, SchemaType]](mapReads(withSchemaValueReader))
    }

  private def mergeErrors(results: Seq[JsResult[SchemaType]]): JsResult[Seq[SchemaType]] =
    results.collect {
      case err@JsError(_) => err
    }.reduceLeft[JsError] {
      case (e1, e2) => JsError.merge(e1, e2)
    }

  private lazy val readJsValueArray: Reads[Seq[SchemaType]] = {
    new Reads[Seq[SchemaType]] {
      override def reads(json: JsValue): JsResult[Seq[SchemaType]] = json match {
        case JsArray(els) =>
          val results = els.map(Json.fromJson[SchemaType](_))
          if (results.exists(_.isError)) {
            mergeErrors(results)
          } else {
            JsSuccess(results.collect { case JsSuccess(succ, _) => succ})
          }
        case other => JsError(s"Expected Json array, got $other.")
      }
    }
  }

  private lazy val readJsObjectArray: Reads[Seq[SchemaType]] = {
    new Reads[Seq[SchemaType]] {
      override def reads(json: JsValue): JsResult[Seq[SchemaType]] = json match {
        case JsArray(els) if !els.exists(_.isInstanceOf[JsObject]) =>
          JsError("Non-object encountered in object-only array.")
        case JsArray(els) =>
          val results = els.map(Json.fromJson[SchemaType](_))
          if (results.exists(_.isError)) {
            mergeErrors(results)
          } else {
            JsSuccess(results.collect { case JsSuccess(succ, _) => succ})
          }
        case other => JsError(s"Expected array of Json objects, got $other.")
      }
    }
  }

  private def tuples2Attributes(props: Iterable[(String, SchemaType)]): List[SchemaAttribute] =
    props.map { case (name, schema) => SchemaAttribute(name, schema) }.toList

}
