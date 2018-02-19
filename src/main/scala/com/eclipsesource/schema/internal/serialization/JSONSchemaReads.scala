package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.refs.{Ref, Refs}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

trait JSONSchemaReads { self: SchemaVersion =>

  private def asSchemaType[A <: SchemaType](s: A): SchemaType = s

  implicit val valueReader: Reads[SchemaType] = {
    refReader
  }.or {
    typeReader
  }.or {
    booleanSchemaReader.map(asSchemaType)
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
    Reads.apply(json => JsError(s"Invalid JSON schema. Could not read\n${Json.prettyPrint(json)}\n"))
  }

  val withSchemaValueReader: Reads[SchemaType] = valueReader or jsValueReader.map(asSchemaType)

  lazy val booleanSchemaReader: Reads[SchemaValue] = self match {
    case Version4 => Reads(_ => JsError("No boolean schema in draft version 4"))
    case Version7 => new Reads[SchemaValue] {
      override def reads(json: JsValue): JsResult[SchemaValue] = json match {
        case b@JsBoolean(_) => JsSuccess(SchemaValue(b))
        case _ => JsError("Expected JsBoolean.")
      }
    }
  }

  lazy val numberReader: Reads[SchemaNumber] = self match {
    case Version4 => numberReader4
    case Version7 => numberReader7
  }

  // TODO: code duplication
  lazy val numberReader4: Reads[SchemaNumber] = {
    (
      (__ \ Keywords.Number.Min).readNullable[BigDecimal] and
        (__ \ Keywords.Number.Max).readNullable[BigDecimal] and
        (__ \ Keywords.Number.ExclusiveMin).readNullable[Boolean] and
        (__ \ Keywords.Number.ExclusiveMax).readNullable[Boolean] and
        (__ \ Keywords.Number.MultipleOf).readNullable[BigDecimal] and
        (__ \ Keywords.String.Format).readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap { read =>

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, format, anyConstraints) = read
      val minimum = min.map(Minimum(_, Some(exclusiveMin.getOrElse(false))))
      val maximum = max.map(Maximum(_, Some(exclusiveMax.getOrElse(false))))
      val typeAsString = anyConstraints.schemaTypeAsString

      if (typeAsString.exists(_ != "number")
        || (typeAsString.isEmpty
        && min.isEmpty
        && max.isEmpty
        && exclusiveMin.isEmpty
        && exclusiveMax.isEmpty
        && multipleOf.isEmpty)) {
        Reads(_ => JsError("Expected number"))
      } else {
        Reads.pure(SchemaNumber(NumberConstraints(minimum, maximum, multipleOf, format, anyConstraints)))
      }
    }
  }

  lazy val numberReader7: Reads[SchemaNumber] = {
    (
      (__ \ Keywords.Number.Min).readNullable[BigDecimal] and
      (__ \ Keywords.Number.Max).readNullable[BigDecimal] and
      (__ \ Keywords.Number.ExclusiveMin).readNullable[BigDecimal] and
      (__ \ Keywords.Number.ExclusiveMax).readNullable[BigDecimal] and
      (__ \ Keywords.Number.MultipleOf).readNullable[BigDecimal] and
      (__ \ Keywords.String.Format).readNullable[String] and
      anyConstraintReader
      ).tupled.flatMap { read =>

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, format, anyConstraints) = read
      val minimum = min.map(Minimum(_, Some(false))) orElse exclusiveMin.map(Minimum(_, Some(true)))
      val maximum = max.map(Maximum(_, Some(false))) orElse exclusiveMax.map(Maximum(_, Some(true)))
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
        Reads.pure(SchemaNumber(NumberConstraints(minimum, maximum, multipleOf, format, anyConstraints)))
      }
    }
  }

  lazy val integerReader: Reads[SchemaInteger] = {
    ((__ \ Keywords.Number.Min).readNullable[BigDecimal] and
      (__ \ Keywords.Number.Max).readNullable[BigDecimal] and
      (__ \ Keywords.Number.ExclusiveMin).readNullable[Boolean] and
      (__ \ Keywords.Number.ExclusiveMax).readNullable[Boolean] and
      (__ \ Keywords.Number.MultipleOf).readNullable[BigDecimal] and
      (__ \ Keywords.String.Format).readNullable[String] and
      anyConstraintReader
      ).tupled.flatMap(read => {

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, format, anyConstraints) = read
      val minimum = min.map(Minimum(_, exclusiveMin))
      val maximum = max.map(Maximum(_, exclusiveMax))
      val typeAsString = anyConstraints.schemaTypeAsString
      val schema = SchemaInteger(NumberConstraints(minimum, maximum, multipleOf, format, anyConstraints))

      if (typeAsString.exists(_ != "integer")
        || (typeAsString.isEmpty
        && min.isEmpty
        && max.isEmpty
        && exclusiveMin.isEmpty
        && exclusiveMax.isEmpty
        && multipleOf.isEmpty)) {
        Reads.apply(_ => JsError("Expected integer."))
      } else {
        Reads.pure(schema)
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
    anyConstraintReader.flatMap(any => Reads.pure(SchemaNull(NoConstraints(any))))

  lazy val booleanReader: Reads[SchemaBoolean] =
    anyConstraintReader.flatMap(any => Reads.pure(SchemaBoolean(NoConstraints(any))))

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

  private def createDelegateReader[A <: SchemaType with HasProps[A]](delegateReads: Reads[A], keywords: Set[String]): Reads[A] = {
    new Reads[A] {
      override def reads(json: JsValue): JsResult[A] = {
        json match {
          case JsObject(props) =>
            delegateReads.reads(json).map(schema => {
              addRemainingProps(schema, props.toList, keywords)
            })
          case err => JsError(s"Expected Json object during read, got $err")
        }
      }
    }
  }

  lazy val delegatingTupleReader: Reads[SchemaTuple] = createDelegateReader(tupleReader, keywords.ofTuple)
  lazy val delegatingArrayReader: Reads[SchemaArray] = createDelegateReader(arrayReader, keywords.ofArray)
  lazy val delegatingObjectReader: Reads[SchemaObject] = createDelegateReader(objectReader, keywords.ofObject)

  private def addRemainingProps[A <: HasProps[A]](init: A, props: Iterable[(String, JsValue)], keywords: Set[String]): A = {
    val remainingProps = props.filterNot { case (propName, _)  => keywords.contains(propName) }
    val remaining: Iterable[(String, SchemaType)] = remainingProps.map { case (name, value) =>
      name -> valueReader.reads(value).asOpt.fold[SchemaType](SchemaValue(value))(x => x)
    }
    init.withProps(remaining.toSeq)
  }

  lazy val arrayReader: Reads[SchemaArray] = {
    ((__ \ Keywords.Array.Items).lazyReadNullable[SchemaType](valueReader) and
      (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](withSchemaValueReader) and
      (__ \ Keywords.Array.MinItems).readNullable[Int] and
      (__ \ Keywords.Array.MaxItems).readNullable[Int] and
      (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
      optional(keywords.contains) and
      anyConstraintReader
      ).tupled.flatMap(
      read => {
        val (items, additionalItems, minItems, maxItems, uniqueItems, contains, any) = read

        if (any.schemaTypeAsString.exists(_ != "array") ||
          (any.schemaTypeAsString.isEmpty
            && items.isEmpty
            && additionalItems.isEmpty
            && minItems.isEmpty
            && maxItems.isEmpty
            && contains.isEmpty
            && uniqueItems.isEmpty)) {
          Reads.apply(_ => JsError("Expected array."))
        } else {
          Reads.pure(
            SchemaArray(
              items.getOrElse(emptyObject),
              ArrayConstraints(maxItems,
                minItems,
                additionalItems,
                uniqueItems,
                contains,
                any
              )
            )
          )
        }
      })
  }

  lazy val tupleReader: Reads[SchemaTuple] = {
    ((__ \ Keywords.Array.Items).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader(anyJsValue)) and
      (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](withSchemaValueReader) and
      (__ \ Keywords.Array.MinItems).readNullable[Int] and
      (__ \ Keywords.Array.MaxItems).readNullable[Int] and
      keywords.contains.fold[Reads[Option[SchemaType]]](Reads.pure(None))(contains => (__ \ contains).lazyReadNullable(valueReader)) and
      (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
      anyConstraintReader
      ).tupled.flatMap { read =>
      val (items, additionalItems, minItems, maxItems, uniqueItems, contains, anyConstraints) = read
      if (anyConstraints.schemaTypeAsString.exists(_ != "array") ||
        ((anyConstraints.schemaTypeAsString.isEmpty
          && items.isEmpty
          && additionalItems.isEmpty
          && minItems.isEmpty
          && maxItems.isEmpty
          && contains.isEmpty
          && uniqueItems.isEmpty) || items.isEmpty)) {
        Reads.apply(_ => JsError("Expected tuple."))
      } else {
        Reads.pure(
          SchemaTuple(items.getOrElse(Seq.empty),
            ArrayConstraints(maxItems,
              minItems,
              additionalItems,
              contains,
              uniqueItems,
              anyConstraints
            )
          )
        )
      }
    }
  }

  lazy val typeReader: Reads[SchemaType] = {
    (__ \ "type").read[String].flatMap {
      case "boolean" => booleanReader.map(asSchemaType)
      case "string" => stringReader.map(asSchemaType)
      case "integer" => integerReader.map(asSchemaType)
      case "number" => numberReader.map(asSchemaType)
      case "array" => delegatingArrayReader.map(asSchemaType) orElse delegatingTupleReader.map(asSchemaType)
      case "object" => delegatingObjectReader.map(asSchemaType)
      case "null" => nullReader.map(asSchemaType)
      case other => Reads.apply(_ => JsError(s"Invalid JSON schema. Unknown $other type.")).map(asSchemaType)
    }
  }

  lazy val refReader: Reads[SchemaType] = {
    (
      (__ \ Keywords.Ref).readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap { case (ref, anyConstraints) =>
      ref.fold[Reads[SchemaType]](
        Reads.apply(_ => JsError("No ref found"))
      )(r => {
        Reads.pure(
          SchemaObject(
            Seq(SchemaProp("$ref", SchemaValue(JsString(r)))),
            ObjectConstraints().copy(any = anyConstraints)
          )
        )
      }
      )
    }
  }

  // TODO: extract into objectConstraintReader
  lazy val objectReader: Reads[SchemaObject] = {
    (
      (__ \ Keywords.Object.Properties).lazyReadNullable[Map[String, SchemaType]](schemaTypeMapReader) and
        (__ \ Keywords.Object.PatternProperties).lazyReadNullable[Map[String, SchemaType]](schemaTypeMapReader) and
        (__ \ Keywords.Object.AdditionalProperties).lazyReadNullable[SchemaType](withSchemaValueReader) and
        (__ \ Keywords.Object.Required).readNullable[List[String]] and
        (__ \ Keywords.Object.Dependencies).lazyReadNullable[Map[String, SchemaType]](mapReadsInstanceWithJsValueReader) and
        (__ \ Keywords.Object.MinProperties).readNullable[Int] and
        (__ \ Keywords.Object.MaxProperties).readNullable[Int] and
        optional(keywords.propertyNames) and
        anyConstraintReader
      ).tupled.flatMap { read =>

      val (
        properties,
        patternProperties,
        additionalProperties,
        required,
        dependencies,
        minProperties,
        maxProperties,
        propertyNames,
        anyConstraints) = read
      val props: List[SchemaProp] = properties.map(tuples2Attributes).getOrElse(List.empty)

      val schema = SchemaObject(
        props,
        ObjectConstraints(
          additionalProperties,
          dependencies,
          patternProperties,
          required,
          minProperties,
          maxProperties,
          propertyNames,
          anyConstraints
        )
      )

      Reads.pure(schema)
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

  lazy val anyConstraintReader: Reads[AnyConstraint] = {

    (
      (__ \ Keywords.Any.Type).readNullable[String] and
        (__ \ Keywords.Any.AllOf).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader()) and
        (__ \ Keywords.Any.AnyOf).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader()) and
        (__ \ Keywords.Any.OneOf).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader()) and
        (__ \ Keywords.Any.Definitions).lazyReadNullable(schemaTypeMapReader) and
        (__ \ Keywords.Any.Enum).readNullable[Seq[JsValue]] and
        readJsNull(__ \ "const") and
        (__ \ Keywords.Any.Not).lazyReadNullable(valueReader) and
        (__ \ Keywords.Any.Description).readNullable[String] and
        (__ \ keywords.id).readNullable[String] and
        optional(keywords.If) and
        optional(keywords.Then) and
        optional(keywords.Else)
      ).tupled.map { read =>

      val (schemaType, allOf, anyOf, oneOf, definitions, enum, const, not, desc, id, _if, _then, _else) = read
      AnyConstraint(schemaType, allOf, anyOf, oneOf, definitions, enum, const, not, desc, id, _if, _then, _else)
    }
  }

  private def optional(keyword: Option[String]): Reads[Option[SchemaType]] =
    keyword.fold[Reads[Option[SchemaType]]](Reads.pure(None))(_if => (__ \ _if).lazyReadNullable(valueReader))

  private val schemaTypeMapReader: Reads[Map[String, SchemaType]] =
    new Reads[Map[String, SchemaType]] {
      def reads(json: JsValue) = json.validate[Map[String, SchemaType]]
    }

  private val mapReadsInstanceWithJsValueReader: Reads[Map[String, SchemaType]] =
    new Reads[Map[String, SchemaType]] {
      def reads(json: JsValue) = json.validate[Map[String, SchemaType]](mapReads(withSchemaValueReader))
    }

  /**
    * Read a JsArray of JsObjects as a Seq of SchemaType.
    */
  private def schemaTypeSeqReader(check: ElementCheck = (_ => true, None)): Reads[Seq[SchemaType]] = {
    new Reads[Seq[SchemaType]] {
      override def reads(json: JsValue) = json match {
        case JsArray(els) if !els.exists(check._1) =>
          JsError(check._2.getOrElse("Error while reading JsArray."))
        case JsArray(els) =>
          // ignore all non-objects
          val results: Seq[JsResult[SchemaType]] = els.filter(check._1).map(Json.fromJson[SchemaType](_))
          if (results.exists(_.isError)) mergeErrors(results)
          else JsSuccess(results.collect { case JsSuccess(s, _) => s })
        case other => JsError(s"Expected array of Json objects, got $other.")
      }
    }
  }

  private def mergeErrors(results: Seq[JsResult[SchemaType]]): JsResult[Seq[SchemaType]] =
    results.collect {
      case err@JsError(_) => err
    }.reduceLeft[JsError] {
      case (e1, e2) => JsError.merge(e1, e2)
    }

  private def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints())

  private def tuples2Attributes(props: Iterable[(String, SchemaType)]): List[SchemaProp] =
    props.map { case (name, schema) => SchemaProp(name, schema) }.toList

  type ElementCheck = ((JsValue) => Boolean, Option[ErrorMessage])
  type ErrorMessage = String
  private def isObject: ElementCheck = ({
    case JsObject(_) => true
    case _ => false
  }, Some("Non-object encountered in object-only array."))

  private def anyJsValue: ElementCheck = ((json: JsValue) => true, None)
}
