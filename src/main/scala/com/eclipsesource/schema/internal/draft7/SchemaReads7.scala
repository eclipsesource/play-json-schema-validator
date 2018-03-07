package com.eclipsesource.schema.internal.draft7

import com.eclipsesource.schema.{SchemaVersion, _}
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import com.eclipsesource.schema.internal.draft7.constraints._
import com.eclipsesource.schema.internal.serialization.SchemaReads
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

trait SchemaReads7 extends SchemaReads { self: SchemaVersion =>

  lazy val anyKeywords = Set(
    Keywords.Any.AllOf,
    Keywords.Any.AnyOf,
    Keywords.Any.OneOf,
    Keywords.Any.Not,
    Keywords.Default,
    Keywords.Any.Definitions,
    Keywords.Any.Description,
    Keywords.Any.Enum,
    Keywords.Any.Type,
    "$id",
    "const",
    "if",
    "then",
    "else"
  )

  override lazy val objectKeywords: Set[String] = Set(
    Keywords.Schema,
    "$ref",
    Keywords.Object.Properties,
    Keywords.Object.PatternProperties,
    Keywords.Object.AdditionalProperties,
    Keywords.Object.Required,
    Keywords.Object.Dependencies,
    "propertyNames"
  ) ++ anyKeywords

  override lazy val arrayKeywords: Set[String] = Set(
    Keywords.Array.AdditionalItems,
    Keywords.Array.Items,
    Keywords.Array.MaxItems,
    Keywords.Array.MinItems,
    Keywords.Array.UniqueItems,
    "contains"
  ) ++ anyKeywords

  override lazy val schemaReadsSeq: Seq[Reads[SchemaType]] = Seq(
    refReads.map(asSchemaType),
    typeReader,
    booleanSchemaReads.map(asSchemaType),
    tupleReads.map(asSchemaType),
    arrayReads.map(asSchemaType),
    stringReads.map(asSchemaType),
    numberReads.map(asSchemaType),
    delegatingObjectReader.map(asSchemaType),
    compoundReader.map(asSchemaType)
  )

  lazy val booleanSchemaReads: Reads[SchemaValue] = {
    case b@JsBoolean(_) => JsSuccess(SchemaValue(b))
    case _ => JsError("Expected boolean schema.")
  }

  override lazy val objectReads: Reads[SchemaObject] = {
    (
      (__ \ Keywords.Object.Properties).lazyReadNullable[Map[String, SchemaType]](schemaTypeMapReader) and
        objectConstraintReads
      ).tupled.flatMap { read =>

      val (properties, constraints) = read
      val props: List[SchemaProp] = properties.map(tuples2Attributes).getOrElse(List.empty)
      Reads.pure(SchemaObject(props, constraints))
    }
  }

  override lazy val arrayReads: Reads[SchemaArray] = {
    (
      (__ \ Keywords.Array.Items).lazyReadNullable[SchemaType](schemaReads) and
        (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](withSchemaValueReader) and
        (__ \ Keywords.Array.MinItems).readNullable[Int] and
        (__ \ Keywords.Array.MaxItems).readNullable[Int] and
        (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
        (__ \ "contains").lazyReadNullable(schemaReads) and
        anyConstraintReads
      ).tupled.flatMap(
      read => {
        val (items, additionalItems, minItems, maxItems, uniqueItems, contains, any) = read
        val constraints = ArrayConstraints7(
          maxItems,
          minItems,
          additionalItems,
          contains,
          uniqueItems,
          any
        )

        if (any.schemaType.exists(_ != "array") ||
          (any.schemaType.isEmpty
            && items.isEmpty
            && additionalItems.isEmpty
            && minItems.isEmpty
            && maxItems.isEmpty
            && contains.isEmpty
            && uniqueItems.isEmpty)) {
          Reads.apply(_ => JsError("Expected array"))
        } else {
          Reads.pure(SchemaArray(items.getOrElse(emptyObject), constraints))
        }
      })
  }

  override lazy val tupleReads: Reads[SchemaTuple] = {
    (
      (__ \ Keywords.Array.Items).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader(anyJsValue)) and
        (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](withSchemaValueReader) and
        (__ \ Keywords.Array.MinItems).readNullable[Int] and
        (__ \ Keywords.Array.MaxItems).readNullable[Int] and
        (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
        (__ \ "contains").lazyReadNullable(schemaReads) and
        anyConstraintReads
      ).tupled.flatMap(
      read => {
        val (items, additionalItems, minItems, maxItems, uniqueItems, contains, any) = read
        val constraints = ArrayConstraints7(
          maxItems,
          minItems,
          additionalItems,
          contains,
          uniqueItems,
          any
        )

        if (any.schemaType.exists(_ != "array") ||
          (any.schemaType.isEmpty
            && items.isEmpty
            && additionalItems.isEmpty
            && minItems.isEmpty
            && maxItems.isEmpty
            && contains.isEmpty
            && uniqueItems.isEmpty)) {
          Reads.apply(_ => JsError("Expected array"))
        } else {
          Reads.pure(SchemaTuple(items.getOrElse(Seq.empty), constraints))
        }
      })
  }

  override lazy val numberReads: Reads[SchemaNumber] =
    numberConstraintReads.flatMap { constraints =>
      Reads.pure(SchemaNumber(constraints))
    }

  override lazy val integerReads: Reads[SchemaInteger] =
    numberConstraintReads.flatMap { constraints =>
      Reads.pure(SchemaInteger(constraints))
    }

  override lazy val stringReads: Reads[SchemaString] =
    stringConstraintReads.flatMap(constraints =>
      Reads.pure(SchemaString(constraints))
    )

  override lazy val anyConstraintReads: Reads[AnyConstraints] = {
    (
      (__ \ Keywords.Any.Type).readNullable[String] and
        (__ \ Keywords.Any.AllOf).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader()) and
        (__ \ Keywords.Any.AnyOf).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader()) and
        (__ \ Keywords.Any.OneOf).lazyReadNullable[Seq[SchemaType]](schemaTypeSeqReader()) and
        (__ \ Keywords.Any.Definitions).lazyReadNullable(schemaTypeMapReader) and
        (__ \ Keywords.Any.Enum).readNullable[Seq[JsValue]] and
        readJsNull(__ \ "const") and
        (__ \ Keywords.Any.Not).lazyReadNullable(schemaReads) and
        (__ \ Keywords.Any.Description).readNullable[String] and
        (__ \ "$id").readNullable[String] and
        (__ \ "if").lazyReadNullable(schemaReads) and
        (__ \ "then").lazyReadNullable(schemaReads) and
        (__ \ "else").lazyReadNullable(schemaReads)
      ).tupled.map { read =>

      val (schemaType, allOf, anyOf, oneOf, definitions, enum, const, not, desc, id, _if, _then, _else) = read
      AnyConstraints7(schemaType, allOf, anyOf, oneOf, definitions, enum, const, not, desc, id, _if, _then, _else)
    }
  }

  lazy val objectConstraintReads: Reads[ObjectConstraints] = {
    (
      (__ \ Keywords.Object.PatternProperties).lazyReadNullable[Map[String, SchemaType]](schemaTypeMapReader) and
        (__ \ Keywords.Object.AdditionalProperties).lazyReadNullable[SchemaType](withSchemaValueReader) and
        (__ \ Keywords.Object.Required).readNullable[List[String]] and
        (__ \ Keywords.Object.Dependencies).lazyReadNullable[Map[String, SchemaType]](mapReadsInstanceWithJsValueReader) and
        (__ \ Keywords.Object.MinProperties).readNullable[Int] and
        (__ \ Keywords.Object.MaxProperties).readNullable[Int] and
        (__ \ "propertyNames").lazyReadNullable(schemaReads) and
        anyConstraintReads
      ).tupled.flatMap { read =>

      val (
        patternProperties,
        additionalProperties,
        required,
        dependencies,
        minProperties,
        maxProperties,
        propertyNames,
        anyConstraints) = read

      val schema =  ObjectConstraints7(
        additionalProperties,
        dependencies,
        patternProperties,
        required,
        minProperties,
        maxProperties,
        propertyNames,
        anyConstraints
      )

      Reads.pure(schema)
    }
  }


  lazy val stringConstraintReads: Reads[StringConstraints] = {
    ((__ \ Keywords.String.MinLength).readNullable[Int] and
      (__ \ Keywords.String.MaxLength).readNullable[Int] and
      (__ \ Keywords.String.Pattern).readNullable[String] and
      (__ \ Keywords.String.Format).readNullable[String] and
      anyConstraintReads
      ).tupled.flatMap(read => {

      val (minLength, maxLength, pattern, format, anyConstraints) = read

      if (anyConstraints.schemaType.exists(_ != "string") ||
        (anyConstraints.schemaType.isEmpty && List(minLength, maxLength, format, pattern).forall(_.isEmpty))) {
        Reads.apply(_ => JsError("Expected string."))
      } else {
        Reads.pure(StringConstraints7(minLength, maxLength, pattern, format, anyConstraints))
      }
    })
  }

  lazy val numberConstraintReads: Reads[NumberConstraints] = {
    (
      (__ \ Keywords.Number.Min).readNullable[BigDecimal] and
        (__ \ Keywords.Number.Max).readNullable[BigDecimal] and
        (__ \ Keywords.Number.ExclusiveMin).readNullable[BigDecimal] and
        (__ \ Keywords.Number.ExclusiveMax).readNullable[BigDecimal] and
        (__ \ Keywords.Number.MultipleOf).readNullable[BigDecimal] and
        (__ \ Keywords.String.Format).readNullable[String] and
        anyConstraintReads
      ).tupled.flatMap { read =>

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, format, anyConstraints) = read
      val minimum = min.map(Minimum(_, Some(false))) orElse exclusiveMin.map(Minimum(_, Some(true)))
      val maximum = max.map(Maximum(_, Some(false))) orElse exclusiveMax.map(Maximum(_, Some(true)))
      val typeAsString = anyConstraints.schemaType

      if (typeAsString.exists(t => t != "number" && t != "integer")
        || (typeAsString.isEmpty
        && min.isEmpty
        && max.isEmpty
        && exclusiveMin.isEmpty
        && exclusiveMax.isEmpty
        && multipleOf.isEmpty)) {
        Reads.apply(_ => JsError("Expected number"))
      } else {
        Reads.pure(NumberConstraints7(minimum, maximum, multipleOf, format, anyConstraints))
      }
    }
  }
}
