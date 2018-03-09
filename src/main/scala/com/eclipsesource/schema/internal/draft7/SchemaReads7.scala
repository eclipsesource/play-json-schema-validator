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
      lazyReadStrictOption[SchemaType](schemaReads, Keywords.Array.Items) and
        lazyReadStrictOption[SchemaType](withSchemaValueReader, Keywords.Array.AdditionalItems) and
        readStrictOption[Int](Keywords.Array.MinItems) and
        readStrictOption[Int](Keywords.Array.MaxItems) and
        readStrictOption[Boolean](Keywords.Array.UniqueItems) and
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
        lazyReadStrictOption[SchemaType](withSchemaValueReader, Keywords.Array.AdditionalItems) and
        readStrictOption[Int](Keywords.Array.MinItems) and
        readStrictOption[Int](Keywords.Array.MaxItems) and
        readStrictOption[Boolean](Keywords.Array.UniqueItems) and
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
        lazyReadStrictOption[Seq[SchemaType]](schemaTypeSeqReader(), Keywords.Any.AllOf) and
        lazyReadStrictOption[Seq[SchemaType]](schemaTypeSeqReader(), Keywords.Any.AnyOf) and
        lazyReadStrictOption[Seq[SchemaType]](schemaTypeSeqReader(), Keywords.Any.OneOf) and
        lazyReadStrictOption(schemaTypeMapReader, Keywords.Any.Definitions) and
        readStrictOption[Seq[JsValue]](Keywords.Any.Enum) and
        readJsNull(__ \ "const") and
        lazyReadStrictOption(schemaReads, Keywords.Any.Not) and
        readStrictOption[String](Keywords.Any.Description) and
        readStrictOption[String]("$id") and
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
      lazyReadStrictOption[Map[String, SchemaType]](schemaTypeMapReader, Keywords.Object.PatternProperties) and
        lazyReadStrictOption(withSchemaValueReader, Keywords.Object.AdditionalProperties) and
        readStrictOption[List[String]](Keywords.Object.Required) and
        lazyReadStrictOption[Map[String, SchemaType]](mapReadsInstanceWithJsValueReader, Keywords.Object.Dependencies) and
        readStrictOption[Int](Keywords.Object.MinProperties) and
        readStrictOption[Int](Keywords.Object.MaxProperties) and
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
    (
      readStrictOption[Int](Keywords.String.MinLength) and
        readStrictOption[Int](Keywords.String.MaxLength) and
        readStrictOption[String](Keywords.String.Pattern) and
        readStrictOption[String](Keywords.String.Format) and
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
      readStrictOption[BigDecimal](Keywords.Number.Min) and
        readStrictOption[BigDecimal](Keywords.Number.Max) and
        readStrictOption[BigDecimal](Keywords.Number.ExclusiveMin) and
        readStrictOption[BigDecimal](Keywords.Number.ExclusiveMax) and
        readStrictOption[BigDecimal](Keywords.Number.MultipleOf) and
        readStrictOption[String](Keywords.String.Format) and
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
