package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.Keywords
import com.eclipsesource.schema.internal.constraints.Constraints._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.immutable.Iterable
import shapeless.syntax.std.tuple._

trait JSONSchemaReads {

  implicit val valueReader: Reads[SchemaType] = (__ \ "type").read[String].flatMap {
    case "boolean" => booleanReader.map(s => s : SchemaType)
    case "string"  => stringReader.map(s => s : SchemaType)
    case "integer" => integerReader.map(s => s : SchemaType)
    case "number"  => numberReader.map(s => s : SchemaType)
    case "array"   => arrayReader.map(s => s : SchemaType).orElse(tupleReader.map(s => s : SchemaType))
    case "object"  => delegatingObjectReader.map(s => s : SchemaType)
    case "null"    => nullReader.map(s => s : SchemaType)
  }.or {
    tupleReader.map(s => s : SchemaType)
  }.or {
    arrayReader.map(s => s : SchemaType)
  }.or {
    stringReader.map(s => s : SchemaType)
  }.or {
    numberReader.map(s => s : SchemaType)
  }.or {
    booleanConstantReader.map(s => s : SchemaType)
  }.or {
    arrayConstantReader.map(s => s : SchemaType)
  }.or {
    delegatingObjectReader.map(s => s : SchemaType)
  }.or {
    compoundReader.map(s => s : SchemaType)
  }.orElse {
    Reads.apply(_ => JsError("Invalid JSON schema"))
  }

  lazy val numberReader: Reads[SchemaNumber] = {
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

      if (typeAsString.exists(_ != "number")
        || (typeAsString.isEmpty && read.take(5).toList.forall(_.isEmpty))) {
        Reads.apply(_ => JsError("Expected number"))
      } else {
        Reads.pure(SchemaNumber(NumberConstraints(minimum, maximum, multipleOf, anyConstraints)))
      }
    })
  }

  lazy val integerReader: Reads[SchemaInteger] = {
    ((__ \ Keywords.Number.Min).readNullable[Int] and
      (__ \ Keywords.Number.Max).readNullable[Int] and
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
        || (typeAsString.isEmpty && read.take(5).toList.forall(_.isEmpty))) {
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
        anyConstraintReader
      ).tupled.flatMap(read => {

      val (minLength, maxLength, pattern, anyConstraints) = read

      if (anyConstraints.schemaTypeAsString.exists(_ != "string") ||
        (anyConstraints.schemaTypeAsString.isEmpty &&List(minLength, maxLength, pattern).forall(_.isEmpty))) {
        Reads.apply(_ => JsError("Expected string."))
      } else {
        Reads.pure(SchemaString(StringConstraints(minLength, maxLength, pattern, anyConstraints)))
      }
    })
  }

  lazy val booleanConstantReader: Reads[SchemaBooleanConstant] = new Reads[SchemaBooleanConstant] {
    override def reads(json: JsValue): JsResult[SchemaBooleanConstant] = json match {
      case bool@JsBoolean(_) => JsSuccess(SchemaBooleanConstant(bool.value))
      case _ => JsError("Expected boolean.")
    }
  }

  lazy val nullReader: Reads[SchemaNull] = {
    anyConstraintReader.flatMap(any => {
      // TODO: null constraint type could be removed
      Reads.pure(SchemaNull(NullConstraints(any)))
    })
  }

  lazy val booleanReader: Reads[SchemaBoolean] = {
    anyConstraintReader.flatMap(any => {
      // TODO: boolean constraint type could be removed
      Reads.pure(SchemaBoolean(BooleanConstraints(any)))
    })
  }

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
          case _ => JsError("Expected compound type.")
        }
      case _ => JsError("Expected json object.")
    }
  }

  lazy val arrayReader: Reads[SchemaArray] = {
    ((__ \ Keywords.Array.Items).lazyReadNullable[SchemaType](valueReader) and
      (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](valueReader) and
      (__ \ Keywords.Array.MinItems).readNullable[Int] and
      (__ \ Keywords.Array.MaxItems).readNullable[Int] and
      (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
      (__ \ Keywords.Id).readNullable[String] and
      anyConstraintReader
      ).tupled.flatMap(
        read => {
          val (items, additionalItems, minItems, maxItems, uniqueItems, id, any) = read

          if (any.schemaTypeAsString.exists(_ != "array") ||
            (any.schemaTypeAsString.isEmpty && read.take(6).toList.forall(_.isEmpty))) {
            Reads.apply(_ => JsError("Expected array."))
          } else {
            Reads.pure(
              SchemaArray(
                () => items.getOrElse(emptyObject),
                ArrayConstraints(maxItems, minItems, additionalItems, uniqueItems, any), id)
            )
          }
        })
  }

  lazy val tupleReader: Reads[SchemaTuple] = {
    ((__ \ Keywords.Array.Items).lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ Keywords.Array.AdditionalItems).lazyReadNullable[SchemaType](valueReader) and
        (__ \ Keywords.Array.MinItems).readNullable[Int] and
        (__ \ Keywords.Array.MaxItems).readNullable[Int] and
        (__ \ Keywords.Array.UniqueItems).readNullable[Boolean] and
        (__ \ Keywords.Id).readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap { read =>
      val (items, additionalItems, minItems, maxItems, uniqueItems, id, any) = read

      if (any.schemaTypeAsString.exists(_ != "array") ||
        (any.schemaTypeAsString.isEmpty &&read.take(6).toList.forall(_.isEmpty) || items.isEmpty)) {
        Reads.apply(_ => JsError("Expected tuple."))
      } else {
        Reads.pure(
          SchemaTuple(() => items.get,
            items.size,
            // initialize with empty schema
            ArrayConstraints(maxItems, minItems, additionalItems, uniqueItems, any),
            id
          )
        )
      }
    }
  }

  lazy val arrayConstantReader: Reads[SchemaArrayConstant] = {
    new Reads[SchemaArrayConstant] {
      override def reads(json: JsValue): JsResult[SchemaArrayConstant] = {
        json match {
          case JsArray(els) => JsSuccess(SchemaArrayConstant(els.collect { case str@JsString(_) => str}.toSeq))
          case _ => JsError("Expected a array of strings")
        }
      }
    }
  }

  def addRemainingProps(initObject: SchemaObject, props: Iterable[(String, JsValue)],
                        occupiedPropNames: List[String]) = {
    val remainingProps: Iterable[(String, JsValue)] = props.filterNot(prop => occupiedPropNames.contains(prop._1))
    remainingProps.foldLeft(initObject)((acc, prop) =>
      acc ++ valueReader.reads(prop._2).asOpt.fold[SchemaObject](SchemaObject())(value =>
        SchemaObject(Seq(SchemaAttribute(prop._1, value)))
      )
    )
  }

  lazy val delegatingObjectReader: Reads[SchemaObject] = {
    new Reads[SchemaObject] {
      override def reads(json: JsValue): JsResult[SchemaObject] = {
        json match {
          case JsObject(props) =>
            objectReader.reads(json).map(schemaObject => {
              addRemainingProps(schemaObject, props.toList, Keywords.ofObject)
            })
          case err => JsError(s"Expected object. Got $err")
        }
      }
    }
  }

  def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints())

  // TODO: extract into objectConstraintReader
  lazy val objectReader: Reads[SchemaObject] = {
    ((__ \ Keywords.Object.Properties).lazyReadNullable[Map[String, SchemaType]](readsInstance) and
        (__ \ Keywords.Object.PatternProperties).lazyReadNullable[Map[String, SchemaType]](readsInstance) and
        (__ \ Keywords.Object.AdditionalProperties).lazyReadNullable[SchemaType](valueReader) and
        (__ \ Keywords.Object.Required).readNullable[List[String]] and
        (__ \ Keywords.Object.Dependencies).lazyReadNullable[Map[String, SchemaType]](readsInstance) and
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
          props ++ ref.map(path => Seq(SchemaAttribute(Keywords.Object.Ref,
            SchemaRef(JSONPointer(path), isAttribute = true, isRemote = path.startsWith("http"))))
          ).getOrElse(Seq.empty),
          ObjectConstraints(
            additionalProperties,
            dependencies,
            patternProperties,
            required,
            minProperties,
            maxProperties,
            anyConstraints),
          id
        )
      )
    }
  }

  lazy val anyConstraintReader: Reads[AnyConstraint] = {
    ((__ \ Keywords.Any.Type).readNullable[String] and
      (__ \ Keywords.Any.AllOf).lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ Keywords.Any.AnyOf).lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ Keywords.Any.OneOf).lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ Keywords.Any.Definitions).lazyReadNullable(readsInstance) and
        (__ \ Keywords.Any.Enum).readNullable[Seq[JsValue]] and
        (__ \ Keywords.Any.Not).lazyReadNullable(valueReader)
      ).tupled.map(
        read => {
          val (schemaType, allOf, anyOf, oneOf, definitions, enum, not) = read

          AnyConstraint(schemaType, allOf, anyOf, oneOf, definitions, enum, not)
        }
      )
  }

  private val readsInstance: Reads[Map[String, SchemaType]] = {
    new Reads[Map[String, SchemaType]] {
      def reads(json: JsValue) = {
        json.validate[Map[String, SchemaType]]
      }
    }
  }

  private lazy val readSeqOfSchemaTypeInstance: Reads[Seq[SchemaType]] = {
    new Reads[Seq[SchemaType]] {
      override def reads(json: JsValue): JsResult[Seq[SchemaType]] = json match {
        case JsArray(els) =>
          val results = els.map(el => Json.fromJson[SchemaType](el))
          if (results.exists(_.isError)) {
            JsError("Non-object encountered in object-only array.")
          } else {
            JsSuccess(results.collect { case JsSuccess(succ, _) => succ})
          }
        case _ => JsError("Expected array")
      }
    }
  }

  private def tuples2Attributes(props: Iterable[(String, SchemaType)]): List[SchemaAttribute] = {
    props.map(property => SchemaAttribute(property._1, property._2)).toList
  }
}
