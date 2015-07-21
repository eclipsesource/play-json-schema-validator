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

  // TODO: asInstanceOf..
  implicit val valueReader: Reads[SchemaType] = (__ \ "type").read[String].flatMap {
    case "boolean" => booleanReader.asInstanceOf[Reads[SchemaType]]
    case "string"  => stringReader.asInstanceOf[Reads[SchemaType]]
    case "integer" => integerReader.asInstanceOf[Reads[SchemaType]]
    case "number"  => numberReader.asInstanceOf[Reads[SchemaType]]
    case "array"   => arrayReader.asInstanceOf[Reads[SchemaType]]
    case "object"  => objectReader.asInstanceOf[Reads[SchemaType]]
    case "null"    => nullReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    tupleReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    arrayReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    stringReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    numberReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    booleanConstantReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    arrayConstantReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    objectReader.asInstanceOf[Reads[SchemaType]]
  }.or {
    compoundReader.asInstanceOf[Reads[SchemaType]]
  }

  lazy val numberReader: Reads[SchemaNumber] = {
    ((__ \ "minimum").readNullable[Double] and
      (__ \ "maximum").readNullable[Double] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[BigDecimal] and
      anyConstraintReader
      ).tupled.flatMap(read => {

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, anyConstraints) = read
      val minConstraint = min.map(Minimum(_, exclusiveMin))
      val maxConstraint = max.map(Maximum(_, exclusiveMax))

      if (anyConstraints.schemaTypeAsString.exists(_ != "number") ||
        (anyConstraints.schemaTypeAsString.isEmpty && read.take(5).toList.forall(_.isEmpty))) {
        Reads.apply(_ => JsError("Expected number"))
      } else {
        Reads.pure(SchemaNumber(NumberConstraints(minConstraint, maxConstraint, multipleOf, anyConstraints)))
      }
    })
  }

  lazy val stringReader: Reads[SchemaString] = {
    ((__ \ "minLength").readNullable[Int] and
        (__ \ "maxLength").readNullable[Int] and
        (__ \ "pattern").readNullable[String] and
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
      // TODO: boolean constraint type could be removed
      Reads.pure(SchemaNull(NullConstraints(any)))
    })
  }

  lazy val booleanReader: Reads[SchemaBoolean] = {
    anyConstraintReader.flatMap(any => {
      // TODO: boolean constraint type could be removed
      Reads.pure(SchemaBoolean(BooleanConstraints(any)))
    })
  }

  lazy val integerReader: Reads[SchemaInteger] = {
    ((__ \ "minimum").readNullable[Int] and
      (__ \ "maximum").readNullable[Int] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[BigDecimal] and
      anyConstraintReader
      ).tupled.flatMap(read => {

      val (min, max, exclusiveMin, exclusiveMax, multipleOf, anyConstraints) = read
      val minimum = min.map(Minimum(_, exclusiveMin))
      val maximum = max.map(Maximum(_, exclusiveMax))

      if (anyConstraints.schemaTypeAsString.exists(_ != "integer") || (anyConstraints.schemaTypeAsString.isEmpty && read.take(5).toList.forall(_.isEmpty))) {
        Reads.apply(_ => JsError("Expected integer."))
      } else {
        Reads.pure(SchemaInteger(NumberConstraints(minimum, maximum, multipleOf, anyConstraints)))
      }
    })
  }

  lazy val compoundReader: Reads[CompoundSchemaType] = new Reads[CompoundSchemaType] {
    override def reads(json: JsValue): JsResult[CompoundSchemaType] = json match {
      case obj@JsObject(fields) =>
        obj \ "type" match {
          case JsArray(values) =>
            println("found values " + values)
            val jsResults: Seq[JsResult[SchemaType]] = values.map(value => valueReader.reads(JsObject(List("type" -> value) ++ fields.filterNot(_._1 == "type"))))
            println("found results " + jsResults)
            val successes = jsResults.collect { case JsSuccess(succ, _) => succ }
            JsSuccess(CompoundSchemaType(successes))
          case _ => JsError("Expected compound.")
        }
      case _ => JsError("Expected compund.")
    }
  }

  lazy val arrayReader: Reads[SchemaArray] = {
    ((__ \ "items").lazyReadNullable[SchemaType](valueReader) and
      (__ \ "additionalItems").lazyReadNullable[SchemaType](valueReader) and
      (__ \ "minItems").readNullable[Int] and
      (__ \ "maxItems").readNullable[Int] and
      (__ \ "uniqueItems").readNullable[Boolean] and
      (__ \ "id").readNullable[String] and
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
    ((__ \ "items").lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ "minItems").readNullable[Int] and
        (__ \ "maxItems").readNullable[Int] and
        (__ \ "additionalItems").lazyReadNullable[SchemaType](valueReader) and
        (__ \ "uniqueItems").readNullable[Boolean] and
        (__ \ "id").readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap { read =>
      val (items, minItems, maxItems, additionalItems, uniqueItems, id, any) = read

      if (any.schemaTypeAsString.exists(_ != "array") ||
        (any.schemaTypeAsString.isEmpty &&read.take(6).toList.forall(_.isEmpty) || items.isEmpty)) {
        Reads.apply(_ => JsError("Expected tuple."))
      } else {
        Reads.pure(
          SchemaTuple(() => items.get,
            items.size,
            // initialize with empty schema
            // TODO: additionalItems also could be an boolean
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

  def addRemainingProps(initObject: SchemaObject, props: Seq[(String, JsValue)], occupiedPropNames: List[String]) = {
    val remainingProps: Seq[(String, JsValue)] = props.filterNot(prop => occupiedPropNames.contains(prop._1))
    remainingProps.foldLeft(initObject)((acc, prop) =>
      acc ++ valueReader.reads(prop._2).asOpt.fold[SchemaObject](SchemaObject())(value => SchemaObject(Seq(SchemaAttribute(prop._1, value))))
    )
  }

  lazy val objectReader: Reads[SchemaObject] = {
    new Reads[SchemaObject] {
      override def reads(json: JsValue): JsResult[SchemaObject] = {
        json match {
          case JsObject(props) =>
            fallBackReader.reads(json).map(schemaObject => {
              addRemainingProps(schemaObject, props, Keywords.ofObject)
            })
          case err => JsError(s"Expected object. Got $err")
        }
      }
    }
  }

  def emptyObject: SchemaType = SchemaObject(Seq.empty, ObjectConstraints())

  // TODO: extract into objectConstraintReader
  lazy val fallBackReader: Reads[SchemaObject] = {
    ((__ \ "properties").lazyReadNullable[Map[String, SchemaType]](readsInstance) and
        (__ \ "id").readNullable[String] and
        (__ \ "patternProperties").lazyReadNullable[Map[String, SchemaType]](readsInstance) and
        (__ \ "additionalProperties").lazyReadNullable[SchemaType](valueReader) and
        (__ \ "required").readNullable[List[String]] and
        (__ \ "dependencies").lazyReadNullable[Map[String, SchemaType]](readsInstance) and
        (__ \ "minProperties").readNullable[Int] and
        (__ \ "maxProperties").readNullable[Int] and
        (__ \ "$ref").readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap { read =>

      val (properties, id, patternProperties, additionalProperties, required, dependencies, minProperties, maxProperties, ref, anyConstraints) = read

      val requiredProperties = required.map(_.toSet).getOrElse(Set.empty)
      val props: List[SchemaAttribute] = properties.map(p => tuples2Attributes(p, requiredProperties)).getOrElse(List.empty)

      Reads.pure(
        SchemaObject(
          props ++ ref.map(path => Seq(SchemaAttribute(Keywords.Object.Ref, SchemaRef(JSONPointer(path), isAttribute = true, isRemote = path.startsWith("http"))))).getOrElse(Seq.empty),
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
    ((__ \ "type").readNullable[String] and
      (__ \ "allOf").lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ "anyOf").lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ "oneOf").lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ "definitions").lazyReadNullable(readsInstance) and
        (__ \ "enum").readNullable[Seq[JsValue]]
      ).tupled.map(
        read => {
          val (schemaType, allOf, anyOf, oneOf, definitions, enum) = read

          AnyConstraint(schemaType, allOf, anyOf, oneOf, definitions, enum)
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

  private def tuples2Attributes(props: Iterable[(String, SchemaType)], requiredProperties: Set[String]): List[SchemaAttribute] = {
    props.map(property =>
      if (requiredProperties.contains(property._1)) {
        SchemaAttribute(property._1, property._2, List())
      } else {
        SchemaAttribute(property._1, property._2, List(SchemaOptionalAnnotation()))
      }
    ).toList
  }
}
