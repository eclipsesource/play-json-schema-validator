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
    (__ \ "items").read[Seq[JsValue]].flatMap {
      it => {
        tupleReader.asInstanceOf[Reads[SchemaType]]
      }
    }
  }.or {
    (__ \ "items").read[JsValue].flatMap {
      it => {
        arrayReader.asInstanceOf[Reads[SchemaType]]
      }
    }
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
  }

  lazy val numberReader: Reads[SchemaNumber] = {
    ( (__ \ "type").readNullable[String] /*(verifying(_ == "number"))*/ and
      (__ \ "minimum").readNullable[Double] and
      (__ \ "maximum").readNullable[Double] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[Double] and
      anyConstraintReader
      ).tupled.flatMap(opts => {

      val isMinExclusive = opts._4
      val isMaxExclusive = opts._5
      val minConstraint = opts._2.map(min => Minimum(min, isMinExclusive))
      val maxConstraint = opts._3.map(max => Maximum(max, isMaxExclusive))
      val multipleOf = opts._6
      val anyConstraints = opts._7

      if ( opts.take(6).toList.forall(_.isEmpty)) {
        Reads.apply(_ => JsError(""))
      } else {
        Reads.pure(SchemaNumber(NumberConstraints(minConstraint, maxConstraint, multipleOf, anyConstraints)))
      }
    })
  }

  lazy val stringReader: Reads[SchemaString] = {
    (
      (__ \ "type").readNullable[String] /*(verifying(_ == "string"))*/ and
        (__ \ "minLength").readNullable[Int] and
        (__ \ "maxLength").readNullable[Int] and
        (__ \ "format").readNullable[String] and
        anyConstraintReader
      ).tupled.flatMap(opts => {

      val stringType = opts._1
      val minLength = opts._2
      val maxLength = opts._3
      val format = opts._4
      val anyConstraints  = opts._5

      if (stringType.isDefined && stringType.get != "string" || List(stringType, minLength, maxLength, format).forall(_.isEmpty)) {
        Reads.apply(_ => JsError("Expected string."))
      } else {
        Reads.pure(SchemaString(StringConstraints(minLength, maxLength, format, anyConstraints)))
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
    (__ \ "type").readNullable[String](verifying(_ == "null")).flatMap(opt =>
      opt.fold[Reads[SchemaNull]](Reads.apply(_ => JsError("Expected integer.")))(
        _ => Reads.pure(SchemaNull())
      )
    )
  }

  lazy val booleanReader: Reads[SchemaBoolean] = {
    ((__ \ "type").readNullable[String] /*(verifying(_ == "boolean")) */ and
      anyConstraintReader
      ).tupled.flatMap(opts => {

      // TODO: boolean constraint type could be removed
      Reads.pure(SchemaBoolean(BooleanConstraints(opts._2)))
    })
  }

  lazy val integerReader: Reads[SchemaInteger] = {
    ((__ \ "type").readNullable[String] /*(verifying(_ == "integer")) */ and
      (__ \ "minimum").readNullable[Int] and
      (__ \ "maximum").readNullable[Int] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[Double] and
      anyConstraintReader
      ).tupled.flatMap(opts => {

      val isMinExclusive = opts._4
      val isMaxExclusive = opts._5
      val minRule        = opts._2.map(Minimum(_, isMinExclusive))
      val maxRule        = opts._3.map(Maximum(_, isMaxExclusive))
      val multipleOf     = opts._6
      val anyConstraints = opts._7

      if (opts.take(6).toList.forall(_.isEmpty)) {
        Reads.apply(_ => JsError("Expected integer."))
      } else {
        // TODO: constraitns
        Reads.pure(SchemaInteger(NumberConstraints(minRule, maxRule, multipleOf, anyConstraints)))
      }
    })
  }


  lazy val arrayReader: Reads[SchemaArray] = {
    ((__ \ "items").read[SchemaType] and
      (__ \ "additionalItems").readNullable[SchemaType] and
      (__ \ "minItems").readNullable[Int] and
      (__ \ "maxItems").readNullable[Int] and
      (__ \ "uniqueItems").readNullable[Boolean] and
      (__ \ "id").readNullable[String] and
      anyConstraintReader
      ).tupled.map(
        read => {
          val (items, additionalItems, minItems, maxItems, uniqueItems, id, any) = read
          // TODO: other properties are missing
          val schemaType: JsResult[SchemaType] = valueReader.reads(Json.toJson(items))
          // TODO: rules missing, get
          // TODO: only updated inner ids, not outer, also, refs need to be updated if ids are presents
          SchemaArray(() => schemaType.get, ArrayConstraints(minItems, maxItems, additionalItems, uniqueItems, any), id)
        })
  }

  lazy val tupleReader: Reads[SchemaTuple] = {
    (
      (__ \ "items").read[Seq[JsValue]] and
        (__ \ "minItems").readNullable[Int] and
        (__ \ "maxItems").readNullable[Int] and
        (__ \ "additionalItems").readNullable[SchemaType] and
        (__ \ "uniqueItems").readNullable[Boolean] and
        (__ \ "id").readNullable[String] and
        anyConstraintReader
      ).tupled.map { read =>
      val (items, minItems, maxItems, additionalItems, uniqueItems, id, any) = read
      val tupleTypes: Seq[JsResult[SchemaType]] = items.map(valueReader.reads)
      val successTupleTypes: Seq[SchemaType] = tupleTypes.collect { case JsSuccess(succ, _) => succ}.toSeq
      SchemaTuple(() => successTupleTypes,
        successTupleTypes.size,
        // initialize with empty schema
        // TODO: additionalItems also could be an boolean
        ArrayConstraints(minItems, maxItems, additionalItems, uniqueItems, any),
        id
      )
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
    (
      (__ \ "properties").lazyReadNullable[Map[String, SchemaType]](readsInstance) and
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

      val (properties, id, patternProperties, additionalProperties, required, dependencies, min, max, ref, anyConstraints) = read

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
            anyConstraints),
          id
        )
      )
    }
  }

  lazy val anyConstraintReader: Reads[AnyConstraint] = {
    (
      (__ \ "allOf").lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ "anyOf").lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ "oneOf").lazyReadNullable[Seq[SchemaType]](readSeqOfSchemaTypeInstance) and
        (__ \ "definitions").lazyReadNullable(readsInstance) and
        (__ \ "enum").readNullable[Seq[JsValue]]
      ).tupled.map(
        read => {
          val (allOf, anyOf, oneOf, definitions, enum) = read

          AnyConstraint(allOf, anyOf, oneOf, definitions, enum)
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
