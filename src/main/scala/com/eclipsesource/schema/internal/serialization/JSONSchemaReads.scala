package com.eclipsesource.schema.internal.serialization

import com.eclipsesource.schema._
import com.eclipsesource.schema.internal.Keywords
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.collection.immutable.Iterable
import shapeless.poly._
import shapeless.syntax.std.tuple._

trait JSONSchemaReads {

  // TODO: asInstanceOf..
  implicit val valueReader: Reads[QBType] = (__ \ "type").read[String].flatMap {
    case "boolean" => booleanReader.asInstanceOf[Reads[QBType]]
    case "string" => stringReader.asInstanceOf[Reads[QBType]]
    case "integer" => integerReader.asInstanceOf[Reads[QBType]]
    case "number" => numberReader.asInstanceOf[Reads[QBType]]
    case "array" =>arrayReader.asInstanceOf[Reads[QBType]]
    case "object" => objectReader.asInstanceOf[Reads[QBType]]
    case "null" => nullReader.asInstanceOf[Reads[QBType]]
  }.or {
    (__ \ "items").read[Seq[JsValue]].flatMap {
      it => {
        tupleReader.asInstanceOf[Reads[QBType]]
      }
    }
  }.or {
    (__ \ "items").read[JsValue].flatMap {
      it => {
        arrayReader.asInstanceOf[Reads[QBType]]
      }
    }
  }.or {
    stringReader.asInstanceOf[Reads[QBType]]
  }.or {
    numberReader.asInstanceOf[Reads[QBType]]
  }.or {
    booleanReader.asInstanceOf[Reads[QBType]]
  }.or {
    arrayConstantReader.asInstanceOf[Reads[QBType]]
  }.or {
    objectReader.asInstanceOf[Reads[QBType]]
  }

  def numberReader: Reads[QBNumber] = {
    ( (__ \ "type").readNullable[String] and
      (__ \ "minimum").readNullable[Double] and
      (__ \ "maximum").readNullable[Double] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[Double] and
      (__ \ "allOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance) and
      (__ \ "anyOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance)
      ).tupled.flatMap(opts => {

      val isMinExclusive = opts._4.getOrElse(false)
      val isMaxExclusive = opts._5.getOrElse(false)
      val minRule = opts._2.map(MinRule(_, isMinExclusive))
      val maxRule = opts._3.map(MaxRule(_, isMaxExclusive))
      val multipleOf = opts._6.map(MultipleOfRule)
      val allOfRule  = opts._7.map(schemas => QBAllOfRule(schemas))
      val anyOfRule  = opts._8.map(schemas => QBAnyOfRule(schemas))

      if (opts.take(6).toList.forall(_.isEmpty)) {
        Reads.apply(_ => JsError(""))
      } else {
        Reads.pure(QBNumberImpl(Seq(minRule, maxRule, multipleOf, allOfRule, anyOfRule).filterNot(_.isEmpty).map(_.get)))
      }
    })
  }



  lazy val stringReader: Reads[QBString] = {
    (
      (__ \ "type").readNullable[String] and
        (__ \ "minLength").readNullable[Int] and
        (__ \ "maxLength").readNullable[Int] and
        (__ \ "allOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance) and
        (__ \ "anyOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance)
      ).tupled.flatMap(opts => {

      val stringType = opts._1
      val minRule    = opts._2.map(MinLengthRule)
      val maxRule    = opts._3.map(MaxLengthRule)
      val allOfRule  = opts._4.map(schemas => QBAllOfRule(schemas))
      val anyOfRule  = opts._5.map(schemas => QBAnyOfRule(schemas))

      val rules: List[Option[ValidationRule]] = List(
        minRule,
        maxRule,
        allOfRule,
        anyOfRule
      )
      val ruleSet: Seq[ValidationRule] = rules.flatten

      if (List(stringType, minRule, maxRule).forall(_.isEmpty)) {
        Reads.apply(_ => JsError("Expected string."))
      } else {
        Reads.pure(QBStringImpl(ruleSet))
      }
    })
  }

  lazy val arrayReader: Reads[QBArray] = {
    ((__ \ "items").read[QBType] and
      (__ \ "id").readNullable[String] and
      (__ \ "additionalItems").readNullable[QBType] and
      (__ \ "allOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance) and
      (__ \ "anyOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance) and
      (__ \ "oneOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance)
      ).tupled.map(read => {

          val (items, id, additionalItems, allOf, anyO, oneOf) = read
          println("Array reader triggered")

          // TODO: other properties are missing
          val qbType: JsResult[QBType] = valueReader.reads(Json.toJson(items))
          // TODO: only updated inner ids, not outer, also, refs need to be updated if ids are presents
          QBArray(
            () => qbType.get, //id.map(i => qbType.get.setResolutionScope(i + qbType.get.resolutionScope.getOrElse(""))).getOrElse(qbType.get),
            Seq.empty[ValidationRule],
            id
          )
        })
  }


  lazy val nullReader: Reads[QBNull] = new Reads[QBNull] {
    override def reads(json: JsValue): JsResult[QBNull] = {
      json match {
        case JsNull => JsSuccess(QBNull())
        case _ => JsError("Expected null")
      }
    }
  }

  lazy val booleanReader: Reads[QBBooleanConstant] = new Reads[QBBooleanConstant] {
    override def reads(json: JsValue): JsResult[QBBooleanConstant] = json match {
      case bool@JsBoolean(_) => JsSuccess(QBBooleanConstant(bool.value))
      case _ => JsError("Expected boolean.")
    }
  }

  lazy val integerReader: Reads[QBInteger] = {
    ((__ \ "type").readNullable[String] and
      (__ \ "minimum").readNullable[Int] and
      (__ \ "maximum").readNullable[Int] and
      (__ \ "exclusiveMinimum").readNullable[Boolean] and
      (__ \ "exclusiveMaximum").readNullable[Boolean] and
      (__ \ "multipleOf").readNullable[Int] and
      (__ \ "allOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance) and
      (__ \ "anyOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance)
      ).tupled.flatMap(opts => {

      val isMinExclusive = opts._4.getOrElse(false)
      val isMaxExclusive = opts._5.getOrElse(false)
      val minRule        = opts._2.map(MinRule(_, isMinExclusive))
      val maxRule        = opts._3.map(MaxRule(_, isMaxExclusive))
      val multipleOf     = opts._6.map(MultipleOfRule(_))
      val allOfRule      = opts._7.map(schemas => QBAllOfRule(schemas))
      val anyOfRule      = opts._8.map(schemas => QBAnyOfRule(schemas))

      if (opts.take(6).toList.forall(_.isEmpty)) {
        Reads.apply(_ => JsError("Expected integer."))
      } else {
        Reads.pure(QBIntegerImpl(Seq(minRule, maxRule, multipleOf).filterNot(_.isEmpty).map(_.get)))
      }
    })
  }

  lazy val tupleReader: Reads[QBTuple] = {
    (
      (__ \ "items").read[Seq[JsValue]]
        and
        (__ \ "additionalItems").readNullable[QBType]
      ).tupled.map { read => {

      val (items, additionalItems) = read
        val tupleTypes: Seq[JsResult[QBType]] = items.map(valueReader.reads)
        val successTupleTypes: Seq[QBType] = tupleTypes.collect { case JsSuccess(succ, _) => succ}.toSeq
        QBTuple(() => successTupleTypes,
          successTupleTypes.size,
          // initialize with empty schema
          // TODO: Option should be removed
          additionalItems.fold(Seq.empty[ValidationRule])(additional => Seq(AdditionalItemsRule(additional))))
      }
    }
  }

  lazy val booleanConstantReader: Reads[QBBooleanConstant] = {
    new Reads[QBBooleanConstant] {
      override def reads(json: JsValue): JsResult[QBBooleanConstant] = {
        json match {
          case JsBoolean(bool) => JsSuccess(QBBooleanConstant(bool))
          case _ => JsError("Expected a boolean")
        }
      }
    }
  }

  lazy val arrayConstantReader: Reads[QBArrayConstant] = {
    new Reads[QBArrayConstant] {
      override def reads(json: JsValue): JsResult[QBArrayConstant] = {
        json match {
          case JsArray(els) => JsSuccess(QBArrayConstant(els.collect { case JsString(str) => str}.toSeq))
          case _ => JsError("Expected a array of strings")
        }
      }
    }
  }

  def addRemainingProps(initObject: QBClass, props: Seq[(String, JsValue)], occupiedPropNames: List[String]) = {
    val remainingProps: Seq[(String, JsValue)] = props.filterNot(prop => occupiedPropNames.contains(prop._1))
    remainingProps.foldLeft(initObject)((acc, prop) =>
      acc ++ valueReader.reads(prop._2).asOpt.fold[QBClass](obj)(value => QBClass(Seq(prop._1 -> value)))
    )
  }


  lazy val objectReader: Reads[QBClass] = {
    new Reads[QBClass] {
      override def reads(json: JsValue): JsResult[QBClass] = {
        json match {
          case JsObject(props) =>
            fallBackReader.reads(json).map(qbObject => {
              addRemainingProps(qbObject, props, Keywords.ofObject)
            })
          case err => println(s"Expected object. Got $err."); JsError(s"Expected object. Got $err")
        }
      }
    }
  }

  lazy val fallBackReader: Reads[QBClass] = {
    (
      (__ \ "properties").lazyReadNullable[Map[String, QBType]](readsInstance) and
        (__ \ "id").readNullable[String] and
        (__ \ "patternProperties").lazyReadNullable[Map[String, QBType]](readsInstance) and
        (__ \ "additionalProperties").lazyReadNullable[QBType](valueReader) and
        (__ \ "required").readNullable[List[String]] and
        (__ \ "dependencies").lazyReadNullable[QBClass](objectReader) and
        (__ \ "allOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance) and
        (__ \ "anyOf").lazyReadNullable[Seq[QBType]](readSeqOfQBTypeInstance) and
        (__ \ "$ref").readNullable[String]
      ).tupled.flatMap { read =>

        val (properties, id, patternProperties, additionalProperties, required, dependencies, all, any, ref) = read

        val requiredProperties = required.map(_.toSet).getOrElse(Set.empty)
        val props: List[QBAttribute] = properties.map(p => tuples2Attributes(p, requiredProperties)).getOrElse(List.empty)

        val additional = AdditionalProperties(additionalProperties.getOrElse(QBClass(Seq.empty)))
        val pattern = patternProperties.map(pp => PatternProperties(pp.toSeq.map(t => QBAttribute(t._1, t._2))))

        val propertiesRule: PropertiesRule = PropertiesRule(pattern, additional)

        val rules: List[Option[ValidationRule]] = List(
          dependencies.map((deps: QBClass) => DependenciesRule(deps)),
          Some(propertiesRule),
          all.map(schemas => QBAllOfRule(schemas)),
          any.map(schemas => QBAnyOfRule(schemas))
        )

        Reads.pure(
          QBClass(
            Seq("properties" -> QBClass(props)),
            rules.flatten,
            id,
            required.getOrElse(Seq.empty)
          ) //++ id.fold(QBClass(Seq.empty))(i => QBClass(Seq("id" -> QBRef(JSONPointer(i), true, i.startsWith("http")))))
            ++ ref.map(path => QBClass(
            Seq(Keywords.Ref ->
              QBRef(JSONPointer(path), isAttribute = true, isRemote = path.startsWith("http"), resScope = id)
            ))
          ).getOrElse(QBClass(Seq.empty))
//            ++ patternProperties.fold(QBClass(Seq.empty))(ps => QBClass(Seq(Keywords.PatternProperties -> QBClass(tuples2Attributes(ps, requiredProperties)))))
//            ++ additionalProperties.fold(QBClass(Seq.empty))(ap => QBClass(Seq(Keywords.AdditionalProperties -> ap)))
//            ++ dependencies.fold(QBClass(Seq.empty))(d => QBClass(Seq(Keywords.Dependencies -> d)))
        )
    }
  }

  private val readsInstance: Reads[Map[String, QBType]] = {
    new Reads[Map[String, QBType]] {
      def reads(json: JsValue) = {
        json.validate[Map[String, QBType]]
      }
    }
  }

  private lazy val readSeqOfQBTypeInstance: Reads[Seq[QBType]] = {
    new Reads[Seq[QBType]] {
      override def reads(json: JsValue): JsResult[Seq[QBType]] = json match {
        case JsArray(els) =>
          val results = els.map(el => Json.fromJson[QBType](el))
          if (results.exists(_.isError)) {
            JsError("Non-object encountered in object-only array.")
          } else {
            JsSuccess(results.collect { case JsSuccess(succ, _) => succ})
          }
        case _ => JsError("Expected array")
      }
    }
  }

  private def tuples2Attributes(props: Iterable[(String, QBType)], requiredProperties: Set[String]): List[QBAttribute] = {
    props.map(property =>
      if (requiredProperties.contains(property._1)) {
        QBAttribute(property._1, property._2, List())
      } else {
        QBAttribute(property._1, property._2, List(QBOptionalAnnotation()))
      }
    ).toList
  }
}
