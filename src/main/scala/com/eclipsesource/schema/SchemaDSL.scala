package com.eclipsesource.schema

import java.net.URI
import java.util.regex.Pattern

import play.api.data.mapping.Path
import play.api.libs.json._
import shapeless._

/**
 * QB DSL.
 */
trait SchemaDSL {

  val properties = "properties"

  implicit def wrapper2doubleRule(rule: DoubleRuleWrapper) = rule.rule

  case class HasDefinitions(definitions: Map[String, QBClass]) {
    def apply(name: String) = QBRef(JSONPointer(s"#/definitions/$name"))
//      def resolve: QBType = definitions.getOrElse(name, sys.error(s"no such field $name"))
//    }
  }

  def definitions(definitions: Map[String, QBClass])(schema: => HasDefinitions => QBClass) = {
    val obj = schema(HasDefinitions(definitions))
//    TODO: remove method
    obj
//    obj.copy(definitions = definitions)
  }

//  def $ref = $ref("#")

  def $ref(path: String): QBRef = {
    if (path.startsWith("http")) {
      QBRef( JSONPointer(path),  isAttribute = false, isRemote = true)
    } else {
      QBRef( JSONPointer(path))
    }
  }

  def obj: QBClass = QBClass(Seq.empty)

  // TODO: fix arbitrary types
  def tuple1(t: QBType, additionalItems: QBType = QBClass(Seq.empty)) =
    QBTuple(() => Seq(t), 1, Seq(AdditionalItemsRule(additionalItems)))
  def tuple2(t1: QBType, t2: QBType, additionalItems: QBType = QBClass(Seq.empty)) =
    QBTuple(() => Seq(t1, t2), 2, Seq(AdditionalItemsRule(additionalItems)))
  def tuple3(t1: QBType, t2: QBType, t3: QBType, additionalItems: QBType = QBClass(Seq.empty)) =
    QBTuple(() => Seq(t1, t2, t3), 3, Seq(AdditionalItemsRule(additionalItems)))
  def tuple(types: Seq[QBType], additionalItems: QBType = QBClass(Seq.empty)) =
    QBTuple(() => types, types.size, Seq(AdditionalItemsRule(additionalItems)))

  /**
   * Classes.
   */
  def qbClass(els: List[(String, QBType)])(): QBClass =
    buildClass(els)

  def qbClass(els: (String, QBType)*): QBClass =
    buildClass(els.toList)

  def qbClass(els: List[(String, QBType)], rules: ValidationRule*): QBClass =
    buildClass(els, rules)

  // TODO remove
  implicit def tuple2attribute(tuple: (String, QBType)) = tuple._2 match {
    case annotatedType: AnnotatedQBType => QBAttribute(tuple._1, annotatedType.qbType, annotatedType.annotations)
    case _ => QBAttribute(tuple._1, tuple._2)
  }

  private def buildClass(attributes: List[(String, QBType)], rules: Seq[ValidationRule] = Seq.empty): QBClass = {
    findDuplicates(attributes.map(_._1))(identity) match {
      case Nil => QBClass(attributes.toList.map(tuple2attribute), rules)
      case duplicates => throw new RuntimeException("qb.duplicate.fields - " + duplicates.mkString(","))
    }
  }

  private def findDuplicates[A, B](list: List[B])(criterion: (B) => A): Seq[A] = {
    (list.groupBy(criterion) filter { case (_, l) => l.size > 1 } keys).toSeq
  }

  def oneOf(schemas: QBClass*): QBClass = QBClass(Seq("oneOf" -> tuple(schemas)))
  def allOf(values: QBClass*): QBClass= QBClass(Seq("allOf" -> tuple(values)), Seq[ValidationRule](QBAllOfRule(values)))
  def anyOf(values: QBClass*): QBClass = QBClass(Seq("anyOf" -> tuple(values)), Seq[ValidationRule](QBAnyOfRule(values)))


  /**
   * Array Rules
   */
  // TODO: check ids
  def qbList(dataType: => QBType): QBArray =
    QBArray(() => dataType, Seq.empty, None)
  def qbList(dataType: => QBType, rules: ValidationRule*): QBArray =
    QBArray(() => dataType, rules, None)


  /**
   * String Rules
   */
  def qbString(rules: ValidationRule*): QBString = QBStringImpl(rules)
  def qbString = QBStringImpl()

  def qbText = qbString
  def qbNonEmptyText = qbString(minLength(1))

  def minLength(n: Int): MinLengthRule = new MinLengthRule(n)
  def maxLength(n: Int): MaxLengthRule = new MaxLengthRule(n)
  def length(lower: Int, upper: Int): ValidationRule = new CompositeRule {
    val rules: Seq[ValidationRule] = Seq(minLength(lower), maxLength(upper))
  }

  def qbEnum(values: String*) = qbString(new EnumRule(values.toList))

  def pattern(regex: String, errMessage: String = "") = new RegexRule(Pattern.compile(regex), errMessage)

  def qbEmail = qbString(pattern("""\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\b""", "q.invalid.email"))

//  def qbDateTime = new QBDateTimeImpl(Set(DateTimeRule))

  // TODO: DSL should not allow specifying doubles when posixTime type is used 
//  def qbPosixTime = new QBPosixTimeImpl(Set(PosixTimeRule))

  /**
   * Number Rules
   */
  def qbNumber: QBNumberImpl = QBNumberImpl()
  def qbNumber(rules: ValidationRule*): QBNumber = QBNumberImpl(rules)

  def min(n: Double) = MinRule(n, false)
  def min(n: Int): DoubleRuleWrapper = DoubleRuleWrapper(MinRule(n, false))


  def maximum(n: Int) = qbInteger(max(n))
  def maximum(n: Double) = qbNumber(max(n))
  def max(n: Double) = MaxRule(n, false)
  def max(n: Int) = DoubleRuleWrapper(MaxRule(n, false))

  def exclusiveMin(n: Int) = DoubleRuleWrapper(MinRule(n, true))
  def exclusiveMin(n: Double) = MinRule(n, true)

  def exclusiveMax(n: Int) = DoubleRuleWrapper(MaxRule(n, true))
  def exclusiveMax(n: Double) = MaxRule(n, true)

  def multipleOf(n: Double): MultipleOfRule = MultipleOfRule(n)
  def multipleOf(n: Int) = DoubleRuleWrapper(MultipleOfRule(n))

  def range(lower: Double, upper: Double): ValidationRule = new CompositeRule {
    val rules: Seq[ValidationRule] = Seq(min(lower), max(upper))
  }

  def range(lower: Int, upper: Int) = DoubleRuleWrapper(new CompositeRule {
    val rules: Seq[ValidationRule] = Seq(min(lower.toDouble), max(upper.toDouble))
  })

  def qbInteger: QBIntegerImpl = QBIntegerImpl()
  def qbInteger(rules: DoubleRuleWrapper*): QBIntegerImpl = QBIntegerImpl(rules.map(_.rule))

  /**
   * Boolean Rules
   */
  def qbBoolean: QBBoolean = QBBooleanImpl()

  /**
   * Array Rules
   */
  def unique: ValidationRule = UniquenessRule()
  def minItems(minItems: Int): ValidationRule = MinItemsRule(minItems)
  def maxItems(maxItems: Int): ValidationRule = MaxItemsRule(maxItems)

  /**
   * Object Rules
   */
  def minProperties(min: Int) = MinPropertiesRule(min)
  def maxProperties(max: Int) = MaxPropertiesRule(max)

  def qbId = qbString

  /**
   * DSL helper class
   */

  /**
   * Annotations
   */
  def default(qbType: QBType, default: JsValue): AnnotatedQBType =
    AnnotatedQBType(qbType, List(new QBDefaultAnnotation(default)))

  /**
   * Mark the attribute as optional.
   */
  def optional(qbType: QBType): AnnotatedQBType = qbType match {
    case q: AnnotatedQBType => q.copy(annotations = QBOptionalAnnotation() :: q.annotations)
    case _ => AnnotatedQBType(qbType, List(QBOptionalAnnotation()))
  }

  /**
   * Mark the attribute as optional with a default value that is used
   * in case the attribute is not present.
   */
  // TODOO: fix me
  def optional(qbType: QBType, defaultValue: JsValue): AnnotatedQBType = qbType match {
    case q: AnnotatedQBType => q.copy(annotations = QBOptionalAnnotation(Some(defaultValue)) :: q.annotations)
    case _ => AnnotatedQBType(qbType, List(QBOptionalAnnotation(Some(defaultValue))))
  }
//  AnnotatedQBType(qbType, List(QBOptionalAnnotation(Some(defaultValue))))

  /**
   * Mark the attribute as read-only.
   */
  def readOnly(qbType: QBType): AnnotatedQBType =
    AnnotatedQBType(qbType, List(QBReadOnlyAnnotation()))

  def id(qbType: QBType): AnnotatedQBType =
    AnnotatedQBType(qbType, List(QBIdAnnotation()))
}
