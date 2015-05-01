//package com.eclipsesource.schema
//
//import org.scalameter.api._
//import play.api.data.mapping.{Failure, Path, Success, VA}
//import play.api.data.validation.ValidationError
//import play.api.libs.json.Json._
//import play.api.libs.json._
//import scala.util.Random
//
//object ValidationBenchmark extends PerformanceTest {
//
//  val WIDTH = 1
//  val DEPTH = 1
//
//  /* configuration */
//  override lazy val reporter = new LoggingReporter
//
//  lazy val executor = LocalExecutor(
//    new Executor.Warmer.Default,
//    Aggregator.min,
//    new Measurer.Default)
//  lazy val persistor = Persistor.None
//
//
//  val numberOfInstancesGen = Gen.range("numberOfInstances")(100, 1000, 100)
//
//  val instances = for {
//    numberOfInstances <- numberOfInstancesGen
//  } yield BenchmarkInstanceGenerator.generate(numberOfInstances)
//
//  performance of "QBValidator" in {
//    measure method "validate" in {
//      using(instances) in { t =>
//        validatorRun(t)
//      }
//    }
//  }
//
//  performance of "MyValidator" in {
//    measure method "validate" in {
//      using(instances) in { t =>
//        t.map {
//          obj =>
//            new MyValidator(BenchmarkSchema.auto).validate1(obj)
//        }
//      }
//    }
//  }
//
//  def validatorRun(objs: List[JsObject]) = {
//    objs.map {
//      instance =>
//        Validator.validate(BenchmarkSchema.auto)(instance)
//      //        match {
//      //          case s: JsSuccess[_] => println("s")
//      //          case e: JsError => println(e)
//      //        }
//    }
//  }
//
//  def generate(depth: Int, fieldsPerObject: Int): (QBClass, JsObject) = {
//    val start = (qbClass(), obj())
//    if (depth == 0) {
//      start
//    } else {
//      (0 to fieldsPerObject)
//        .foldLeft(start) {
//        (t: (QBClass, JsObject), idx: Int) =>
//          val name = "field" + idx
//
//          val sub = idx % 5 match {
//            case 0 => (qbBoolean, JsBoolean(true))
//            case 1 => (qbNumber(), JsNumber(42))
//            case 2 => (qbString, JsString("Some Text"))
//            case 3 =>
////              val subsub = generate(depth - 1, fieldsPerObject)
//              (qbList(qbClass()), JsArray((0 to fieldsPerObject).map({ x: Int => obj()})))
//            case 4 => generate(depth - 1, fieldsPerObject)
//          }
//
//          (t._1 ++ qbClass(name -> sub._1), t._2 + (name -> sub._2))
//      }
//    }
//  }
//
//}
//
//object BenchmarkInstanceGenerator {
//
//  import play.api.libs.json.Json._
//
//  val r = new Random(12345)
//
//  def generate(): JsObject = {
//    obj(
//      "meta" -> obj(
//        "name" -> generateString(10),
//        "make" -> generateString(15),
//        "year" -> generateNumberString(4),
//        "price" -> r.nextInt()
//      ),
//      "extra" -> arr(
//        obj(
//          "name" -> generateString(10),
//          "description" -> generateString(1000),
//          "price" -> r.nextInt()
//        )
//      ),
//      "tires" -> arr(
//        obj(
//          "diameter" -> r.nextInt(30),
//          "width" -> r.nextInt(20),
//          "color" -> "red",
//          "material" -> generateString(10)
//        )
//      ),
//      "technicalData" -> obj(
//        "engine" -> obj(
//          "capacity" -> r.nextInt(),
//          "torque" -> r.nextInt(),
//          "power" -> r.nextInt()
//        ),
//        "maxVelocity" -> r.nextInt(),
//        "weight" -> r.nextInt()
//      ),
//      "interior" -> obj(
//        "colorOne" -> generateString(10),
//        "colorTwo" -> generateString(10),
//        "colorThree" -> generateString(10)
//      ),
//      "exterior" -> obj(
//        "colorOne" -> generateString(10),
//        "colorTwo" -> generateString(10)
//      ),
//      "objects" -> createObject(ValidationBenchmark.DEPTH, ValidationBenchmark.WIDTH)
//    )
//  }
//
//  def createObject(depth: Int, width: Int): JsObject = {
//    if (depth <= 0) {
//      obj(
//        Range(0, width).map {
//          idx =>
//            idx.toString -> toJsFieldJsValueWrapper(generateString(10))
//        }: _ *
//      )
//    } else {
//      obj(
//        Range(0, width).map {
//          idx =>
//            idx.toString -> toJsFieldJsValueWrapper(createObject(depth - 1, width))
//        }: _ *
//      )
//    }
//  }
//
//  def generate(cnt: Int): List[JsObject] = {
//    (0 until cnt).map {
//      idx =>
//        generate()
//    }.toList
//  }
//
//  def generateString(cnt: Int): String = {
//    r.nextString(cnt)
//  }
//
//  def generateNumberString(cnt: Int): String = {
//    val builder = new StringBuilder()
//    for (i <- 0 until cnt) {
//      builder.append(r.nextInt(10))
//    }
//    builder.toString()
//  }
//
//}
//
//object BenchmarkSchema {
//
//  val tire = qbClass(
//    "diameter" -> qbNumber,
//    "width" -> qbNumber,
//    "color" -> qbString,
//    "material" -> qbString
//  )
//
//  val color = qbEnum("red", "blue", "green", "yellow", "magenta", "cyan")
//
//  val auto = qbClass(
//    "meta" -> qbClass(
//      "name" -> qbString,
//      "make" -> qbString,
//      "year" -> qbString,
//      "price" -> qbNumber
//    ),
//    "extra" -> qbList(qbClass(
//      "name" -> qbString,
//      "description" -> qbText,
//      "price" -> qbNumber
//    )),
//    "tires" -> qbList(tire),
//    "technicalData" -> qbClass(
//      "engine" -> qbClass(
//        "capacity" -> qbNumber,
//        "torque" -> qbNumber,
//        "power" -> qbNumber
//      ),
//      "maxVelocity" -> qbNumber,
//      "weight" -> qbNumber
//
//    ),
//    "interior" -> qbClass(
//      "colorOne" -> qbString,
//      "colorTwo" -> qbString,
//      "colorThree" -> qbString
//    ),
//    "exterior" -> qbClass(
//      "colorOne" -> qbString,
//      "colorTwo" -> qbString
//    ),
//    "objects" -> createObject(ValidationBenchmark.DEPTH, ValidationBenchmark.WIDTH)
//  )
//
//  def createObject(depth: Int, width: Int): QBClass = {
//    if (depth <= 0) {
//      qbClass(
//        Range(0, width).map {
//          idx =>
//            idx.toString -> qbString
//        }: _ *
//      )
//    } else {
//      qbClass(
//        Range(0, width).map {
//          idx =>
//            idx.toString -> createObject(depth - 1, width)
//        }: _ *
//      )
//    }
//  }
//
//}
//
//class MyValidator(val schema: QBClass) {
//
//  def validate(value: JsValue, bType: QBType, path: Path): VA[JsValue] = {
//    (value, bType) match {
//      case (v: JsNumber, t: QBNumber) => t.rule.repath(_ => path).validate(v)
//      case (v: JsNumber, t: QBInteger) => t.rule.repath(_ => path).validate(v)
//      case (v: JsBoolean, t: QBBoolean) =>  t.rule.repath(_ => path).validate(v)
//      case (v: JsString, t: QBString) =>  t.rule.repath(_ => path).validate(v)
//      case (v: JsArray, t: QBArray) =>
//        val arr: VA[JsValue] = v.value.zipWithIndex.map(item => validate(item._1, t.items, path \ item._2)).foldLeft[VA[JsValue]](Success(v)) {
//          (res: VA[JsValue], itemRes: VA[JsValue]) => (res, itemRes) match {
//            case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2): VA[JsValue]
//            case (_, f@Failure(_)) => f
//            case (succ@Success(s: JsValue), _) => succ
//          }
//        }
//        if (arr.isSuccess) {
//          t.rule.repath(_ => path).validate(arr.get)
//        } else {
//          arr
//        }
//      case (v: JsObject, t: QBClass) =>
//        val obj = t.attributes.map {
//          attr =>
//            val name = attr.name
//            val attrValue = v \ name
//            val p = Path \ name
//            validate(attrValue, attr.qbType, p)
//        }.foldLeft[VA[JsValue]](Success(v)) {
//          (res: VA[JsValue], itemRes: VA[JsValue]) => (res, itemRes) match {
//            case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2): VA[JsValue]
//            case (_, f@Failure(_)) => f
//            case (succ@Success(s: JsValue), _) => succ
//          }
//        }
//        if (obj.isSuccess) {
//          t.rule.repath(_ => path).validate(obj.get)
//        } else {
//          obj
//        }
//      case _ => Failure(List(Path() -> List(ValidationError("Expected: " + bType + " Actual: " + value))))
//    }
//  }
//
//  def validate1(json: JsValue): VA[JsValue] = validate(json, schema, Path())
//
//}
