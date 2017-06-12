# Play JSON Schema Validator

[![Build Status](https://travis-ci.org/eclipsesource/play-json-schema-validator.svg?branch=master)](https://travis-ci.org/eclipsesource/play-json-schema-validator) [![Coverage Status](https://coveralls.io/repos/eclipsesource/play-json-schema-validator/badge.svg?branch=master&service=github)](https://coveralls.io/github/eclipsesource/play-json-schema-validator?branch=master)

This is a JSON schema (draft v4) validation library for Scala based on Play's JSON library.

If you experience any issues or have feature requests etc., please don't hesitate to [file an issue](https://github.com/eclipsesource/play-json-schema-validator/issues/new). Thanks!

## <a name="Installation">Installation 

Add an additional resolver to your `build.sbt` file:

```
resolvers += "emueller-bintray" at "http://dl.bintray.com/emueller/maven"
```

Then add the dependency (**Scala 2.12** only, Scala 2.11 support is discontinued with 0.9.x):

```Scala
libraryDependencies ++= Seq(
  "com.eclipsesource" %% "play-json-schema-validator" % "0.9.2"
)
```
 
For Scala 2.11 use:
  
```Scala
libraryDependencies ++= Seq(
  "com.eclipsesource" %% "play-json-schema-validator" % "0.8.9"
)
```
 
## Usage

Schemas can be parsed by passing the schema string to `Json.fromJson`, for instance like this:

```Scala
  val schema = Json.fromJson[SchemaType](Json.parse(
    """{
      |"properties": {
      |  "id":    { "type": "integer" },
      |  "title": { "type": "string" },
      |  "body":  { "type": "string" }
      |}
    }""".stripMargin)).get
```

With a schema at hand, we can now validate `JsValue`s via the `SchemaValidator` (note that since 0.8.0 the validator is a class and not an object anymore):

```Scala 
val validator = new SchemaValidator()
validator.validate(schema, json)
```

`validate` returns a `JsResult[A]`. `JsResult` can either be a `JsSuccess` or a `JsError`.
`validate` is also provided with overloaded alternatives where Play's `Reads` or `Writes` instances can be passed additionally. 
This is useful for mapping `JsValue`s onto case classes and vice versa:

```Scala
validate[A](schemaUrl: URL, input: => JsValue, reads: Reads[A]) : JsResult[A]
validate[A](schemaUrl: URL, input: A, writes: Writes[A]): JsResult[JsValue] 
validate[A: Format](schemaUrl: URL, input: A): JsResult[A] 
```

## Error Reporting

In case the `validate` method returns an failure, errors can be converted to JSON by calling the `toJson` method.
Below is given an example taken from the example app:

```Scala
import com.eclipsesource.schema._ // brings toJson into scope

val result = validator.validate(schema, json, Post.reads)
result.fold(
  invalid = { errors =>  BadRequest(errors.toJson) },
  valid = { post => ... } 
)
```

Errors feature a `schemaPath`, an `instancePath`, a `value` and a `msgs` property. While `schemaPath` and `instancePath` should be self explanatory, `value` holds the validated value and `msgs` holds all errors related to the validated value. The value of the `msgs` property is always an array. Below is an example, again taken from the example app.

```Javascript
{
  "schemaPath" : "#/properties/title",
  "keyword": "minLength",
  "instancePath" : "/title",
  "value" : "a",
  "msgs" : [ "a violates min length of 3", "a does not match pattern ^[A-Z].*" ],
  "errors": []
}
```

The value of `schemaPath` will be updated when following any refs, hence when validating

```Javascript
{
  "properties": {
    "foo": {"type": "integer"},
    "bar": {"$ref": "#/properties/foo"}
  }
}
```

the generated error report's `schemaPath` property will point to `#/properties/foo`.

### id

In case the schema to validate against makes use of the `id` property to alter resolution scope (or if the schema has been loaded via an `URL`), the error report also contains a `resolutionScope` property.

### anyOf, oneOf, allOf 
In case of `allOf`, `anyOf` and `oneOf`,  the `errors` array property holds the actual sub errors. For instance, if we have a schema like the following:

```Javascript
{
  "anyOf": [
    { "type": "integer" },
    { "minimum": 2      }
  ]
}
```
and we validate the value `1.5`, the `toJson` method returns this error: 

```Javascript
[ {
  "schemaPath" : "#",
  "errors" : {
    "/anyOf/0" : [ {
      "schemaPath" : "#/anyOf/0",
      "errors" : { },
      "msgs" : [ "Wrong type. Expected integer, was number" ],
      "value" : 1.5,
      "instancePath" : "/"
    } ],
    "/anyOf/1" : [ {
      "schemaPath" : "#/anyOf/1",
      "errors" : { },
      "msgs" : [ "minimum violated: 1.5 is less than 2" ],
      "value" : 1.5,
      "instancePath" : "/"
    } ]
  },
  "msgs" : [ "Instance does not match any of the schemas" ],
  "value" : 1.5,
  "instancePath" : "/"
} ]
```

## Customizable error reporting

The validator allows you to alter error messages via [scala-i18n](https://github.com/osinka/scala-i18n),
 e.g. for localizing errors reports.
You can alter messages by placing a `messages_XX.txt` into your resources folder (by default `conf`).
The keys used for replacing messages can be found [here](https://github.com/eclipsesource/play-json-schema-validator/blob/master/src/main/resources/messages.txt).
In case you use the validator within a Play application, you'll need to convert Play's `Lang` and make it implicitly available for the `SchemaValidator`, e.g. via:
 
 ```Scala
implicit def fromPlayLang(lang: Lang): com.osinka.i18n.Lang = com.osinka.i18n.Lang(lang.locale)
```
  
## Example

An online demo of the library can be found [here](http://play-json-schema-validator.herokuapp.com/).

See the respective [github repo](https://github.com/edgarmueller/schema-validator-web) for the source code.
