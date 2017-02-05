# Play JSON Schema Validator

[![Build Status](https://travis-ci.org/eclipsesource/play-json-schema-validator.svg?branch=master)](https://travis-ci.org/eclipsesource/play-json-schema-validator) [![Coverage Status](https://coveralls.io/repos/eclipsesource/play-json-schema-validator/badge.svg?branch=master&service=github)](https://coveralls.io/github/eclipsesource/play-json-schema-validator?branch=master)

This is a JSON schema (draft v4) validation library for Scala based on Play's JSON library.

If you experience any issues or have feature requests etc., please don't hesitate to [file an issue](https://github.com/eclipsesource/play-json-schema-validator/issues/new). Thanks!

<a name="Installation">
## Installation

Add an additional resolver to your `build.sbt` file:

```
resolvers += "emueller-bintray" at "http://dl.bintray.com/emueller/maven"
```

Then add the dependency:

```
libraryDependencies ++= Seq(
 "com.eclipsesource" %% "play-json-schema-validator" % "0.8.7"
)
```


## Usage

*Note*: For usage instructions prior to 0.8.0, please see the wiki.

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

## Example

An online demo of the library can be looked at [here](http://play-json-schema-validator.herokuapp.com/).

See the respective [github repo](https://github.com/edgarmueller/schema-validator-web) for the source code.
