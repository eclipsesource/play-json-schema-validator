# Play JSON Schema Validator

[![Build Status](https://travis-ci.org/eclipsesource/play-json-schema-validator.svg?branch=master)](https://travis-ci.org/eclipsesource/play-json-schema-validator) [![Coverage Status](https://coveralls.io/repos/eclipsesource/play-json-schema-validator/badge.svg?branch=master&service=github)](https://coveralls.io/github/eclipsesource/play-json-schema-validator?branch=master)

This is a JSON schema (draft v4) validation library for Scala based on Play's JSON library and the [unified validation library](https://github.com/jto/validation).

If you have any issues, feature requests etc, please don't hesistate to [file an issue](https://github.com/eclipsesource/play-json-schema-validator/issues/new). Thanks!

## Installation

In your `build.sbt` file, first add an additional resovler:

```
resolvers += "emueller-bintray" at "http://dl.bintray.com/emueller/maven"
```

Then add the library itself via:

```
libraryDependencies ++= Seq(
 "com.eclipsesource" %% "play-json-schema-validator" % "0.6.0"
)
``` 
 
## Usage

Schemas can be parsed by passing the schema string to `Json.fromJson` like this:

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

With a schema at hand, we can now validate `JsValue`s via the `SchemaValidator`.

```Scala 
SchemaValidator.validate(schema, json)
```

`validate` returns a `VA[JsValue]`. `VA` is part of the [unified validation library](https://github.com/jto/validation) and can either be a `Success` or a `Failure`.
`validate` is also provided with overloaded alternatives where Play's `Reads` or `Writes` instances can be passed additionally. 
This is useful for mapping `JsValue`s onto case classes and vice versa:

```Scala
validate[A](schema: SchemaType, input: => JsValue, reads: Reads[A]) : VA[A]
validate[A](schema: SchemaType, input: A, writes: Writes[A]): VA[JsValue] 
validate[A: Format](schema: SchemaType, input: A): VA[A] 
```

## Error Reporting

In case the `validate` method returns an failure, errors can be converted to JSON by calling the `toJson` on the errors. Below is given an example taken from the example app:

```Scala
val result: VA[Post] = SchemaValidator.validate(schema, json, Post.reads)
result.fold(
  invalid = { errors =>  BadRequest(errors.toJson) },
  valid = { post => ... } 
)
```

Erros feature a `schemaPath`, an `instancePath`, a `value` and a `msgs` property. While `schemaPath` and `instancePath` should be self explanatory, `value` holds the validated value and `msgs` holds all errors related to the validated value. The value of the `msgs` property is always an array. Below is an example, again taken from the example app.

```Javascript
{
  "schemaPath" : "/properties/title",
  "instancePath" : "/title",
  "value" : "a",
  "msgs" : [ "a violates min length of 3", "a does not match pattern ^[A-Z].*" ],
  "errors": []
}
```

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

An online demo of the library can be seen [here](https://ancient-atoll-3956.herokuapp.com/).

For a complete Play application that makes use of this library, please see [this example](https://github.com/edgarmueller/play-json-schema-example).
