# Play JSON Schema Validator

[![Build Status](https://travis-ci.org/eclipsesource/play-json-schema-validator.svg?branch=master)](https://travis-ci.org/eclipsesource/play-json-schema-validator) [![Coverage Status](https://coveralls.io/repos/edgarmueller/play-json-schema-validator/badge.svg?branch=master&service=github)](https://coveralls.io/github/edgarmueller/play-json-schema-validator?branch=master)

This is a JSON schema (draft v4) validation library for Scala based on Play's JSON library and the [unified validation library](https://github.com/jto/validation).

## Installation

In your `build.sbt` file, first add an additional resovler:

```
resolvers += "emueller-bintray" at "http://dl.bintray.com/emueller/maven"
```

Then add the library itself via:

```
libraryDependencies ++= Seq(
 "com.eclipsesource" %% "play-json-schema-validator" % "0.5.2"
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
  "msgs" : [ "a violates min length of 3", "a does not match pattern ^[A-Z].*" ]
}
```

### anyOf, oneOf, allOf 
In case of `allOf`, `anyOf` and `oneOf`, the failure(s) also contain an `errors` array property which holds the actual sub errors. For instance, if we have a schema like the following:

```Javascript
{
  "oneOf": [
    { "type": "integer" },
    { "minimum": 2 }
  ]
}
```
and we validate `3` the returned failure looks like this: 

```Javascript
{
  "schemaPath" : "/",
  "errors" : [ {
    "schemaPath" : "/",
    "instancePath" : "/",
    "value" : 1.5,
    "msgs" : [ "Wrong type. Expected integer, was number" ]
  }, {
    "schemaPath" : "/",
    "instancePath" : "/",
    "value" : 1.5,
    "msgs" : [ "minimum violated: 1.5 is less than 2" ]
  } ],
  "msgs" : [ "1.5 does not match any of the schemas" ],
  "value" : 1.5,
  "instancePath" : "/"
}
```

## Example

For a complete Play application that makes use of this library, please see [this example](https://github.com/edgarmueller/play-json-schema-example).
