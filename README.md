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
 "com.eclipsesource" %% "play-json-schema-validator" % "0.5.1"
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

## Example

For a complete Play application that makes use of this library, please see [this example](https://github.com/edgarmueller/play-json-schema-example).
