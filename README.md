# Play JSON Schema Validator

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

This is a JSON schema validation library for Scala based on Play's JSON library and the [unified validation](https://github.com/jto/validation).
Schemas can be passed in as strings like this:

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
