package com.eclipsesource.schema

import com.eclipsesource.schema.drafts.Version7
import com.eclipsesource.schema.test.JsonSpec
import org.specs2.mutable.Specification

class RefRemoteDraft7Spec extends Specification with JsonSpec {

  import Version7._

  implicit val validator: SchemaValidator = SchemaValidator(Some(Version7))
    .addSchema(
      "http://localhost:1234/integer.json",
      JsonSource.schemaFromStream(
        getClass.getResourceAsStream("/remotes/integer.json")
      ).get
    )
    .addSchema(
      "http://localhost:1234/subSchemas.json",
      JsonSource.schemaFromStream(
        getClass.getResourceAsStream("/remotes/subSchemas.json")
      ).get
    )
    .addSchema(
      "http://localhost:1234/folder/folderInteger.json",
      JsonSource.schemaFromStream(
        getClass.getResourceAsStream("/remotes/folder/folderInteger.json")
      ).get
    )
    .addSchema(
      "http://localhost:1234/name.json",
      JsonSource.schemaFromStream(
        getClass.getResourceAsStream("/remotes/name.json")
      ).get
    )

  "refRemote draft7" in {
    validate("refRemote", "draft7")
  }
}
