package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{SchemaRef, SchemaType}
import play.api.data.mapping.Path


case class Context(
  schemaPath: Path,
  instancePath: Path,
  root: SchemaType,
  visited: Set[SchemaRef],
  id: Option[String] = None
)