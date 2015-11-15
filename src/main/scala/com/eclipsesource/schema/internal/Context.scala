package com.eclipsesource.schema.internal

import java.net.URL

import com.eclipsesource.schema.{SchemaRef, SchemaType}
import play.api.data.mapping.Path


case class Context(
  schemaPath: Path,
  instancePath: Path,
  root: SchemaType,
  visited: Set[SchemaRef],
  id: Option[String] = None,
  baseUrl: Option[URL] = None
)