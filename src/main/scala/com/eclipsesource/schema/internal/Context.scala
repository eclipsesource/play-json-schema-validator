package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{SchemaType, SchemaContainer, SchemaAnnotation, SchemaRef}
import play.api.data.mapping.Path


case class Context(
                    path: Path,
                    root: SchemaType,
                    annotations: Seq[SchemaAnnotation],
                    visited: Set[SchemaRef],
                    id: Option[String] = None
                    )