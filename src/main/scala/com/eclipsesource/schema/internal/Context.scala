package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{QBType, QBContainer, QBAnnotation, QBRef}
import play.api.data.mapping.Path


case class Context(
                    path: Path,
                    root: QBType,
                    annotations: Seq[QBAnnotation],
                    visited: Set[QBRef], // should be extractable into RefResolver
                    id: Option[String] = None
                    )



