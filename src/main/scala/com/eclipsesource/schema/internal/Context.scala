package com.eclipsesource.schema.internal

import com.eclipsesource.schema.{QBContainer, QBAnnotation, QBRef}
import play.api.data.mapping.Path


case class Context(
                    path: Path,
                    parent: Option[QBContainer],
                    annotations: Seq[QBAnnotation],
                    visited: Set[QBRef]
                    )