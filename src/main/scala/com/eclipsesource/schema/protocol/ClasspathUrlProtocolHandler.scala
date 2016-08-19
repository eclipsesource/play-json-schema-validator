package com.eclipsesource.schema.protocol

import com.eclipsesource.schema.UrlProtocolHandler

case class ClasspathUrlProtocolHandler() extends ClasspathHandler with UrlProtocolHandler {
  override def protocol: String = "classpath"
}
