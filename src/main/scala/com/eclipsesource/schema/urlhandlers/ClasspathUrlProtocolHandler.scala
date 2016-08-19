package com.eclipsesource.schema.urlhandlers

case class ClasspathUrlProtocolHandler() extends ClasspathHandler with UrlProtocolHandler {
  override def protocol: String = "classpath"
}
