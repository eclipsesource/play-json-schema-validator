package com.eclipsesource.schema.urlhandlers

class ClasspathUrlProtocolHandler extends ClasspathUrlHandler with UrlProtocolHandler {
  override def protocol: String = "classpath"
}
