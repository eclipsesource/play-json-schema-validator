package com.eclipsesource.schema.urlhandlers

import java.net.{URL, URLConnection}

/**
  * URLStreamHandler that looks for a given URL on the classpath.
  */
class ClasspathUrlHandler extends UrlHandler {

  override def openConnection(url: URL): URLConnection =
    getClass.getResource(url.getPath).openConnection()

  override def protocol: String = "classpath"
}
