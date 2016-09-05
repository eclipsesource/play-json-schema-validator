package com.eclipsesource.schema.urlhandlers

import java.net.{URL, URLConnection, URLStreamHandler}

object ClasspathUrlHandler {
  def Scheme = "classpath"
  def apply = new ClasspathUrlHandler
}

/**
  * URLStreamHandler that looks for a given URL on the classpath.
  */
class ClasspathUrlHandler extends URLStreamHandler {

  override def openConnection(url: URL): URLConnection =
    getClass.getResource(url.getPath).openConnection()
}
