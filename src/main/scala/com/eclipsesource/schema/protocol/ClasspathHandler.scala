package com.eclipsesource.schema.protocol

import java.net.{URL, URLConnection, URLStreamHandler}

/**
  * URLStreamHandler that looks for a given URL on the classpath.
  */
class ClasspathHandler extends URLStreamHandler {

  override def openConnection(url: URL): URLConnection =
    getClass.getResource(url.getPath).openConnection()

}
