package com.eclipsesource.schema.internal.url

import java.net.{URL, URLConnection}

case class ClasspathUrlResolver() extends UrlResolver {

  override def openConnection(url: URL): URLConnection = {
    getClass.getResource(url.getPath).openConnection()
  }

  override def protocol: String = "classpath"
}