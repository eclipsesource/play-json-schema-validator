package com.eclipsesource.schema.internal.url

import java.net.{URL, URLConnection}

import com.eclipsesource.schema.UrlResolver

case class ClasspathUrlResolver() extends UrlResolver {

  override def openConnection(url: URL): URLConnection = {
    getClass.getResource(url.getPath).openConnection()
  }

  override def protocol: String = "classpath"
}