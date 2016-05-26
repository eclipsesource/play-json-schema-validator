package com.eclipsesource.schema.internal.url

import java.net.{URLStreamHandler, URLStreamHandlerFactory}

trait UrlStreamHandlerFactory extends URLStreamHandlerFactory {

  protected var protocolHandlers = Map.empty[String, UrlResolver]

  override def createURLStreamHandler(protocol: String): URLStreamHandler = {
    protocolHandlers.getOrElse(protocol, null)
  }

  def addUrlHandler(resolver: UrlResolver): this.type = {
    protocolHandlers = protocolHandlers + (resolver.protocol -> resolver)
    this
  }
}