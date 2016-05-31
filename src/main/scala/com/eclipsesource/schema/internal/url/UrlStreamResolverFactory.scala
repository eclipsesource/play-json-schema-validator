package com.eclipsesource.schema.internal.url

import java.net.{URLStreamHandler, URLStreamHandlerFactory}

trait UrlStreamResolverFactory extends URLStreamHandlerFactory {

  protected var protocolHandlers = Map.empty[String, URLStreamHandler]

  override def createURLStreamHandler(protocol: String): URLStreamHandler = {
    protocolHandlers.getOrElse(protocol, null)
  }

  def addUrlResolver(resolver: UrlResolver): this.type = {
    protocolHandlers = protocolHandlers + (resolver.protocol -> resolver)
    this
  }

  def addUrlHandler(protocol: String, handler: URLStreamHandler): this.type = {
    protocolHandlers = protocolHandlers + (protocol -> handler)
    this
  }
}
