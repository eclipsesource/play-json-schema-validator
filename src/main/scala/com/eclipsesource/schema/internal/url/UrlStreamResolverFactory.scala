package com.eclipsesource.schema.internal.url

import java.net.{URLStreamHandler, URLStreamHandlerFactory}

import com.eclipsesource.schema.UrlResolver

case class UrlStreamResolverFactory(protocolHandlers: Map[String, URLStreamHandler] =
                                    Map.empty[String, URLStreamHandler]) extends URLStreamHandlerFactory {

  override def createURLStreamHandler(protocol: String): URLStreamHandler = {
    protocolHandlers.getOrElse(protocol, null)
  }

  def addUrlResolver(resolver: UrlResolver): UrlStreamResolverFactory =
    copy(protocolHandlers = protocolHandlers + (resolver.protocol -> resolver))


  def addUrlHandler(protocolEntry: (String, URLStreamHandler)): UrlStreamResolverFactory =
    copy(protocolHandlers = protocolHandlers + protocolEntry)
}
