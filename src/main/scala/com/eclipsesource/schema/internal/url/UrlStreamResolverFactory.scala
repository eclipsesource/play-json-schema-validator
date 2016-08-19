package com.eclipsesource.schema.internal.url

import java.net.{URLStreamHandler, URLStreamHandlerFactory}

import com.eclipsesource.schema.UrlProtocolHandler

case class UrlStreamResolverFactory(
                                     protocolUrlHandlers: Map[String, URLStreamHandler] = Map.empty[String, URLStreamHandler],
                                     relativeUrlHandlers: Set[URLStreamHandler] = Set.empty[URLStreamHandler]
                                   ) extends URLStreamHandlerFactory {

  override def createURLStreamHandler(protocol: String): URLStreamHandler =
    protocolUrlHandlers.getOrElse(protocol, null)

  def addUrlHandler(handler: UrlProtocolHandler): UrlStreamResolverFactory =
    copy(protocolUrlHandlers = protocolUrlHandlers + (handler.protocol -> handler))

  def addRelativeUrlHandler(handler: URLStreamHandler): UrlStreamResolverFactory =
    copy(relativeUrlHandlers = relativeUrlHandlers + handler)

  def addUrlHandler(protocolEntry: (String, URLStreamHandler)): UrlStreamResolverFactory =
    copy(protocolUrlHandlers = protocolUrlHandlers + protocolEntry)
}
