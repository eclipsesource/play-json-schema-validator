package com.eclipsesource.schema.internal.url

import java.net.{URLStreamHandler, URLStreamHandlerFactory}

import com.eclipsesource.schema.urlhandlers.UrlProtocolHandler

case class UrlStreamResolverFactory(
                                     private val protocolUrlHandlers: Map[String, URLStreamHandler] = Map.empty[String, URLStreamHandler],
                                     relativeUrlHandlers: Map[String, URLStreamHandler] = Map.empty[String, URLStreamHandler]
                                   ) extends URLStreamHandlerFactory {

  def contains(p: String): Boolean = protocolUrlHandlers.contains(p) || relativeUrlHandlers.contains(p)

  override def createURLStreamHandler(protocol: String): URLStreamHandler =
    protocolUrlHandlers.get(protocol).orNull

  def addUrlHandler(handler: UrlProtocolHandler): UrlStreamResolverFactory =
    copy(protocolUrlHandlers = protocolUrlHandlers + (handler.protocol -> handler))

  def addUrlHandler(protocolEntry: (String, URLStreamHandler)): UrlStreamResolverFactory =
    copy(protocolUrlHandlers = protocolUrlHandlers + protocolEntry)

  def addRelativeUrlHandler(handler: UrlProtocolHandler): UrlStreamResolverFactory =
    copy(relativeUrlHandlers = relativeUrlHandlers + (handler.protocol -> handler))

  def addRelativeUrlHandler(handler: (String, URLStreamHandler)): UrlStreamResolverFactory =
    copy(relativeUrlHandlers = relativeUrlHandlers + (handler._1 -> handler._2))

}
