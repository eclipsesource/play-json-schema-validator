package com.eclipsesource.schema.internal.url

import java.net.{URLStreamHandler, URLStreamHandlerFactory}

case class UrlStreamResolverFactory(
                                     private val protocolUrlHandlers: Map[String, URLStreamHandler] = Map.empty[String, URLStreamHandler],
                                     relativeUrlHandlers: Map[String, URLStreamHandler] = Map.empty[String, URLStreamHandler]
                                   ) extends URLStreamHandlerFactory {

  def hasHandlerFor(scheme: String): Boolean = protocolUrlHandlers.contains(scheme) || relativeUrlHandlers.contains(scheme)

  override def createURLStreamHandler(protocol: String): URLStreamHandler =
    protocolUrlHandlers.get(protocol).orNull

  def addUrlHandler(protocolEntry: (String, URLStreamHandler)): UrlStreamResolverFactory =
    copy(protocolUrlHandlers = protocolUrlHandlers + protocolEntry)

}
