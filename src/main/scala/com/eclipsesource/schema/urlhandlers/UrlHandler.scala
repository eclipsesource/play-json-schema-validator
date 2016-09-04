package com.eclipsesource.schema.urlhandlers

import java.net.URLStreamHandler

object UrlHandler {
  val ProtocolLessScheme = "play-schema-validator"
}

trait UrlHandler extends URLStreamHandler {

  def protocol: String

}
