package com.eclipsesource.schema.urlhandlers

import java.net.URLStreamHandler

trait UrlProtocolHandler extends URLStreamHandler {

  def protocol: String

}
