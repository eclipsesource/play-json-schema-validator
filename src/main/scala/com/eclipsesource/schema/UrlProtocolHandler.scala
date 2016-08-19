package com.eclipsesource.schema

import java.net.URLStreamHandler

trait UrlProtocolHandler extends URLStreamHandler {

  def protocol: String

}
