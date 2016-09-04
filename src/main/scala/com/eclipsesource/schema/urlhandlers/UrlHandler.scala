package com.eclipsesource.schema.urlhandlers

import java.net.URLStreamHandler

trait UrlHandler extends URLStreamHandler {

  def protocol: String

}
