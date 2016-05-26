package com.eclipsesource.schema.internal.url

import java.net.URLStreamHandler

trait UrlResolver extends URLStreamHandler {

  def protocol: String

}
