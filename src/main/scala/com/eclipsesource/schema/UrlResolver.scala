package com.eclipsesource.schema

import java.net.URLStreamHandler

trait UrlResolver extends URLStreamHandler {

  def protocol: String

}
