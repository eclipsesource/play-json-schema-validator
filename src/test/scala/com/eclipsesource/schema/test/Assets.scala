package com.eclipsesource.schema.test

import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.mvc.{Action, Handler}

object Assets {

  import play.api.mvc.Results._
  implicit val mimeTypes = new DefaultFileMimeTypes(FileMimeTypesConfiguration(Map("json" -> "application/json")))

  def routes(clazz: Class[_], prefix: String = ""): PartialFunction[(String, String), Handler] = {
    case (_, path) =>

      try {
        Action(Ok.sendResource(prefix + path.substring(1), clazz.getClassLoader))
      } catch {
        case ex: Throwable => Action(BadRequest(ex.getMessage))
      }
  }
}
