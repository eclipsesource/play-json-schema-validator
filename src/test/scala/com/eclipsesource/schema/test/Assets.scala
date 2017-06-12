package com.eclipsesource.schema.test

import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.mvc.{Action, Handler}

object Assets {

  import play.api.mvc.Results._
  implicit val mimeTypes = new DefaultFileMimeTypes(FileMimeTypesConfiguration(Map("json" -> "application/json")))

  def routes(clazz: Class[_], prefix: String = ""): PartialFunction[(String, String), Handler] = {
    case (_, path) =>
      try {
        val resourceName = prefix + path.substring(1)
        Option(clazz.getClassLoader.getResource(resourceName))
          .map(_ => Action(Ok.sendResource(resourceName, clazz.getClassLoader)))
          .getOrElse(Action(BadRequest(s"$resourceName not found.")))
      } catch {
        case ex: Throwable =>
          Action(BadRequest(ex.getMessage))
      }
  }
}
