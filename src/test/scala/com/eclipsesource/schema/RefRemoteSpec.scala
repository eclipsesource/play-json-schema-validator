package com.eclipsesource.schema

import java.net.URL

import com.eclipsesource.schema.test.JsonSpec
import controllers.Assets
import play.api.mvc.Handler
import play.api.test._

class RefRemoteSpec extends PlaySpecification {

  val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/remotes", path)
  }

  "Remote ref" should {
    "validate" in new WithServer(app = new FakeApplication(withRoutes = routes), port = 1234) {
      val resourceUrl: URL = getClass.getResource("/draft4/refRemote.json")
      foreach(JsonSpec.examplesFromUrl(resourceUrl))(example => example.execute)
    }
  }
}
