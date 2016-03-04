package com.eclipsesource.schema

import com.eclipsesource.schema.test.JsonSpec
import controllers.Assets
import org.specs2.execute.Result
import org.specs2.specification.AfterAll
import org.specs2.specification.core.{FragmentsContinuation, Execution}
import org.specs2.specification.dsl.Online
import play.api.mvc.Handler
import play.api.test._

class RefRemoteSpec extends PlaySpecification with JsonSpec with AfterAll with Online {

  lazy val routes: PartialFunction[(String, String), Handler] = {
    case (_, path) => Assets.versioned("/remotes", path)
  }

  lazy val server = TestServer(port = 1234, FakeApplication(withRoutes = routes))

  def afterAll = server.stop

  "Validation from remote resources is possible" >> {
    { { server.start; Thread.sleep(1000) } must not(throwAn[Exception]) } continueWith
    validate("refRemote")
  }
}
