package com.eclipsesource.schema.internal.refs

import java.net.{URI, URL}
import com.eclipsesource.schema.internal.url.UrlStreamResolverFactory
import scala.util.Try

object Refs {
  val `#` = Ref("#")
  val `/` = Ref("/")

  /**
    * Normalizes a given ref, i.e. the result of this method
    * is an always absolute URL wrapped in a ref, even if the given ref
    * is relative.
    *
    * @param ref the ref to be normalized
    * @param currentScope the current resolution scope
    * @return an absolute URL wrapped in a ref
    */
  private[schema] def mergeRefs(ref: Ref, currentScope: Option[Ref], urlHandlers: Option[UrlStreamResolverFactory] = None): Ref = {

    ref match {

      case a@AbsoluteRef(_) =>
        a

      case l@LocalRef(localRef) =>
        currentScope.map(scope =>
          if (scope.value.endsWith("#")) Ref(scope.value.init + localRef)
          else Ref(scope.value.init + localRef)
        ).getOrElse(l)

      case r@RelativeRef(relativeRef) =>
        if (relativeRef.startsWith("#") && currentScope.exists(_.value.startsWith("#"))) {
          // both refs are plain name fragments, switch scope
          ref
        } else
        if (relativeRef.startsWith("#")) {
          // ref is plain name fragment
          currentScope.map(url =>
            if (url.value.endsWith("#")) Ref(url.value.init + relativeRef)
            else Ref(url.value + relativeRef)
          ).getOrElse(r)
        } else {
          findBaseUrl(currentScope).map(
            baseUrl =>
              if (currentScope.exists(_.endsWith("#")) && !relativeRef.contains("#"))
                Ref(baseUrl.value + relativeRef + "#")
              else
                Ref(baseUrl.value + relativeRef)
          ).getOrElse(r)
        }
    }
  }

  /**
    * Finds out the actual base URL based on a given resolution scope.
    *
    * @param scope the resolution scope from which to determine the base URL
    * @return the base URL, if any, otherwise None
    */
  private[schema] def findBaseUrl(scope: Option[Ref]): Option[Ref] = {

    def createUrlFromRef(ref: Ref) = Try { new URL(null, ref.value) }.toOption

    def createBaseUrl(url: URL, protocol: String, host: String, port: Int, file: String): Option[URL] = {
      val f = file.substring(0, file.lastIndexOf("/") + 1)

      if (url.getHost.nonEmpty) {
        if (url.getPort != -1) {
          createUrlFromRef(Ref(s"$protocol://$host:$port$f"))
        } else {
          createUrlFromRef(Ref(s"$protocol://$host$f"))
        }
      } else createUrlFromRef(Ref(s"$protocol:$f"))
    }

    val url: Option[URL] = for {
      id       <- scope
      url      <- createUrlFromRef(id)
      protocol = url.getProtocol
      host     = url.getHost
      port     = url.getPort
      file     = url.getFile
      baseUrl  <- createBaseUrl(url, protocol, host, port, file)
    } yield baseUrl

    url.map(u => Ref(u.toString))
  }
}

trait Ref {

  def value: String
  private final val WithProtocol = "^([^:\\/?]+):.+"
  private final val ProtocolPattern = WithProtocol.r.pattern

  def endsWith(s: String): Boolean = value.endsWith(s)

  /**
    * Whether the ref contains any #.
    *
    * @return true, if the ref contains a # character
    */
  def hasFragment: Boolean = value.contains("#")

  def pointer: Option[Ref] = {
    if (hasFragment) Some(Ref(value.substring(value.indexOf("#"))))
    else None
  }

  def documentName: Ref = {
    val hashIdx = value.indexOf("#")
    if (hasFragment) Ref(value.substring(0, hashIdx)) else this
  }

  def scheme: Option[String] = {
    val matcher = ProtocolPattern.matcher(value)
    matcher.find()
    Try { matcher.group(1).replaceAll("[^A-Za-z]+", "") }.toOption
  }
}

object Ref {

  private def isAbsolute(value: String) = Try { new URI(value) }
    .map(_.isAbsolute)
    .getOrElse(false)

  def apply(ref: String): Ref = {
    if (isAbsolute(ref)) {
      AbsoluteRef(ref)
    } else if (ref.startsWith("#/") || ref == "#") {
      LocalRef(ref)
    } else {
      RelativeRef(ref)
    }
  }
}

case class LocalRef(value: String) extends Ref
case class RelativeRef(value: String) extends Ref
case class AbsoluteRef(value: String) extends Ref
