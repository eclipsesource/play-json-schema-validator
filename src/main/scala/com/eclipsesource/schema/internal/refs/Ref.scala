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
  private[schema] def normalize(ref: Ref, currentScope: Option[Ref], urlHandlers: Option[UrlStreamResolverFactory] = None): Ref = {

    def isRefWithCustomScheme = ref.scheme.fold(false)(p => urlHandlers.exists(_.hasHandlerFor(p)))

    ref match {

      case _ if currentScope.exists(_.isAbsolute) && ref.isAbsolute && !isRefWithCustomScheme =>
        ref

      // ref starts with #, merge with scope id, possibly dropping a #
      case _ if ref.isFragment =>
        currentScope.map(_.dropRightHashIfAny.append(ref)).getOrElse(ref)

      // make sure we have a # at the end, if applicable, that is
      // the ref has no # and also does not end with a /
      case _ if ref.isAbsolute && !isRefWithCustomScheme =>
        ref.withHashAtEnd

      case _ if isRefWithCustomScheme && currentScope.exists(_.isAbsolute) =>
        val baseUrl = findBaseUrl(currentScope)
        baseUrl.map(url =>
          url.append(Refs.`/`).append(ref.withoutScheme.withHashAtEnd)
        ).getOrElse(ref)

      // append ref to scope id in case the latter ends with a /
      case _ if currentScope.exists(_.endsWith("/")) =>
        currentScope.map(_.append(ref)).getOrElse(ref)

      // if all cases failed, try to create a relative reference, e.g.
      // if given http://x.y.z/schema.json# and some.json#/definitions/prop
      // we want the result to be http://x.y.z/some.json#/definitions/prop
      case other =>
        val baseUrl = findBaseUrl(currentScope)
        baseUrl.map(url =>
          if (url.endsWith("/")) url.append(ref.withHashAtEnd)
          else ref.withHashAtEnd.prepend(url.append(`/`))
        ).getOrElse(ref)
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
      if (url.getHost.nonEmpty) {
        if (url.getPort != -1) {
          createUrlFromRef(Ref(s"$protocol://$host:$port"))
        } else {
          createUrlFromRef(Ref(s"$protocol://$host"))
        }
      } else createUrlFromRef(Ref(s"$protocol:${file.substring(0, file.lastIndexOf("/"))}"))
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

case class Ref(value: String) {

  private final val WithProtocol = "^([^:\\/?]+):.+"
  private final val ProtocolPattern = WithProtocol.r.pattern

  def startsWith(p: Ref) = value.startsWith(p.value)
  def startsWith(s: String) = value.startsWith(s)
  def endsWith(s: String) = value.endsWith(s)

  /**
    * Whether the ref contains any #.
    *
    * @return true, if the ref contains a # character
    */
  def hasFragment = value.contains("#")

  def isAnchor = value.startsWith("#") && value.length > 1 && value.charAt(1) != '/'

  /**
    * Whether the ref is a fragment, i.e. whether it starts with a # character.
    *
    * @return
    */
  def isFragment = value.startsWith("#")

  def isAbsolute = Try { new URI(value) }.map(_.isAbsolute).getOrElse(false)

  // TODO
  def fragments: Option[Ref] = {
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

  def length = value.length

  def drop(n: Int) = Ref(value.drop(n))

  def isRootRelative = withoutScheme.value.startsWith("#/")

  def dropHashAtStart =
    if (isRootRelative) Ref(value.substring(math.min(2, value.length)))
    else this

  def dropRightHashIfAny: Ref = {
    if (value.endsWith("#")) Ref(value.dropRight(1)) else Ref(value)
  }

  def append(otherPointer: Ref): Ref = Ref(value + otherPointer.value)

  def prepend(otherPointer: Ref): Ref = Ref(otherPointer.value + value)

  def withHashAtEnd: Ref =
    if (!hasFragment && !endsWith("/")) append(Refs.`#`)
    else this

  def withoutScheme: Ref = {
    scheme.fold(this)(s => Ref(value.drop(s.length + 3)))
  }

}

