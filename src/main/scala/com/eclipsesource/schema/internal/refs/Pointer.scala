package com.eclipsesource.schema.internal.refs

import java.net.{URI, URL}

import play.api.data.validation.ValidationError

import scala.util.{Success, Try}

object Pointers {
  val `#` = Pointer("#")
  val `/` = Pointer("/")

  /**
    * Normalizes a pointer URL, i.e. the outcome of this method
    * is an always absolute URL, even if the given pointer
    * is relative.
    *
    * @param pointer the URL to be normalized
    * @param currentScope the current resolution scope
    * @return an absolute URL
    */
  private[schema] def normalize(pointer: Pointer, currentScope: Option[Pointer])
                               (implicit createUrl: Pointer => Option[URL] = p => Try { new URL(p.value) }.toOption): Pointer = {

    pointer match {

      case _ if currentScope.exists(_.isAbsolute) && pointer.isAbsolute =>
        pointer

      // pointer starts with #, merge with scope id, possibly dropping a #
      case _ if pointer.isFragment =>
        currentScope.map(_.dropRightHashIfAny.append(pointer)).getOrElse(pointer)

      // make sure we have a # at the end, if applicable, that is
      // the pointer has no # and also does not end with a /
      case _ if pointer.isAbsolute =>
        pointer.withHashAtEnd

      // append pointer to scope id in case the latter ends with a /
      case _ if currentScope.exists(_.isSegment) =>
        currentScope.map(_.append(pointer)).getOrElse(pointer)

      // if all cases failed, try to create a relative reference, e.g.
      // if given http://x.y.z/schema.json# and some.json#/definitions/prop
      // we want the result to be http://x.y.z/some.json#/definitions/prop
      case other =>
        val baseUrl = findBaseUrl(currentScope, createUrl)
        baseUrl.map(url =>
          if (url.isSegment) pointer.withHashAtEnd.prepend(url)
          else pointer.withHashAtEnd.prepend(url.append(`/`))
        ).getOrElse(pointer)
    }
  }

  /**
    * Finds out the actual base URI based on a given resolution scope.
    *
    * @param scope the resolution scope from which to determine the base URL
    * @return the base URL, if any, otherwise None
    */
  private[schema] def findBaseUrl(scope: Option[Pointer], createUrl: Pointer => Option[URL]): Option[Pointer] = {

    def createBaseUrl(url: URL, protocol: String, host: String, port: Int, file: String): Option[URL] = {
      if (url.getHost.nonEmpty) {
        if (url.getPort != -1) {
          createUrl(Pointer(s"$protocol://$host:$port"))
        } else {
          createUrl(Pointer(s"$protocol://$host"))
        }
      } else {
        createUrl(Pointer(s"$protocol://${file.substring(0, file.lastIndexOf("/"))}"))
      }
    }

    val url: Option[URL] = for {
      id       <- scope
      url      <- createUrl(id)
      protocol = url.getProtocol
      host     = url.getHost
      port     = url.getPort
      file     = url.getFile
      baseUrl  <- createBaseUrl(url, protocol, host, port, file)
    } yield baseUrl

    url.map(u => Pointer(u.toString))
  }
}

case class Pointer(value: String) {

  private final val WithProtocol = "^([^:\\/?]+):.+"
  private final val ProtocolPattern = WithProtocol.r.pattern

  def startsWith(str: String) = value.startsWith(str)
  def startsWith(p: Pointer) = value.startsWith(p.value)
  def endsWith(s: String) = value.endsWith(s)

  def isSegment = value.endsWith("/")

  /**
    * Whether the pointer contains any #.
    *
    * @return true, if the pointer contains a # character
    */
  def hasFragment = value.contains("#")
  def hasSegment = value.contains("/")

  def isAnchor = value.startsWith("#") && value.length > 1 && value.charAt(1) != '/'

  /**
    * Whether the pointer is a fragment, i.e. whether it starts with a # character.
    *
    * @return
    */
  def isFragment = value.startsWith("#")

  def isAbsolute = Try { new URI(value) }.map(_.isAbsolute).getOrElse(false)

  // TODO
  def fragments: Option[Pointer] = {
    if (hasFragment) Some(Pointer(value.substring(value.indexOf("#"))))
    else None
  }

  def documentName: Pointer = {
    val hashIdx = value.indexOf("#")
    if (hasFragment) Pointer(value.substring(0, hashIdx)) else this
  }

  def protocol: Option[String] = {
    val matcher = ProtocolPattern.matcher(value)
    matcher.find()
    Try { matcher.group(1).replaceAll("[^A-Za-z]+", "") }.toOption
  }

  def withHashAtStart =
    if (!hasFragment) Pointer(s"#/$value")
    else this

  def length = value.length

  def drop(n: Int) = Pointer(value.drop(n))

  def dropHashAtStart =
    if (value.startsWith("#/")) Pointer(value.substring(math.min(2, value.length)))
    else if (isFragment) Pointer(value.substring(math.min(1, value.length)))
    else this

  def dropRightHashIfAny: Pointer = {
    if (value.endsWith("#")) Pointer(value.dropRight(1)) else Pointer(value)
  }

  def append(otherPointer: Pointer): Pointer = Pointer(value + otherPointer.value)

  def prepend(otherPointer: Pointer): Pointer = Pointer(otherPointer.value + value)

  def withHashAtEnd: Pointer =
    if (!hasFragment && !isSegment) append(Pointers.`#`)
    else this
}

