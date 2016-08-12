package com.eclipsesource.schema.internal.refs

import java.net.URI

import scala.util.Try

object Pointers {
  val `#` = Pointer("#")
  val `/` = Pointer("/")
}

case class Pointer(value: String) {

  private final val WithProtocol = "^([^:\\/?]+):.+"
  private final val ProtocolPattern = WithProtocol.r.pattern

  def isSegment = value.endsWith("/")

  /**
    * Whether the pointer contains any #.
    * @return true, if the pointer contains a # character
    */
  def hasFragment = value.contains("#")
  def hasSegment = value.contains("/")
  /**
    * Whether the pointer is a fragment, i.e. whether it starts with a # character.
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

