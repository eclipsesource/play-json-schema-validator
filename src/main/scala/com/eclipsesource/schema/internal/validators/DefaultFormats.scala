package com.eclipsesource.schema.internal.validators

import java.time.OffsetDateTime
import java.time.format.DateTimeParseException
import java.util.regex.Pattern

import com.eclipsesource.schema.SchemaFormat
import com.google.common.net.InetAddresses
import io.mola.galimatias.URL
import jdk.nashorn.internal.runtime.regexp.RegExpFactory
import play.api.libs.json.{JsNumber, JsString, JsValue}

import scala.util.Try
import scalaz.\/

object DefaultFormats {

  val formats: Map[String, SchemaFormat] = Map(
    UriFormat.name      -> UriFormat,
    HostnameFormat.name -> HostnameFormat,
    EmailFormat.name    -> EmailFormat,
    IPv4Format.name     -> IPv4Format,
    IPv6Format.name     -> IPv6Format,
    DatetimeFormat.name -> DatetimeFormat,
    UuidFormat.name     -> UuidFormat,
    RegexFormat.name    -> RegexFormat,
    Int32Format.name    -> Int32Format,
    Int64Format.name    -> Int64Format
  )

  def get(format: String): Option[SchemaFormat] = formats.get(format)

  object DatetimeFormat extends SchemaFormat {

    override def name: String = "date-time"

    // http://hg.mozilla.org/comm-central/rev/031732472726
    override def validate(json: JsValue): Boolean = json match {
      case JsString(dateTime) =>
        \/.fromTryCatchThrowable[OffsetDateTime, DateTimeParseException](OffsetDateTime.parse(dateTime))
          .fold(_ => false, _ => true)
      case _ => false
    }
  }

  object IPv4Format extends SchemaFormat {
    override def name: String = "ipv4"
    override def validate(json: JsValue): Boolean = json match {
      case JsString(ipv4) => InetAddresses.isInetAddress(ipv4)
      case _ => false
    }
  }

  object IPv6Format extends SchemaFormat {
    override def name: String = "ipv6"
    override def validate(json: JsValue): Boolean = json match {
      case JsString(ipv6) => InetAddresses.isInetAddress(ipv6)
      case _ => false
    }
  }

  object UriFormat extends SchemaFormat {
    override def name: String = "uri"
    override def validate(json: JsValue): Boolean = json match {
      case JsString(uri) => Try { URL.parse(uri) }.isSuccess
      case _ => false
    }
  }

  object HostnameFormat extends SchemaFormat {

    // modified version of http://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
    // http://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address

    val Hostname ="^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9]))*$"
    private val CompiledHostnamePattern = Pattern.compile(Hostname)

    override def name: String = "hostname"
    override def validate(json: JsValue): Boolean = json match {
      case JsString(host) => CompiledHostnamePattern.matcher(host).find || InetAddresses.isInetAddress(host)
      case _ => false
    }
  }

  object EmailFormat extends SchemaFormat {
    val Email = "^([a-zA-Z0-9.!#$%&’'*+/=?^_`{|}~-]+)@([a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*)$"
    private val CompiledEmailPattern = Pattern.compile(Email)

    override def name: String = "email"
    override def validate(json: JsValue): Boolean = json match {
      case JsString(email) => CompiledEmailPattern.matcher(email).find
      case _ => false
    }
  }

  object UuidFormat extends SchemaFormat {
    val UuidPattern: Pattern = Pattern.compile("[0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12}")
    override def name: String = "uuid"
    override def validate(json: JsValue): Boolean = json match{
      case JsString(uuid) => UuidPattern.matcher(uuid).find
      case _ => false
    }
  }

  object RegexFormat extends SchemaFormat {
    override def name: String = "regex"
    override def validate(json: JsValue): Boolean = json match {
      case JsString(regex) => Try {
        new RegExpFactory().compile(regex, "")
      }.isSuccess
      case _ => false
    }
  }

  object Int32Format extends SchemaFormat {
    /**
      * The name of the format.
      *
      * @return the format name
      */
    override def name: String = "int32"

    /**
      * Check whether the given value conforms to this format.
      *
      * @param json the JSON value to be checked
      * @return whether the JSON value conforms to this format
      */
    override def validate(json: JsValue): Boolean = json match {
      case JsNumber(number) => number.isValidInt
      case _ => false
    }
}

  object Int64Format extends SchemaFormat {
    /**
      * The name of the format.
      *
      * @return the format name
      */
    override def name: String = "int64"

    /**
      * Check whether the given value conforms to this format.
      *
      * @param json the JSON value to be checked
      * @return whether the JSON value conforms to this format
      */
    override def validate(json: JsValue): Boolean = json match {
      case JsNumber(number) => number.isValidLong
      case _ => false
    }
}
}
