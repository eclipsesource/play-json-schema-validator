package com.eclipsesource.schema.internal.validators

import java.util.regex.Pattern

import com.google.common.net.InetAddresses

object Formats {

  trait Format {
    def name: String
    def validate(s: String): Boolean
  }

  private val formats: Map[String, Format] = Map(
    UrlFormat.name      -> UrlFormat,
    HostnameFormat.name -> HostnameFormat,
    EmailFormat.name    -> EmailFormat,
    IPv4Format.name     -> IPv4Format,
    IPv6Format.name     -> IPv6Format,
    DatetimeFormat.name -> DatetimeFormat
  )

  def get(format: String): Option[Format] = formats.get(format)

  object DatetimeFormat extends Format {
    private val CompiledDatetimePattern = Pattern.compile(
      "^([0-9]{4})-([0-9]{2})-([0-9]{2})([Tt]([0-9]{2}):([0-9]{2}):([0-9]{2})(\\.[0-9]+)?)?(([Zz]|([+-])([0-9]{2}):([0-9]{2})))?"
    )
    override def name: String = "date-time"

    // http://hg.mozilla.org/comm-central/rev/031732472726
    override def validate(dateTime: String): Boolean = CompiledDatetimePattern.matcher(dateTime).find
  }

  object IPv4Format extends Format {
    override def name: String = "ipv4"
    override def validate(ipv4: String): Boolean = InetAddresses.isInetAddress(ipv4)
  }

  object IPv6Format extends Format {
    override def name: String = "ipv6"
    override def validate(ipv6: String): Boolean = InetAddresses.isInetAddress(ipv6)
  }

  object UrlFormat extends Format {

    // modified version of
    // http://codereview.stackexchange.com/questions/78768/regex-to-parse-uris-for-their-correctness-according-to-rfc-3986

    private val Scheme = "[A-Za-z][A-Za-z0-9+.-]*:"
    private val AuthoritativeDeclaration = "/{2}"
    private val UserInfo = "(?:[\\w.~-]|%[\\w^_]{2})+(?::(?:[\\w.~-]|%[A-Fa-f0-9]{2})+)?@"
    private val Host = "(?:[\\w^_](?:[A-Za-z0-9-]*[\\w^_])?\\.){1,126}[\\w^_](?:[A-Za-z0-9-]*[\\w^_])?"
    private val Port = ":\\d+"
    private val Path = "/(?:[\\w.-~]|%[\\w^_]{2})*"
    private val Query = "\\?(?:[\\w.~-]+(?:=(?:[\\w+.~-]|%[\\w^_]{2})+)?)(?:[&|;][\\w.~-]+(?:=(?:[A-Za-z0-9-._~+]|%[\\w^_]{2})+)?)*"

    private val UrlRegex = "(?:" + Scheme + AuthoritativeDeclaration + ")?(?:" + UserInfo + ")?" + Host + "(?:" + Port + ")?(?:" + Path + ")*(?:" + Query + ")?"
    private val CompiledPattern = Pattern.compile(UrlRegex)

    override def name: String = "uri"
    override def validate(uri: String): Boolean = CompiledPattern.matcher(uri).find()
  }

  object HostnameFormat extends Format {

    // modified version of http://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
    // http://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address

    val Hostname ="^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9]))*$"
    private val CompiledHostnamePattern = Pattern.compile(Hostname)

    override def name: String = "hostname"
    override def validate(host: String): Boolean =
      CompiledHostnamePattern.matcher(host).find || InetAddresses.isInetAddress(host)
  }

  object EmailFormat extends Format {

    // http://stackoverflow.com/questions/201323/using-a-regular-expression-to-validate-an-email-address
    private val CompiledEmailPattern = Pattern.compile("^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$")

    override def name: String = "email"
    override def validate(email: String): Boolean = CompiledEmailPattern.matcher(email).find
  }

}
