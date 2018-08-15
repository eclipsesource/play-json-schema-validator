package com.eclipsesource.schema.internal.refs

import org.specs2.mutable.Specification

/**
  * Examples from http://json-schema.org/draft-04/json-schema-core.html#rfc.section.7.2.2
  */
class RefsSpec extends Specification {
  "Refs" should {
    "should define resolution scopes" in {

      Refs.mergeRefs(
        Ref("#foo"),
        Some(Ref("#bar"))
      ) must beEqualTo(Ref("#foo"))

      Refs.mergeRefs(
        Ref("#"), Some(Ref("http://x.y.z/rootschema.json#"))
      ) must beEqualTo(Ref("http://x.y.z/rootschema.json#"))

      Refs.mergeRefs(
        Ref("#foo"), Some(Ref("http://x.y.z/rootschema.json#"))
      ) must beEqualTo(Ref("http://x.y.z/rootschema.json#foo"))

      Refs.mergeRefs(
        Ref("otherschema.json"), Some(Ref("http://x.y.z/rootschema.json#"))
      ) must beEqualTo(Ref("http://x.y.z/otherschema.json#"))

      Refs.mergeRefs(
        Ref("#bar"), Some(Ref("http://x.y.z/otherschema.json"))
      ) must beEqualTo(Ref("http://x.y.z/otherschema.json#bar"))

      Refs.mergeRefs(
        Ref("t/inner.json#a"), Some(Ref("http://x.y.z/rootschema.json#"))
      ) must beEqualTo(Ref("http://x.y.z/t/inner.json#a"))

      Refs.mergeRefs(
        Ref("some://where.else/completely#"), Some(Ref("http://x.y.z/rootschema.json#"))
      ) must beEqualTo(Ref("some://where.else/completely#"))
    }
  }
}
