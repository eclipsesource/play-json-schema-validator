package com.eclipsesource.schema.internal

import com.eclipsesource.schema.SchemaType
import com.osinka.i18n.Lang
import play.api.libs.json.{JsValue, Json, JsonValidationError, Writes}

package object refs {

  implicit class ResolveResultExtensionOps(resolvedResult: ResolvedResult) {
    def toJson(implicit writes: Writes[SchemaType]): JsValue =
      Json.toJson(resolvedResult.resolved)
  }

  implicit class SchemaRefResolverExtensionOps(resolver: SchemaRefResolver) {
    def resolveFromRoot(ref: String, scope: SchemaResolutionScope)
                       (implicit lang: Lang = Lang.Default): Either[JsonValidationError, ResolvedResult] = {
      resolver.resolve(scope.documentRoot, Ref(ref), scope).toEither
    }
  }
}
