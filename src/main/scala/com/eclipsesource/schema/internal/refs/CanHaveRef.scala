package com.eclipsesource.schema.internal.refs

import play.api.data.validation.ValidationError


/**
  * A typeclass that determines whether a value of the given
  * type can contain references
  *
  * @tparam A the type that can contain references
  */
trait CanHaveRef[A] {

  /**
    * Resolve the fragment against the given value. A fragment
    * is a single identifier like a property name.
    *
    * @param a the value
    * @param fragment the fragment to be resolved
    * @return a right-based Either containg the result
    */
  def resolve(a: A, fragment: String): Either[ValidationError, A]

  /**
    * Whether the given value has an id field which can alter resolution scope.
    *
    * @param a the instance to be checked
    * @return true, if the given instance has an id field, false otherwise
    */
  def refinesScope(a: A): Boolean = findScopeRefinement(a).isDefined

  /**
    * Tries to find an id field which refines the resolution scope.
    *
    * @param a the instance to be checked
    * @return true, if the given instance has an id field, false otherwise
    */
  def findScopeRefinement(a: A): Option[Pointer]

  /**
    *
    * @param a
    * @return
    */
  def anchorsOf(a: A): Map[Pointer, A]

  /**
    * Tries to find a resolvable instance within the given value.
    *
    * @param a the value
    * @return an Option containing the field name and value, if any ref has been found
    */
  def findRef(a: A): Option[Pointer]
}
