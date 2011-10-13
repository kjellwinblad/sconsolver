package me.winsh.sconsolver.core

import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap

trait Store {

  val variableToDomainMap: Map[Var, Domain]

  def apply(variable: Var) = getDomain(variable)

  def apply(variable: Var, domain: Domain): Store

  def contains(variable: Var): Boolean

  def getDomain(variable: Var): Domain

  def changedVars(that: Store): Set[Var]

  def addVars(varsWithDomain: List[(Var, Domain)]): Store

  def firstUnassignedVar: Var

  def failed: Boolean

  def fixPoint: Boolean

}

object Store {

  def apply(varDomainList: List[(Var, Domain)]): Store = new StoreImpl(HashMap[Var, Domain]() ++ varDomainList)

  private class StoreImpl(varDomainMap: HashMap[Var, Domain]) extends Store {

    val variableToDomainMap: HashMap[Var, Domain] = varDomainMap

    def apply(variable: Var, domain: Domain): Store =
      new StoreImpl(variableToDomainMap.updated(variable, domain))

    def getDomain(variable: Var) = variableToDomainMap.get(variable) match {
      case None => throw new Exception("Can not find variable with id " + variable.id + " in the store.\n" + this)
      case Some(domain) => domain
    }

    def contains(variable: Var) = variableToDomainMap.contains(variable)

    def changedVars(that: Store) = {

      require(this.variableToDomainMap.size == that.variableToDomainMap.size, "The stores need to have the same variables.")

      HashSet[Var]() ++ variableToDomainMap.filter((vd) => that(vd._1) != vd._2).map(_._1)

    }

    def firstUnassignedVar = {

      val op = variableToDomainMap.find((varDomain) => {

        val (variable, domain) = varDomain

        if (domain.fixPoint || domain.failed)
          false
        else
          true
      })

      op match {
        case Some(x) => x._1
        case None => variableToDomainMap.head._1
      }
    }

    def addVars(varsWithDomain: List[(Var, Domain)]) = varsWithDomain match {
      case Nil => this
      case varList => new StoreImpl(variableToDomainMap ++ varsWithDomain)
    }

    def failed = variableToDomainMap.exists((varDomain) => {

      val (_, domain) = varDomain

      domain.failed
    })

    def fixPoint = variableToDomainMap.forall((varDomain) => {

      val (_, domain) = varDomain

      domain.fixPoint
    })

    override def equals(that: Any) = that.asInstanceOf[Store].variableToDomainMap == this.variableToDomainMap

    override def toString = "Store(" + variableToDomainMap.toList.
      sortWith((a, b) => (a._1.id < b._1.id)).
      map(e => (e._1 + "->" + e._2)).mkString(", ") + ")"

  }

}