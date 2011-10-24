package me.winsh.sconsolver.constraints

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._

trait IntRelationConstraints extends Constraint {

  def lessThanOrEqual(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new LessThanOrEqualPropagator(x, y))
  }

  def isLessThanOrEqual(x: Var, y: Var, result: Var) = {
    checkRequirements(x, y, result)
    checkBoolean(result)
    add(new LessThanAndLessThanOrEqualResultPropagator(x, y, result, lessThan = false))
  }

  def greaterThanOrEqual(x: Var, y: Var) = lessThanOrEqual(y, x)

  def isGreaterThanOrEqual(x: Var, y: Var, result: Var) = isLessThanOrEqual(y, x, result)

  def lessThan(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new LessThanPropagator(x, y))
  }

  def isLessThan(x: Var, y: Var, result: Var) = {
    checkRequirements(x, y, result)
    checkBoolean(result)
    add(new LessThanAndLessThanOrEqualResultPropagator(x, y, result, lessThan = true))
  }

  def greaterThan(x: Var, y: Var) = lessThan(y, x)

  def isGreaterThan(x: Var, y: Var, result: Var) = isLessThan(y, x, result)

  def equal(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new EqualPropagator(x, y))
  }

  def isEqual(x: Var, y: Var, result: Var) = {
    checkRequirements(x, y, result)
    checkBoolean(result)
    add(new EqualResultPropagator(x, y, result))
  }

  def notEqual(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new NotEqualPropagator(x, y))
  }

  def isNotEqual(x: Var, y: Var, result: Var) = {
    checkRequirements(x, y, result)
    checkBoolean(result)
    add(new NotEqualResultPropagator(x, y, result))
  }

}