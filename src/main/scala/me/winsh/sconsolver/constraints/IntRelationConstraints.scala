package me.winsh.sconsolver.constraints

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._
import me.winsh.sconsolver.propagators.IsEqualsPropagator

trait IntRelationConstraints extends Constraint {

  def lessThanOrEqual(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new LessThanOrEqualPropagator(x, y))
  }

  def isLessThanOrEqual(x: Var, y: Var, z:Var) = {
    checkRequirements(List(x,y,z))
    add(new IsLessThanAndLessThanOrEqualPropagator(x, y, z, lessThan=false))
  }
  
  def greaterThanOrEqual(x: Var, y: Var) = lessThanOrEqual(y,x)

  def isGreaterThanOrEqual(x: Var, y: Var, z:Var) = isLessThanOrEqual(y,x,z)
  
  
  def lessThan(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new LessThanPropagator(x, y))
  }

  def isLessThan(x: Var, y: Var, z:Var) = {
    checkRequirements(List(x,y,z))
    add(new IsLessThanAndLessThanOrEqualPropagator(x, y, z, lessThan=true))
  }
  
  def greaterThan(x: Var, y: Var) = lessThan(y,x)

  def isGreaterThan(x: Var, y: Var, z: Var) = isLessThan(y,x,z)
  
  def equal(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new EqualsPropagator(x, y))
  }
  
  def isEqual(x: Var, y: Var, z:Var) = {
    checkRequirements(List(x, y, z))
    add(new IsEqualsPropagator(x, y, z))
  }
  
  def notEqual(x: Var, y: Var) = {
    checkRequirements(x, y)
    add(new NotEqualsPropagator(x, y))
  }
  
  def isNotEqual(x: Var, y: Var, z:Var) = {
    checkRequirements(List(x, y, z))
    add(new IsNotEqualsPropagator(x, y, z))
  }

}