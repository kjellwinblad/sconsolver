package me.winsh.sconsolver.constraints

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._



trait LogicalConstraints extends Constraint {

  def and(x: Var, y: Var) = {
    checkRequirements(x, y)
    checkBoolean(x,y)
    add(new AndPropagator(x, y))
  }
  
  def and(x: Var, y: Var, result: Var) = {
    checkRequirements(x, y, result)
    checkBoolean(x, y, result)
    add(new AndResultPropagator(x, y, result))
  }

  def xor(x: Var, y: Var) = {
    checkRequirements(x, y)
    checkBoolean(x,y)
    throw new UnsupportedOperationException()
    //add(new XorPropagator(x, y))
  }
  
  def xor(x: Var, y: Var, result: Var) = {
    checkRequirements(x, y, result)
    checkBoolean(x, y, result)
    throw new UnsupportedOperationException()
    //add(new XorResultPropagator(x, y, result))
  }
  
  def or(x: Var, y: Var) = {
    checkRequirements(x, y)
    checkBoolean(x, y)
    add(new OrPropagator(x, y))
  }
  
  def or(x: Var, y: Var, result: Var) = {
    checkRequirements(x, y, result)
    checkBoolean(x, y, result)
    add(new OrResultPropagator(x, y, result))
  }

  def not(x: Var) = {
    checkRequirements(x)
    checkBoolean(x)
    throw new UnsupportedOperationException()
    //add(new NotPropagator(x, y, result))
  }
  
  def not(x: Var, result: Var) = {
    checkRequirements(x, result)
    checkBoolean(x, result)
    throw new UnsupportedOperationException()
    //add(new NotResultPropagator(x, y, result))
  }

}