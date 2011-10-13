package me.winsh.sconsolver.constraints

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._

trait ArithmeticsConstraints extends Constraint {

  def add(x: Var, y: Var, result: Var): Propagator = {

    checkRequirements(List(x, y, result))

    add(new AddPropagator(x, y, result))
  }

  def sub(x: Var, y: Var, result: Var): Propagator = {

    checkRequirements(List(x, y, result))

    add(new SubPropagator(x, y, result))
  }

  def mult(x: Var, y: Var, result: Var): Propagator = {

    checkRequirements(List(x, y, result))

    add(new MultPropagator(x, y, result))
  }

  def div(x: Var, y: Var, result: Var): Propagator = {

    checkRequirements(List(x, y, result))

    add(new DivPropagator(x, y, result))
  }
}