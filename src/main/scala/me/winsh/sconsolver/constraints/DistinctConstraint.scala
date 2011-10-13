package me.winsh.sconsolver.constraints

import me.winsh.sconsolver.core._

import me.winsh.sconsolver.propagators._

trait DistinctConstraint extends Constraint {

  def distinct(vars: Iterable[Var]) = {

    checkRequirements(vars)

    add(new DistinctPropagator(vars.toList))
  }

  def distinct(vars: Var*): Propagator = distinct(vars.toList)

}