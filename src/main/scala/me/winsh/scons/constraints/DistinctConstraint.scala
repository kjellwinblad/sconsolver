package me.winsh.scons.constraints

import me.winsh.scons.core._

import me.winsh.scons.propagators._

trait DistinctConstraint  extends Constraint {

	def distinct(vars:Iterable[Var]) = {
		
		checkRequirements(vars)
		
		add(new DistinctPropagator(vars.toList))
	}
	
	def distinct(vars: Var*):Propagator = distinct(vars.toList)
	
	
}