package me.winsh.scons.constraints

import me.winsh.scons.core._

import me.winsh.scons.propagators.LessThanOrEqualPropagator

trait LessThanOrEqualConstraint extends Constraint {
	
	def lessThanOrEqual(x:Var, y:Var) = {
		
		//Check that it is in the store
		
		
		
		add(new LessThanOrEqualPropagator(x,y))
	}
	
}