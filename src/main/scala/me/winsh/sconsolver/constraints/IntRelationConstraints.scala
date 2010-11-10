package me.winsh.sconsolver.constraints

import me.winsh.sconsolver.core._

import me.winsh.sconsolver.propagators._

trait IntRelationConstraints extends Constraint {
	
	 
	
	def lessThanOrEqual(x:Var, y:Var) = {
		
		checkRequirements(x, y)
		
		add(new LessThanOrEqualPropagator(x,y))
	}
	
	def greaterThanOrEqual(x:Var, y:Var) = {
		
		checkRequirements(y, x)
		
		add(new LessThanOrEqualPropagator(y,x))
	}
	
	def lessThan(x:Var, y:Var) = {
		
		checkRequirements(x, y)
		
		add(new LessThanPropagator(x,y))
	}
	
	def greaterThan(x:Var, y:Var) = {
		
		checkRequirements(y, x)
		 
		add(new LessThanPropagator(y,x))
	}

	def equal(x:Var, y:Var) = {

		checkRequirements(x, y)
		
		add(new EqualsPropagator(x,y))
	}
	
	def notEqual(x:Var, y:Var) = {
		
		checkRequirements(x, y)
		
		add(new NotEqualsPropagator(x,y))
	}
	

	

}