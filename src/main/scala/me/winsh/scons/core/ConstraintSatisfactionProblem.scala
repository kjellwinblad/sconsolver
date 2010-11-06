package me.winsh.scons.core

import me.winsh.scons.constraints._

import scala.collection.immutable.HashSet

trait CSP extends ConstraintSatisfactionProblem

trait ConstraintSatisfactionProblem extends LessThanOrEqualConstraint {
 
	private var variableList = List[(Var, Domain)]()
	
	private var propagatorList = List[Propagator]()

	def initialStore:Store = Store(variableList.toList)
	
	def initialPropagatorSet:Set[Propagator] = HashSet[Propagator]() ++ propagatorList
	
	def add(variable:Var, domain:Domain) = {
		 
		variableList = (variable,domain)::variableList
		
		variable
		
	}
	
	def add(propagator:Propagator) = {
		
		propagatorList = propagator::propagatorList
		
		propagator
		
	}	
	

	def solve:Store = initialStore
	 
	implicit def range2var(r:Range): Var = {
		
		val newVar = Var()
		
		add(newVar, Domain(r))
		
		newVar
		
	}

	implicit def int2var(i:Int): Var = {
		
		val newVar = Var()
		
		add(newVar, Domain(i))
		
		newVar
		
	}
	
}