package me.winsh.scons.core

import me.winsh.scons.constraints._
import me.winsh.scons.search.DepthFirstSearchFindFirst
import me.winsh.scons.branchings.value._
import me.winsh.scons.branchings.variable._

import scala.collection.immutable.HashSet

trait SimpleCSPBase extends ConstraintSatisfactionProblemBase[Store]{
	
	val storeToResultFunction:((Store)=>Store) = (s:Store)=>s
	
} 

trait CSPBase[R] extends ConstraintSatisfactionProblemBase[R]

trait ConstraintSatisfactionProblemBase[R] 
      extends IntRelationConstraints  with DistinctConstraint{
	
	private var variableList = List[(Var, Domain)]()
	
	private var propagatorList = List[Propagator]()

	private var branching = Branching(FirstUnassigned, MinValue)
	
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
	
	def newIntVar(d:Domain):Var = {
		
		val newVar = Var()
		
		add(newVar, d)
		
		newVar
	}

	def newIntVar():Var = {
		
		val newVar = Var()
		
		add(newVar, Domain(Int.MaxValue, Int.MinValue))
		
		newVar
	}
	
	def newIntVar(domainValues: Iterable[Int]):Var = newIntVar(Domain(domainValues.toList))
	
	def newIntVarMinMax(min: Int, max: Int):Var = newIntVar(Domain(min,max))
	
	def newIntVar(domainValues: Int*):Var = newIntVar(Domain(domainValues.toList))
	
	val storeToResultFunction:((Store)=>R)
	
	def findFirstSolution:Option[R] = 
		DepthFirstSearchFindFirst.findFirst[R](initialPropagatorSet,
												initialStore,
												branching,
												storeToResultFunction)
	 
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
	
	implicit def var2intVar(v:Var): IntVar = {
		
		new IntVar{
			val id = v.id
			
			def <=(that:IntVar):Propagator = lessThanOrEqual(this, that)
			def >=(that:IntVar):Propagator = greaterThanOrEqual(this, that)
			def <(that:IntVar):Propagator = lessThan(this, that)
			def >(that:IntVar):Propagator = greaterThan(this, that)
			
			def ===(that:IntVar):Propagator = equal(this, that)
			def !==(that:IntVar):Propagator = notEqual(this, that)
		}
		
	}
	
}