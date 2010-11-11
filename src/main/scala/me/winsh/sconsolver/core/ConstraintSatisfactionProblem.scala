package me.winsh.sconsolver.core

import me.winsh.sconsolver.constraints._
import me.winsh.sconsolver.search.DepthFirstSearch
import me.winsh.sconsolver.branchings.value._
import me.winsh.sconsolver.branchings.variable._

import scala.collection.immutable.HashSet

trait CSPModel extends ConstraintSatisfactionProblemModel[Store]{
	
	val storeToResultFunction:((Store)=>Store) = (s:Store)=>s

	val branching:Branching = Branching(FirstUnassigned, MinValue)
	
	def findFirstSolution:Option[Store] = 
		DepthFirstSearch.findFirstSolution(initialPropagators,
												initialStore,
												branching,
												storeToResultFunction)
} 


trait ConstraintSatisfactionProblemModel[R] 
      extends IntRelationConstraints  with DistinctConstraint{
	
	//Model definition
	//================
	
	private var variableList = List[(Var, Domain)]()
	
	private var propagatorList = List[Propagator]()

	val branching:Branching
	
	def initialStore:Store = Store(variableList.toList)
	
	def initialPropagators:List[Propagator] = propagatorList
	
	val storeToResultFunction:((Store)=>R)
	
	def findFirstSolution:Option[R]
	
	//Domain Specific Language
	//========================
	
	def add(variable:Var, domain:Domain) = {
		 
		variableList = (variable,domain)::variableList
		
		variable
		
	}
	
	def add(propagator:Propagator) = {
		
		propagatorList = propagator::propagatorList
		
		propagator
		
	}
	
	//IntVar declarations
	
	def newIntVar(d:Domain):Var = {
		
		val newVar = Var()
		
		add(newVar, d)
		
		newVar
	}

	def newIntVar():Var = newIntVar(Domain(Int.MaxValue, Int.MinValue))
	
	def newIntVar(min:Int, max:Int):Var = newIntVar(Domain(min, max))
	
	def newIntVar(values:Iterable[Int]):Var = newIntVar(Domain(values.toList))
	
	def newIntVarConstant(value:Int):Var = newIntVar(Domain(value))
	
	def c(value:Int) = newIntVarConstant(value)

	//implicit def range2var(r:Range): Var = newIntVar(Domain(r))

	implicit def int2var(i:Int): Var = c(i)
	
	//BoolVar declarations
	
	def newBoolVar():Var = newIntVar(Domain(0, 1))
	
	def newBoolVarConstant(value:Boolean):Var = newIntVar(Domain(if(true) 1 else 0))
	
	def c(value:Boolean) = newBoolVarConstant(value)
	 
	implicit def boolean2var(value:Boolean): Var = c(value)
	

	//Helper to add constraints
	
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