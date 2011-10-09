package me.winsh.sconsolver.core

import me.winsh.sconsolver.constraints._
import me.winsh.sconsolver.search.DepthFirstSearch
import me.winsh.sconsolver.branchings.value._
import me.winsh.sconsolver.branchings.variable._

import scala.collection.immutable.HashSet

trait BasicCSPModel extends ConstraintSatisfactionProblemModel[Store] {

  def solutionStoreToSolution(s: Store) = s


}

trait CSPModel[R] extends ConstraintSatisfactionProblemModel[R] {


}

trait ConstraintSatisfactionProblemModel[R]
  extends IntRelationConstraints with DistinctConstraint with ArithmeticsConstraints{


  //Domain Specific Language State
  //==============================

  private var variableList = List[(Var, Domain)]()

  private var propagatorList = List[Propagator]()
	
  //Configurations
  //==============

  def solutionStoreToSolution(s:Store):R

  var branching: Branching = Branching(FirstUnassigned, MinValue)
  
  var searchMethod:SearchMethod = DepthFirstSearch

  def constrain(s:Store):Unit = require(false,"The constrain(s:Store) method need to be defined in the model in order to call findBestSolution")
  
  //Convenience Methods
  //===================

  def initialStore: Store = Store(variableList.toList)

  def initialPropagators: List[Propagator] = propagatorList

  //Search
  //======

  def findFirstSolution: Option[R] =
    searchMethod.findFirstSolution(
      initialPropagators,
      initialStore,
      branching,
      solutionStoreToSolution)
      
  def findAllSolutions: List[R] =
    searchMethod.findAllSolutions(
      initialPropagators,
      initialStore,
      branching,
      ((l: List[R], s: Store) => (solutionStoreToSolution(s) :: l)))

  def findBestSolution: Option[R] =
    searchMethod.findBestSolution(
      initialPropagators,
      initialStore,
      branching,
      constrainTransformation,
      solutionStoreToSolution)
      
  //Domain Specific Language
  //========================

  def add(variable: Var, domain: Domain) = {

    variableList = (variable, domain) :: variableList

    variable

  }

  def add(propagator: Propagator) = {

    propagatorList = propagator :: propagatorList

    propagator

  }
  
  private def constrainTransformation(s:Store):(List[Propagator], List[(Var,Domain)]) = {
    	
	  val varListBeforeConstrain = this.variableList 
	  
	  val propagatorListBeforeConstrain = this.propagatorList
	  
	  constrain(s)
	  
	  val varListToReturn = this.variableList  filterNot (varListBeforeConstrain contains)
	  
	  val propagatorListToReturn = this.propagatorList filterNot (propagatorListBeforeConstrain contains)
	  
	  //Reset
	  
	  this.variableList = varListBeforeConstrain
	  
	  this.propagatorList = propagatorListBeforeConstrain
	  
	  (propagatorListToReturn, varListToReturn)
	   
  }

    //IntVar declarations

  def newIntVar(d: Domain): Var = {

    val newVar = Var()

    add(newVar, d)

    newVar
  }
  
  def newIntVar(): Var = newIntVar(-10000000, 10000000)
  
  def newIntVar(min: Int, max: Int): Var = newIntVar(Domain(min, max))

  def newIntVar(values: Iterable[Int]): Var = newIntVar(Domain(values.toList))

  def newIntVarConstant(value: Int): Var = newIntVar(Domain(value))

  def c(value: Int) = newIntVarConstant(value)

  //implicit def range2var(r:Range): Var = newIntVar(Domain(r))

  implicit def int2var(i: Int): Var = c(i)

  //BoolVar declarations

  def newBoolVar(): Var = newIntVar(Domain(0, 1))

  def newBoolVarConstant(value: Boolean): Var = newIntVar(Domain(if (true) 1 else 0))

  def c(value: Boolean) = newBoolVarConstant(value)

  implicit def boolean2var(value: Boolean): Var = c(value)

  //Helper to add constraints

  implicit def var2intVar(v: Var): IntVar = {

    new IntVar {
      val id = v.id

      def <=(that: IntVar): Propagator = lessThanOrEqual(this, that)
      def >=(that: IntVar): Propagator = greaterThanOrEqual(this, that)
      def <(that: IntVar): Propagator = lessThan(this, that)
      def >(that: IntVar): Propagator = greaterThan(this, that)

      def ===(that: IntVar): Propagator = equal(this, that)
      def !==(that: IntVar): Propagator = notEqual(this, that)
    }

  }

}