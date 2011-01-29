package me.winsh.sconsolver.search

import me.winsh.sconsolver.core._

object DepthFirstSearch extends SearchMethod {

  def find[R](
    propagators: List[Propagator],
    store: Store,
    branching: Branching,
    numberOfSolutions: Int = -1,
    f: (List[R], Store) => List[R] = ((l: List[R], s: Store) => s :: l),
    constrain: (Store) => (List[Propagator], List[(Var, Domain)]) = ((s: Store) => (Nil, Nil))): List[R] = {

    case class EnoughSolutionsException(solutions: List[R]) extends Exception
    
    def findInter(
      propagators: List[Propagator],
      store: Store,
      resultSoFar: List[R],
      numberOfSolutions: Int): (List[R], Int, (List[Propagator], List[(Var, Domain)])) = {
    	
      val (newPropagators, newStore1) = Propagate.propagate(propagators, store)

      if (newStore1.failed)
        (resultSoFar, numberOfSolutions, (Nil, Nil))
      else if (newStore1.fixPoint) {
        if (numberOfSolutions == 1)
          throw EnoughSolutionsException(f(resultSoFar, newStore1))
        else {

          val (constrainPropagatos, newVarsWithDomain) = constrain(newStore1)
          
          (f(resultSoFar, newStore1),
            numberOfSolutions - 1,
            (constrainPropagatos, newVarsWithDomain))

        }
      } else {

        val (branches, newStore2) = branching.branches(newPropagators, newStore1)

        def solutionsInBranches(
          constrainPropsSoFar: List[Propagator],
          constrainVarsWithDomainSoFar:List[(Var,Domain)],
          store: Store,
          branches: List[List[Propagator]],
          bSolutionsSoFar: List[R],
          numberOfSolutions: Int): (List[R], Int, (List[Propagator], List[(Var,Domain)])) = branches match {
          case Nil => (bSolutionsSoFar, numberOfSolutions, (constrainPropsSoFar, constrainVarsWithDomainSoFar))
          case bProps :: rest => {

        	val newStore = store.addVars(constrainVarsWithDomainSoFar)
        	  
            val ((solutionsInBranch, newNumberOfSolutions, (constrainProps, constrainVarsWithDomain))) =
              findInter(bProps ::: constrainPropsSoFar ::: newPropagators, newStore, Nil, numberOfSolutions)

            
              
            solutionsInBranches(constrainProps ::: constrainPropsSoFar, constrainVarsWithDomain:::constrainVarsWithDomainSoFar, store, rest, solutionsInBranch ::: bSolutionsSoFar, newNumberOfSolutions)

          }
        }

        solutionsInBranches(Nil, Nil, newStore2, branches, Nil, numberOfSolutions)
      }

    }

    val solutions = try {

      val (solutions, _, _) = findInter(propagators, store, Nil, numberOfSolutions)

      solutions

    } catch {
      case EnoughSolutionsException(solutions) => solutions
    }

    solutions
  }

}