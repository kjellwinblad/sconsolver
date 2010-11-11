package me.winsh.sconsolver.search

import me.winsh.sconsolver.core._

object DepthFirstSearch extends SearchMethod {

  def findFirstSolution[R](
    propagators: List[Propagator],
    store: Store,
    branching: Branching,
    f: (Store) => R = ((s: Store) => s)) = {

    def findFirstInter(propagators: List[Propagator], store: Store): Option[Store] = {

      val (newPropagators, newStore1) = Propagate.propagate(propagators, store)

      if (newStore1.failed)
        None
      else if (newStore1.fixPoint)
        Some(newStore1)
      else {
 
        val (branches, newStore2) = branching.varBranching.branches(newPropagators, newStore1, branching.valBranching)

        def findFirstSolutionInBranches(branches: List[List[Propagator]],
          solution: Option[Store]): Option[Store] = branches match {
          case Nil => solution
          case newProps :: rest =>
            findFirstInter(newProps:::newPropagators, newStore2) match { 
              case Some(x) => findFirstSolutionInBranches(Nil, Some(x))
              case None => findFirstSolutionInBranches(rest, None)
            }

        }

        findFirstSolutionInBranches(branches, None)
      }

    }

    findFirstInter(propagators, store) match {
      case Some(s) => Some(f(s))
      case None => None
    }

  }
  
  	def findBestSolution[R](propagators:List[Propagator],
					 store:Store, 
					 branching:Branching,
					 solutionEvaluationProps:(Store)=>List[Propagator],
					 f:(Store)=>R = ((s:Store)=>s)):Option[R] =Some(f(store))
	
	def findAllSolutions[R](propagators:List[Propagator],
					 store:Store, 
					 branching:Branching, 
					 f:(List[R], Store)=>List[R] = ((l:List[R], s:Store)=>s::l)):List[R] = f(Nil,store)
  

}