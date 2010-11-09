package me.winsh.scons.search

import me.winsh.scons.core._

object DepthFirstSearchFindFirst extends SearchMethod {

  def findFirst[R](propagators: Set[Propagator],
    store: Store,
    branching: Branching,
    f: (Store) => R = ((s: Store) => s)) = {

    def findFirstInter(propagators: Set[Propagator], store: Store): Option[Store] ={

      val (newPropagators, newStore1) = Propagate.propagate(propagators, store)

      if (newStore1.failed)
        None
      else if (newStore1.fixPoint)
        Some(newStore1)
      else {

    	  val (branches, newStore2) = branching.varBranching.branching(newPropagators, newStore1, branching.valBranching)

    	  def findFirstSolutionInBranches(branches:List[Set[Propagator]], 
    	 		  						  solution:Option[Store]):Option[Store] = branches match {
    	 	  case Nil => solution
    	 	  case newProps::rest =>
    	 	  findFirstInter(newPropagators.union(newProps), newStore2) match{
    	 	 	  case Some(x)=>findFirstSolutionInBranches(Nil, Some(x))
    	 	 	  case None => findFirstSolutionInBranches(rest, None)
    	 	  }
    	 	  
    	  }
    	  
    	  findFirstSolutionInBranches(branches, None)
      }

    }
    
    findFirstInter(propagators, store) match{
    	case Some(s) => Some(f(s))
    	case None => None
    }

  }

}