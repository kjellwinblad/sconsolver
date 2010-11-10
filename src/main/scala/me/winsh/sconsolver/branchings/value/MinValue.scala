package me.winsh.sconsolver.branchings.value

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._

object MinValue extends ValBranching{

		def minValueBranchingFun(variable:Var,s:Store) = {
			
			val domain = s(variable)
			
			val brancingVar = Var()

			val brancingVarDomain = Domain(domain.head)

			val newS = s(brancingVar, brancingVarDomain)

			val branchingPropagatorSet1 = Set[Propagator](new EqualsPropagator(variable,brancingVar))
			
			val branchingPropagatorSet2 = Set[Propagator](new NotEqualsPropagator(variable,brancingVar))
			
			(List(branchingPropagatorSet1, branchingPropagatorSet2), newS)
		}
	
		val branching:((Var,Store)=>(List[Set[Propagator]], Store)) = minValueBranchingFun
	
}