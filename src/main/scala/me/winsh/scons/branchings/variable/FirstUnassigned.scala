package me.winsh.scons.branchings.variable

import me.winsh.scons.core._

object FirstUnassigned extends VarBranching{

	def firstUnassignedBranchingFun(propagators:Set[Propagator],store:Store,valBrancing:ValBranching) ={
		
		val unassignedVar = store.firstUnassignedVar
		
		valBrancing.branching(unassignedVar,store)
	}
	
	val branching:((Set[Propagator],Store,ValBranching)=>(List[Set[Propagator]], Store)) = firstUnassignedBranchingFun

}