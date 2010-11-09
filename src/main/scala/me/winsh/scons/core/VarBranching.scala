package me.winsh.scons.core

trait VarBranching {

	val branching:((Set[Propagator],Store,ValBranching)=>(List[Set[Propagator]], Store))
	
}