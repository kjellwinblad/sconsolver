package me.winsh.sconsolver.core

trait VarBranching {

	val branching:((Set[Propagator],Store,ValBranching)=>(List[Set[Propagator]], Store))
	
}