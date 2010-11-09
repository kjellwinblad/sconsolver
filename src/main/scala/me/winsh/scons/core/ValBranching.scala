package me.winsh.scons.core

trait ValBranching {

	val branching:((Var,Store)=>(List[Set[Propagator]], Store))
	
}