package me.winsh.sconsolver.core

trait ValBranching {

	val branching:((Var,Store)=>(List[Set[Propagator]], Store))
	
}