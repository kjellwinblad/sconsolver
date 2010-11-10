package me.winsh.sconsolver.core

trait SearchMethod {

	def findFirst[R](propagators:Set[Propagator],
					 store:Store, 
					 branching:Branching, 
					 f:(Store)=>R = ((s:Store)=>s)):Option[R]
	
}