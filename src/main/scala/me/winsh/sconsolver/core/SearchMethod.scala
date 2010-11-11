package me.winsh.sconsolver.core

trait SearchMethod {

	def findFirstSolution[R](propagators:List[Propagator],
					 store:Store, 
					 branching:Branching, 
					 f:(Store)=>R = ((s:Store)=>s)):Option[R]

	def findBestSolution[R](propagators:List[Propagator],
					 store:Store, 
					 branching:Branching,
					 solutionEvaluationProps:(Store)=>List[Propagator],
					 f:(Store)=>R = ((s:Store)=>s)):Option[R]
	
	def findAllSolutions[R](propagators:List[Propagator],
					 store:Store, 
					 branching:Branching, 
					 f:(List[R], Store)=>List[R] = ((l:List[R], s:Store)=>s::l)):List[R]
}