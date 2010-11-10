package me.winsh.sconsolver.core

trait Constraint{

	protected val ALL_VARS_NEED_TO_BE_IN_THE_STORE_MESSAGE = 
		"All vars in the contstaint need to be added to the store. Use one of the newIntVar() methods to add vars."
	
	def initialStore:Store
	
	def add(propagator:Propagator):Propagator

	protected def checkRequirements(x:Var, y:Var){
		//Check that it is in the store
		require(initialStore.contains(x), ALL_VARS_NEED_TO_BE_IN_THE_STORE_MESSAGE)
		require(initialStore.contains(y), ALL_VARS_NEED_TO_BE_IN_THE_STORE_MESSAGE)
	}
	
	protected def checkRequirements(vars: Iterable[Var]){
		//Check that it is in the store
		for(variable <- vars)	
			require(initialStore.contains(variable), ALL_VARS_NEED_TO_BE_IN_THE_STORE_MESSAGE)

	}
	
}