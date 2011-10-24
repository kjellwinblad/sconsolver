package me.winsh.sconsolver.core

trait Constraint {


  def initialStore: Store

  def add(propagator: Propagator): Propagator

  protected def checkRequirements(vars: Var*) {
    //Check that they are in the store
    for (variable <- vars)
      require(initialStore.contains(variable), "All vars in the contstaint need to be added to the store. Use one of the newIntVar() methods to add vars.")
  }

  protected def checkBoolean(vars: Var*) {
    //Check that they are boolean
    for (variable <- vars) {
      val domain = initialStore(variable) 
      require(domain.lessThan(0).isEmpty && domain.greaterThan(1).isEmpty, "A variable that should be boolean contains other values than 0 and 1")
        
      
    }
  }

}