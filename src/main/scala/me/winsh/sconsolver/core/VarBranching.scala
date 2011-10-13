package me.winsh.sconsolver.core

trait VarBranching {

  val branches: ((List[Propagator], Store, ValBranching) => (List[List[Propagator]], Store))

}