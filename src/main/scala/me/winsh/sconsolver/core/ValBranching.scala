package me.winsh.sconsolver.core

trait ValBranching {

  val branches: ((Var, Store) => (List[List[Propagator]], Store))

}