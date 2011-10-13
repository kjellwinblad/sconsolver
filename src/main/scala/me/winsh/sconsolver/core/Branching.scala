package me.winsh.sconsolver.core

case class Branching(val varBranching: VarBranching, val valBranching: ValBranching) {

  def branches(propagators: List[Propagator], store: Store) =
    varBranching.branches(propagators, store, valBranching)

}