package me.winsh.sconsolver.branchings.variable

import me.winsh.sconsolver.core._

object FirstUnassigned extends VarBranching {

  def firstUnassignedBranchingFun(propagators: List[Propagator], store: Store, valBrancing: ValBranching) = {

    val unassignedVar = store.firstUnassignedVar

    valBrancing.branches(unassignedVar, store)
  }

  val branches: ((List[Propagator], Store, ValBranching) => (List[List[Propagator]], Store)) = firstUnassignedBranchingFun

}