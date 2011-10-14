package me.winsh.sconsolver.branchings.variable
import me.winsh.sconsolver.core._


object Random extends VarBranching {

  def randomVarBranchingFun(propagators: List[Propagator], store: Store, valBrancing: ValBranching) = {

    val randomVars = store.unassignedVars.toArray

    valBrancing.branches(randomVars(scala.util.Random.nextInt(randomVars.size)), store)
  }

  val branches: ((List[Propagator], Store, ValBranching) => (List[List[Propagator]], Store)) = randomVarBranchingFun

}