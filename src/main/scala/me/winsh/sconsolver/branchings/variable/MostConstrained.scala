package me.winsh.sconsolver.branchings.variable
import me.winsh.sconsolver.core._
import scala.collection.mutable.HashMap

object MostConstrained extends VarBranching {

  def randomVarBranchingFun(propagators: List[Propagator], store: Store, valBrancing: ValBranching) = {

    val variable =
      if (propagators.size == 0)
        store.firstUnassignedVar
      else {
        
        val constraintsPerVarCount = HashMap[Var, Int]()
        val unassignedVarsSet = Set() ++ store.unassignedVars
        for (prop <- propagators)
          prop.parameters.foreach((variable) => if(unassignedVarsSet.contains(variable)){
            constraintsPerVarCount.get(variable) match {
              case None => constraintsPerVarCount.put(variable, 1)
              case Some(count) => constraintsPerVarCount.put(variable, count + 1)
            }
          })

        val maxPair = constraintsPerVarCount.iterator.maxBy(_._2)
        maxPair._1
      }
    valBrancing.branches(variable, store)
  }

  val branches: ((List[Propagator], Store, ValBranching) => (List[List[Propagator]], Store)) = randomVarBranchingFun

}