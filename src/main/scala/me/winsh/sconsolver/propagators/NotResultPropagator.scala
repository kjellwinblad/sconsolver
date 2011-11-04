package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class NotResultPropagator(val x: Var, val result: Var) extends Propagator {

  val parameters: List[Var] = List(x, result)

  def propagate(s: Store) = {
    val xD = s(x)
    val zD = s(result)

    if (zD.fixPoint(1))
      new NotPropagator(x).propagate(s)
    else if (zD.fixPoint(0) && xD.fixPoint(0))
      (Failed, s(x, Domain())(result,Domain()))
    else if (xD.fixPoint(0))
      (Subsumed, s(result,Domain(1)))
    else if (zD.fixPoint(0))
      (Subsumed, s(x, Domain(1)))
    else if (xD.fixPoint(1))
      (Subsumed, s(result,Domain(0)))      
    else
      (FixPoint, s)
  }

}