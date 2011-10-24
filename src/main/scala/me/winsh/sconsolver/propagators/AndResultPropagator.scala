package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class AndResultPropagator(val x: Var, val y: Var, val result: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, result)

  def propagate(s: Store): (PropagatorMessage, Store) = {

    val xD = s(x)
    val yD = s(y)
    val zD = s(result)

    if (zD.fixPoint(0) && (xD.fixPoint(1) && yD.fixPoint(1)))
      (Failed, s)
    else if (zD.fixPoint(0) && (xD.fixPoint(0) || yD.fixPoint(0)))
      (Subsumed, s)
    else if (zD.fixPoint(1))
      new AndPropagator(x, y).propagate(s)
    else
      (FixPoint, s)

  }

}