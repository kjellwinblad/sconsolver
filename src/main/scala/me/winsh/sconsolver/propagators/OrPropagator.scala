package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class OrPropagator(val x: Var, val y: Var) extends Propagator {

  val parameters: List[Var] = List(x, y)

  def propagate(s: Store) = {
    val xD = s(x)
    val yD = s(y)

    if (xD.fixPoint(0) && yD.fixPoint(0))
      (Failed, s(x, Domain())(y, Domain()))
    else if (xD.fixPoint(0) && yD==Domain(0,1))
      (Subsumed, s(y,Domain(1)))
    else if (yD.fixPoint(0) && xD==Domain(0,1))
      (Subsumed, s(x,Domain(1)))
    else if (xD.fixPoint(1) || yD.fixPoint(1))
      (Subsumed, s)
    else
      (FixPoint, s)
  }

}