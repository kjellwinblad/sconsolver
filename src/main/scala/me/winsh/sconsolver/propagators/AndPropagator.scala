package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class AndPropagator(val x: Var, val y: Var) extends Propagator {

  val parameters: List[Var] = List(x, y)

  def propagate(s: Store) = {
    val xDomain = s(x)
    val yDomain = s(y)

    if (xDomain.fixPoint(0) || yDomain.fixPoint(0))
      (Failed, s(x, Domain())(y, Domain()))
    else
      (Subsumed, s(x,Domain(1))(y,Domain(1)))
  }

}