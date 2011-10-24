package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class NotPropagator(val x: Var) extends Propagator {

  val parameters: List[Var] = List(x)

  def propagate(s: Store) = {
    val xD = s(x)

    if (xD.fixPoint(0))
      (Subsumed, s)
    if (xD.fixPoint(1))
      (Failed, s(x, Domain()))
    else
      (Subsumed, s(x,Domain(0)))
  }

}