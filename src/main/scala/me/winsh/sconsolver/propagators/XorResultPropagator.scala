package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class XorResultPropagator(val x: Var, val y: Var, val z: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, z)

  def propagate(s: Store) = {
    val xD = s(x)
    val yD= s(y)
    val zD= s(z)

    if (zD.fixPoint(1))
      new XorPropagator(x,y).propagate(s)
    else if (zD.fixPoint(0))
      new EqualPropagator(x,y).propagate(s)
    else 
      (FixPoint, s)
  }

}