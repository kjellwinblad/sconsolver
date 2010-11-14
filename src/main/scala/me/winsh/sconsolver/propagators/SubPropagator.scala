package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class SubPropagator(val x: Var, val y: Var, val result: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, result)

  def propagate(s: Store) = {

    //x - y = z

    val xD = s(x)

    val yD = s(y)

    val zD = s(result)

    if (xD.fixPoint && yD.fixPoint && zD.fixPoint && ((xD.value - yD.value) == zD.value))
      (Subsumed(), s)
    else {
    	
      val newZD = zD.greaterThanOrEqual(xD.min - yD.max).lessThanOrEqual(xD.max - yD.min)

      val newXD = xD.greaterThanOrEqual(zD.min + yD.min).lessThanOrEqual(zD.max + yD.max)

      val newYD = yD.greaterThanOrEqual(xD.min - zD.max).lessThanOrEqual(xD.max - zD.min)

      val newStore = s(x, newXD)(y, newYD)(result, newZD)

      if (newXD.failed || newYD.failed || newZD.failed)
        (Failed(), newStore)
      else
        (NoFixPoint(), newStore)
    }

  }

}