package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class AddPropagator(val x: Var, val y: Var, val result: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, result)

  def propagate(s: Store) = {

    //x + y = z

    val xD = s(x)

    val yD = s(y)

    val zD = s(result)

    if (xD.fixPoint && yD.fixPoint && zD.fixPoint && ((xD.value + yD.value) == zD.value))
      (Subsumed(), s)
    else {
    	
      val newZD = zD.greaterThanOrEqual(xD.min + yD.min).lessThanOrEqual(xD.max + yD.max)

      val newXD = xD.greaterThanOrEqual(zD.min - yD.max).lessThanOrEqual(zD.max - yD.min)

      val newYD = yD.greaterThanOrEqual(zD.min - xD.max).lessThanOrEqual(zD.max - xD.min)

      val newStore = s(x, newXD)(y, newYD)(result, newZD)

      if (newXD.failed || newYD.failed || newZD.failed)
        (Failed(), newStore)
      else
        (NoFixPoint(), newStore)
    }

  }

}