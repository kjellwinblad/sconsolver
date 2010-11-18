package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class SubPropagator(val x: Var, val y: Var, val result: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, result)

  def propagate(s: Store) = {

    //x - y = z

    val xD = s(x)

    val yD = s(y)

    val zD = s(result)

    if (subsumed(xD, yD, zD))
      (Subsumed(), s)
    else {

      val newZD = zD.greaterThanOrEqual(xD.min - yD.max).lessThanOrEqual(xD.max - yD.min)

      if (newZD.failed)
        (Failed(), s(result, newZD))
      else {

        val newXD = xD.greaterThanOrEqual(newZD.min + yD.min).lessThanOrEqual(newZD.max + yD.max)

        if (newXD.failed)
          (Failed(), s(x,newXD)(result, newZD))
        else {

          val newYD = yD.greaterThanOrEqual(newXD.min - newZD.max).lessThanOrEqual(newXD.max - newZD.min)

          val newStore = s(x, newXD)(y, newYD)(result, newZD)

          if (newYD.failed)
            (Failed(), newStore)
          else if (subsumed(newXD, newYD, newZD))
        	  (Subsumed(), newStore)
          else
            (NoFixPoint(), newStore)
            
        }
      }
    }

  }

  private def subsumed(x: Domain, y: Domain, z: Domain) =
    x.fixPoint && y.fixPoint && z.fixPoint && ((x.value - y.value) == z.value)

}