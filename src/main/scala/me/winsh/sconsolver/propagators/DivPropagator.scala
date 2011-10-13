package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class DivPropagator(val x: Var, val y: Var, val result: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, result)

  def propagate(s: Store) = {

    //x / y = z

    val yD = s(y)

    //We want to remove 0 from yD if it contains 0
    if (yD.contains(0)) {
      val newYD = yD.difference(Domain(0))
      if (newYD.failed)
        (Failed, s(y, newYD))
      else
        (NoFixPoint, s(y, newYD))
    } else
      new MultPropagator(y, result, x).propagate(s)

  }

}