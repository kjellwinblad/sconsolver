package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class LessThanPropagator(val x: Var, val y: Var) extends Propagator {

  val parameters: List[Var] = List(x, y)

  def propagate(s: Store) = {

    val xDomain = s(x)

    val yDomain = s(y)

    val newDomainX = xDomain.lessThan(yDomain.max)

    val newDomainY = yDomain.greaterThan(xDomain.min)

    val newStore = s(x, newDomainX)(y, newDomainY)

    if (newDomainX.failed || newDomainY.failed)
      (Failed, newStore)
    else if (newDomainX.max < newDomainY.min)
      (Subsumed, newStore)
    else
      (FixPoint, newStore)

  }

}