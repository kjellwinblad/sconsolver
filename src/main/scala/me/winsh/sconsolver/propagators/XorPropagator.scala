package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class XorPropagator(val x: Var, val y: Var) extends Propagator {

  val parameters: List[Var] = List(x, y)

  def propagate(s: Store) = {
    val xDomain = s(x)
    val yDomain = s(y)

    if ((xDomain.fixPoint(1) && yDomain.fixPoint(0)) ||
      (xDomain.fixPoint(0) && yDomain.fixPoint(1)))
      (Subsumed, s)
    else if ((xDomain.fixPoint(0) && yDomain.fixPoint(0)) ||
      (xDomain.fixPoint(1) && yDomain.fixPoint(1)))
      (Failed, s(x, Domain())(y, Domain()))
    else {
      
      val newDomainX =
        if (yDomain.fixPoint(0))
          xDomain.difference(Domain(0))
        else if (yDomain.fixPoint(1))
          xDomain.difference(Domain(1))
        else xDomain
        
      val newDomainY =
        if (xDomain.fixPoint(0))
          yDomain.difference(Domain(0))
        else if (xDomain.fixPoint(1))
          yDomain.difference(Domain(1))
        else yDomain

      if (newDomainX.failed || newDomainY.failed)
        (Failed, s(x, newDomainX)(y, newDomainY))
      else if (newDomainX.fixPoint && newDomainX.fixPoint)
        (Subsumed, s(x, newDomainX)(y, newDomainY))
      else
        (FixPoint, s)

    }
  }

}