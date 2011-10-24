package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class NotEqualResultPropagator(val x: Var, val y: Var, val z: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, z)

  def propagate(s: Store) = {

    val xDomain = s(x)
    val yDomain = s(y)
    val zDomain = s(z)

    if (zDomain.fixPoint(1))
      new NotEqualPropagator(x, y).propagate(s)
    else if (zDomain.fixPoint(0))
      new EqualPropagator(x, y).propagate(s)
    else if (xDomain.fixPoint && yDomain.fixPoint && xDomain.value == yDomain.value) {
      val newDomainZ = zDomain.difference(Domain(1))
      if (newDomainZ.isEmpty)
        (Failed, s(z, newDomainZ))
      else
        (Subsumed, s(z, newDomainZ))
    } else {

      val intersection = xDomain.intersection(yDomain)

      intersection.isEmpty match {
        case true => (Subsumed, s(z, Domain(1)))
        case false => (FixPoint, s)
      }
    }
  }

}