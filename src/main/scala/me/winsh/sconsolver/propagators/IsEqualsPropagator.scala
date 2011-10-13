package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class IsEqualsPropagator(val x: Var, val y: Var, val z: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, z)

  def propagate(s: Store) = {

    val zDomain = s(z)

    if (zDomain.fixPoint(1))
      new EqualsPropagator(x, y).propagate(s)
    else if (zDomain.fixPoint(0))
      new NotEqualsPropagator(x, y).propagate(s)
    else {

      val xDomain = s(x)

      val yDomain = s(y)

      val intersection = xDomain.intersection(yDomain)

      intersection.isEmpty match {
        case true => (Subsumed, s(z, Domain(0)))
        case false => (FixPoint, s)
      }
    }
  }
}