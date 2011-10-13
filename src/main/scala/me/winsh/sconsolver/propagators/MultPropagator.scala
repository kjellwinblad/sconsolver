package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class MultPropagator(val x: Var, val y: Var, val result: Var) extends Propagator {

  val parameters: List[Var] = List(x, y, result)

  def propagate(s: Store) = {

    //x * y = z

    val xD = s(x)

    val yD = s(y)

    val zD = s(result)

    if (subsumed(xD, yD, zD))
      (Subsumed, s)
    else if (xD.fixPoint(0)) { //x=0

      val newZD = zD.intersection(xD)

      (ifZEmptyFailedElseSubsumed(newZD), s(result, newZD))

    } else if (yD.fixPoint(0)) { //y=0

      val newZD = zD.intersection(yD)

      (ifZEmptyFailedElseSubsumed(newZD), s(result, newZD))

    } else if (zD.fixPoint(0) && !xD.contains(0) && !yD.contains(0)) { //z=0

      (Failed, s(x, Domain.empty)(y, Domain.empty))

    } else if (zD.contains(0) && !xD.contains(0) && !yD.contains(0)) {

      (NoFixPoint, s(result, zD.difference(Domain(0))))

    } else {

      val newZD = constrainZ(xD, yD, zD)

      if (newZD.failed)
        (Failed, s(result, newZD))
      else {

        val newXD = constrainMult1(xD, yD, newZD)

        val newYD = constrainMult1(yD, xD, newZD)

        val newStore = s(x, newXD)(y, newYD)(result, newZD)

        if (newYD.failed || newXD.failed)
          (Failed, newStore)
        else if (subsumed(newXD, newYD, newZD))
          (Subsumed, newStore)
        // else if (newXD!=xD || newYD!=yD)
        //   (NoFixPoint, newStore) Must Implement equals on domain
        else
          (NoFixPoint, newStore)

      }
    }

  }

  private def subsumed(x: Domain, y: Domain, z: Domain) =
    x.fixPoint && y.fixPoint && z.fixPoint && ((x.value * y.value) == z.value)

  private def ifZEmptyFailedElseSubsumed(dom: Domain) =
    if (dom.failed) Failed else Subsumed

  private def constrainZ(xD: Domain, yD: Domain, zD: Domain) = {

    val zMinMaxCandidates = for {
      x <- List(xD.min, xD.max);
      y <- List(yD.min, yD.max)
    } yield x * y

    val zMin = zMinMaxCandidates.min

    val zMax = zMinMaxCandidates.max

    zD.greaterThanOrEqual(zMin).lessThanOrEqual(zMax)

  }

  private def constrainMult1(mult1D: Domain, mult2D: Domain, zD: Domain) =
    if (mult2D.contains(0))
      mult1D
    else {

      val mult1MinMaxCandidates =
        for {
          mult2 <- mult2D;
          z <- List(zD.min, zD.max)
        } yield z / mult2

      val mult1Min = mult1MinMaxCandidates.min.floor.toInt

      val mult1Max = mult1MinMaxCandidates.max.ceil.toInt

      val firstConstrain = mult1D.greaterThanOrEqual(mult1Min)

      if (firstConstrain.isEmpty)
        firstConstrain
      else
        firstConstrain.lessThanOrEqual(mult1Max)

    }

}