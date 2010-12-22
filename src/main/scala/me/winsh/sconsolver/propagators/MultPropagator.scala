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
      (Subsumed(), s)
    else if (xD.fixPoint(0)) {

      val newZD = zD.intersection(xD)

      if (newZD.failed)
        (Failed(), s(result, newZD))
      else
        (Subsumed(), s(result, newZD))

    } else if (yD.fixPoint(0)) {

      val newZD = zD.intersection(yD)

      if (newZD.failed)
        (Failed(), s(result, newZD))
      else
        (Subsumed(), s(result, newZD))

    } else if (zD.fixPoint(0) && xD.intersection(zD).failed && yD.intersection(zD).failed)
    	(Failed(), s(x, xD.intersection(zD))(y, yD.intersection(zD)))
      else if (zD.intersection(Domain(0)).failed && 
    		  (xD.intersection(Domain(0)).fixPoint || yD.intersection(Domain(0)).fixPoint))
    	  (NoFixPoint(), s(x, xD.difference(Domain(0)))(y, yD.difference(Domain(0))))
      else if (zD.intersection(Domain(0)).fixPoint && 
    		  (xD.intersection(Domain(0)).failed && yD.intersection(Domain(0)).failed))
    	  (NoFixPoint(), s(result, zD.difference(Domain(0))))
      else if(xD.intersection(Domain(0)).fixPoint || yD.intersection(Domain(0)).fixPoint) 
    	  (FixPoint(), s)
      else {

      val newZD = constrainZ(xD, yD, zD)

      if (newZD.failed)
        (Failed(), s(result, newZD))
      else {

          val newXD = constrainMult1(xD, yD, newZD)
          
          val newYD = constrainMult1(yD, xD, newZD)

          val newStore = s(x, newXD)(y, newYD)(result, newZD)

          if (newYD.failed || newXD.failed )
            (Failed(), newStore)
          else if (subsumed(newXD, newYD, newZD))
            (Subsumed(), newStore)
          else
            (NoFixPoint(), newStore)

        }
      }
    

  }

  private def subsumed(x: Domain, y: Domain, z: Domain) =
    x.fixPoint && y.fixPoint && z.fixPoint && ((x.value * y.value) == z.value)

  private def constrainZ(xD: Domain, yD: Domain, zD: Domain) = {

    val zMinMaxCandidates = for (
      x <- List(xD.min, xD.max);
      y <- List(yD.min, yD.max)
    ) yield x * y

    val zMin = zMinMaxCandidates.tail.foldLeft(
      zMinMaxCandidates.head)((e1, e2) => e1.min(e2))

    val zMax = zMinMaxCandidates.tail.foldLeft(
      zMinMaxCandidates.head)((e1, e2) => e1.max(e2))

    zD.greaterThanOrEqual(zMin).lessThanOrEqual(zMax)

  }

  private def constrainMult1(mult1D: Domain, mult2D: Domain, zD: Domain) = {


    	
      val mult1MinMaxCandidates =
        for (
          mult2 <- List(mult2D.min, mult2D.max);
          z <- List(zD.min, zD.max)
        ) yield z.toDouble / mult2

      val mult1Min = (mult1MinMaxCandidates.tail.foldLeft(
        mult1MinMaxCandidates.head)((e1, e2) => e1.min(e2))).floor.toInt

      val mult1Max = (mult1MinMaxCandidates.tail.foldLeft(
        mult1MinMaxCandidates.head)((e1, e2) => e1.max(e2))).ceil.toInt

      val firstConstrain = mult1D.greaterThanOrEqual(mult1Min)
      println(mult1D + " " + mult1Min + " " + mult1Max + " " + firstConstrain.size)
      if(firstConstrain.isEmpty)
    	  firstConstrain
      else
    	  firstConstrain.lessThanOrEqual(mult1Max)

    

  }

}