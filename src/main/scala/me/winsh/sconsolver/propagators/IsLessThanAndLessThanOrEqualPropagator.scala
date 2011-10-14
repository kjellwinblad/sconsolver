package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class IsLessThanAndLessThanOrEqualPropagator(val x: Var, val y: Var, val z: Var, val lessThan: Boolean = true) extends Propagator {

  val parameters: List[Var] = List(x, y, z)
  private val comparator1 = if(lessThan) (a:Int,b:Int)=>a<b else (a:Int,b:Int)=>a<=b
  private val comparator2 = if(lessThan) (a:Int,b:Int)=>a>=b else (a:Int,b:Int)=>a>b
  //(x<y)==z
  def propagate(s: Store) = {

    val xDomain = s(x)
    val yDomain = s(y)
    val zDomain = s(z)

    if (zDomain.fixPoint(1))
      if (lessThan)
        new LessThanPropagator(x, y).propagate(s)
      else
        new LessThanOrEqualPropagator(x, y).propagate(s)
    else if (zDomain.fixPoint(0))
      if (lessThan)
        new LessThanOrEqualPropagator(y, x).propagate(s)
      else
        new LessThanPropagator(y, x).propagate(s)
    else if(comparator1(xDomain.max,yDomain.min)){
      (Subsumed, s(z, Domain(1)))
    } else if(comparator2(xDomain.min,yDomain.max)){
      (Subsumed, s(z, Domain(0)))
    }else
      (FixPoint, s)
  }
}