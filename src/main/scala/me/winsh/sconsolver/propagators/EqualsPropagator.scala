package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class EqualsPropagator(val x:Var,val y:Var) extends Propagator{

	val parameters:List[Var] = List(x, y) 
	
	def propagate(s:Store) = {
		
		val xDomain = s(x)

		val yDomain = s(y)
		
		val newDomainX = xDomain.intersection(yDomain)
		
		val newDomainY = yDomain.intersection(xDomain)
		
		val newStore = s(x, newDomainX)(y, newDomainY)
		
		if(newDomainX.failed || newDomainY.failed)
			(Failed(), newStore)
		if(newDomainX.fixPoint || newDomainY.fixPoint)
			(Subsumed(), newStore)
		else
			(FixPoint(), newStore)

	}
	
}