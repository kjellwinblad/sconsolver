package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

class NotEqualsPropagator(val x:Var,val y:Var) extends Propagator{

	val parameters:List[Var] = List(x, y) 
	
	def propagate(s:Store) = {
		
		val xDomain = s(x)
		
		val yDomain = s(y)
		
		def single(d:Domain) = if(d.fixPoint) d else Domain()
		
		
		val newDomainX = xDomain.difference(single(yDomain))
		
		val newDomainY = yDomain.difference(single(xDomain))
		
		val newStore = s(x, newDomainX)(y, newDomainY)
		
		if(newDomainX.failed || newDomainY.failed)
			(Failed(), newStore)
		else if(newDomainX.intersection(newDomainY).isEmpty)
			(Subsumed(), newStore)
		else
			(FixPoint(), newStore)

	}
	
}