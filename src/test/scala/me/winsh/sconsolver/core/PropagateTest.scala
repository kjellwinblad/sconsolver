package me.winsh.sconsolver.core

import me.winsh.sconsolver.propagators._
import org.junit._
import Assert._


class PropagateTest {
	
	@Test
	def propagate(){
		
		val simpleCSP = new BasicCSPModel {
			
			val x =  newIntVar(5 to 15)
			
			val y =  newIntVar(1 to 10)
			 
			lessThanOrEqual(x, y)
			
			lessThanOrEqual(y, 7)
			
			val s = initialStore
			
			val p = initialPropagators
			 
			val (newP, newS) = Propagate.propagate(p, s)
			
			
			assertTrue(newP.contains(new LessThanOrEqualPropagator(x,y)))
			
			assertEquals(Domain(5 to 7), newS(x))
			
			assertEquals(Domain(5 to 7), newS(y))
		}
		
	}

}