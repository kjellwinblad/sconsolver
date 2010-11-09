package me.winsh.scons

import me.winsh.scons.core._
import me.winsh.scons.propagators._
import org.junit._
import Assert._


class PropagateTest {
	
	@Test
	def propagate(){
		
		val simpleCSP = new SimpleCSPBase {
			
			val x =  newIntVar(5 to 15)
			
			val y =  newIntVar(1 to 10)
			 
			lessThanOrEqual(x, y)
			
			lessThanOrEqual(y, 7)
			
			val s = initialStore
			
			val p = initialPropagatorSet
			 
			val (newP, newS) = Propagate.propagate(p, s)
			
			
			assertTrue(newP.contains(new LessThanOrEqualPropagator(x,y)))
			
			assertEquals(Domain(5 to 7), newS(x))
			
			assertEquals(Domain(5 to 7), newS(y))
		}
		
	}

}