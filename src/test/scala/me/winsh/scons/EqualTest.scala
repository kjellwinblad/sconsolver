package me.winsh.scons

import me.winsh.scons.core._
import me.winsh.scons.propagators._
import org.junit._
import Assert._

@Test
class EqualTest {

	@Test
	def equalSuccessTest{
		
		
		val simpleCSP = new SimpleCSPBase {
			
			val x =  newIntVar(5 to 15)
			
			val y =  newIntVar(8)
			
			x === y
			
			val s = initialStore
			
			val p = initialPropagatorSet
			
			val (newP, newS) = Propagate.propagate(p, s)


			
			assertEquals(Domain(8), newS(x))
			
			assertEquals(Domain(8), newS(y))
			
			assertTrue(newP.isEmpty) 
			
			
		}
		
	
	}
	
		@Test
	def equalFailTest{
		
		
		val simpleCSP = new SimpleCSPBase {
			
			val x =  newIntVar(5 to 15)
			
			val y =  newIntVar(40 to 50)
			
			x === y
			
			val s = initialStore
			
			val p = initialPropagatorSet
			
			val (newP, newS) = Propagate.propagate(p, s)
			
			assertTrue(newS.failed)
			

			
			
		}
		
	
	}
	
}