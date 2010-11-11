package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import org.junit._
import Assert._

@Test
class NotEqualTest {

	@Test
	def notEqualSuccessTest{
		
		
		val simpleCSP = new CSPModel {
			
			val x =  newIntVar(10 to 15)
			
			val y =  c(8)
			
			x !== y
			
			val s = initialStore
			
			val p = initialPropagators
			
			val (newP, newS) = Propagate.propagate(p, s)


			
			assertEquals(Domain(10 to 15), newS(x))
			
			assertEquals(Domain(8), newS(y))
			
			assertTrue(newP.isEmpty) 
			
			
		}
		
	
	}
	
		@Test
	def notEqualFailTest{
		
		
		val simpleCSP = new CSPModel {
			
			val x =  c(5)
			
			val y =  c(5)
			
			x !== y
			
			val s = initialStore
			
			val p = initialPropagators
			
			val (newP, newS) = Propagate.propagate(p, s)
			
			assertTrue(newS.failed)
			

			
			
		}
		
	
	}
	
}