package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import org.junit._
import Assert._

@Test
class EqualTest {

	@Test
	def equalSuccessTest{
		
		
		val simpleCSP = new CSPModel {
			
			val x =  newIntVar(5 to 15)
			
			val y =  c(8)
			
			x === y
			
			val s = initialStore
			
			val p = initialPropagators
			
			val (newP, newS) = Propagate.propagate(p, s)


			
			assertEquals(Domain(8), newS(x))
			
			assertEquals(Domain(8), newS(y))
			
			assertTrue(newP.isEmpty) 
			
			
		}
		
	
	}
	
		@Test
	def equalFailTest{
		
		
		val simpleCSP = new CSPModel {
			
			val x =  newIntVar(5 to 15)
			
			val y =  newIntVar(40 to 50)
			
			x === y
			
			val s = initialStore
			
			val p = initialPropagators
			
			val (newP, newS) = Propagate.propagate(p, s)
			
			assertTrue(newS.failed)
			

			
			
		}
		
	
	}
	
}