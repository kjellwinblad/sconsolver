package me.winsh.scons

import me.winsh.scons.core._
import me.winsh.scons.propagators._
import org.junit._
import Assert._

class CSPTest {
	
	@Test
	def simpleDefinitionTest {
		
		val simpleCSP = new CSP {
			
			val x:Var =  1 to 10
			
			val y:Var =  5 to 15
			 
			lessThanOrEqual(x, y)
			
			val s = initialStore
			
			assertEquals(Domain(1 to 10), s(x))
			
			assertEquals(Domain(5 to 15), s(y))
			
			assertEquals(new LessThanOrEqualPropagator(x,y), initialPropagatorSet.toList.first)
		}
		
	}
	
}