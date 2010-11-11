package me.winsh.sconsolver.core

import me.winsh.sconsolver.propagators._
import org.junit._
import Assert._

class CSPTest {
	
	@Test
	def simpleDefinitionTest {
		
		val simpleCSP = new CSPModel {
			
			val x =  newIntVar(1 to 10)
			
			val y =  newIntVar(5 to 15)
			 
			lessThanOrEqual(x, y)
			
			val s = initialStore 
			
			assertEquals(Domain(1 to 10), s(x))
			
			assertEquals(Domain(5 to 15), s(y))
			
			assertEquals(new LessThanOrEqualPropagator(x,y), initialPropagators.head)
		}
		
	}
	
	@Test
	def infixConstraintNotificationTest {
		
		val simpleCSP = new CSPModel {
			
			val x:Var =  newIntVar(5 to 15)
			
			val y:Var =  newIntVar(1 to 10)
			
			x <= y
			
			val constant = c(7)
			y <= constant
			
			val s = initialStore
			
			val p = initialPropagators
			
			val (newP, newS) = Propagate.propagate(p, s)
					
			assertTrue(newP.toSet.contains(new LessThanOrEqualPropagator(x,y)))
			
			assertFalse(newP.toSet.contains(new LessThanOrEqualPropagator(y,constant)))
			
			assertEquals(Domain(5 to 7), newS(x))
			
			assertEquals(Domain(5 to 7), newS(y))
		}
		 
	} 
	
	
		@Test
	def findFirstTest {
		
		val simpleCSP = new CSPModel {
			
			val x =  newIntVar(5 to 15)
			
			val y =  newIntVar(1 to 10)
			
			x <= y
			
			y <= (7:Var)
			
		}
		
		simpleCSP.findFirstSolution match{
			case Some(s) =>{
				assertEquals(Domain(5), s(simpleCSP.x))
				assertEquals(Domain(5), s(simpleCSP.x))
			}
			case None => fail("There should be solutions here")
		}
		
	}
		

	
}