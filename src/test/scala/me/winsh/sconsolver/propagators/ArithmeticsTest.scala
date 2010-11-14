package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import org.junit._
import Assert._

@Test
class ArithmeticsTest {
 
	@Test
	def add{
		
		val simpleCSP = new CSPModel[Int] {
			
			val x =  newIntVar(1 to 10)
			
			val y =  newIntVar(1 to 10)
			
			val result =  newIntVar(-100,100)
			
			add(x, y, result)
						
			def solutionStoreToSolution(s:Store) = {
				
				
				s(result).value
			}
		}
		
		val solutions = simpleCSP.findAllSolutions
		
		assertEquals(100, solutions.size)
		
		assertEquals(Set() ++ (2 to 20), solutions.toSet)
	}
	
		@Test
	def addFail{
		
		val simpleCSP = new CSPModel[Int] {
			
			val x =  newIntVar(1 to 10)
			
			val y =  newIntVar(1 to 10)
			
			val result =  c(1)
			
			add(x, y, result)
						
			def solutionStoreToSolution(s:Store) = {
				
				
				s(result).value
			}
		}
		
		val solutions = simpleCSP.findAllSolutions.toSet
		
		assertTrue(solutions.isEmpty)
	}
		
			@Test
	def sub{
		
		val simpleCSP = new CSPModel[Int] {
			
			val x =  newIntVar(1 to 10)
			
			val y =  newIntVar(1 to 10)
			
			val result =  newIntVar(-100,100)
			
			sub(x, y, result)
						
			def solutionStoreToSolution(s:Store) = {
				
				
				s(result).value
			}
		}
		
		val solutions = simpleCSP.findAllSolutions
		
		assertEquals(100, solutions.size)
		
		assertEquals(Set() ++ (-9 to 9), solutions.toSet)
	}
	
		@Test
	def subFail{
		
		val simpleCSP = new CSPModel[Int] {
			
			val x =  newIntVar(1 to 10)
			
			val y =  newIntVar(1 to 10)
			
			val result =  c(10)
			
			sub(x, y, result)
						
			def solutionStoreToSolution(s:Store) = {
				
				
				s(result).value
			}
		}
		
		val solutions = simpleCSP.findAllSolutions.toSet
		
		assertTrue(solutions.isEmpty)
	}
	
	
}