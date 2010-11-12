package me.winsh.sconsolver.core.search

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.search._
import me.winsh.sconsolver.branchings.value._
import me.winsh.sconsolver.branchings.variable._
import org.junit._
import Assert._

class DepthFirstSearchTest {

	@Test
	def severalSolutionsTest{
		
		val simpleModel = new ConstraintSatisfactionProblemModel[Int]{
			
			branching = Branching(FirstUnassigned, MinValue)
  
			searchMethod = DepthFirstSearch
			
			val x = newIntVar((4 to 7).union(1 to 2).union(10 to 11))
		
			x!==c(5)
			
			x<c(7)
			
			def solutionStoreToSolutionTypeFunction(s:Store) = s(x).min
			
		}
		
		
		val allSolutions = simpleModel.findAllSolutions
		
		println(allSolutions)
		println(simpleModel.initialStore)
		
		assertEquals(4, allSolutions.size)
		
		assertTrue(allSolutions.contains(1))
		
		assertTrue(allSolutions.contains(2))
		
		assertTrue(allSolutions.contains(4))

		assertTrue(allSolutions.contains(6))		
	}
	
		@Test
	def noSolutionTest{
		
		val simpleModel = new ConstraintSatisfactionProblemModel[Int]{
			
			branching = Branching(FirstUnassigned, MinValue)
  
			searchMethod = DepthFirstSearch
			
			val x = newIntVar((4 to 6).union(1 to 2).union(10 to 11))
		
			x!==c(5)
			
			x<c(7)
			
			x>c(8)
			
			def solutionStoreToSolutionTypeFunction(s:Store) = s(x).min
			
		}
		
		
		val allSolutions = simpleModel.findAllSolutions
		
		assertEquals(0, allSolutions.size)
		

		
	}
	
}