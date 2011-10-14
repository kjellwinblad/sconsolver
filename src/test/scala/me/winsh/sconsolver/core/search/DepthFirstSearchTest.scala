package me.winsh.sconsolver.core.search

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.search._
import me.winsh.sconsolver.branchings.value._
import me.winsh.sconsolver.branchings.variable._
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

class DepthFirstSearchTest {

  @Test 
  def severalSolutionsTest {

    val simpleModel = new CSPModel[Int] {

      branching = Branching(FirstUnassigned, MinValue)
 
      searchMethod = DepthFirstSearch

      val x = newIntVar((4 to 7).union(1 to 2).union(10 to 11))
 
      satisfy(x !== c(5))

      satisfy(x < c(7))

      def solutionStoreToSolution(s: Store) = s(x).min

    }

    val allSolutions = simpleModel.findAllSolutions

    assertEquals(4, allSolutions.size)

    assertTrue(allSolutions.contains(1))

    assertTrue(allSolutions.contains(2))
//(1 to 2, 4, 6)
    assertTrue(allSolutions.contains(4))

    assertTrue(allSolutions.contains(6))
  }

  @Test
  def noSolutionTest {

    val simpleModel = new CSPModel[Int] {

      branching = Branching(FirstUnassigned, MinValue)

      searchMethod = DepthFirstSearch

      val x = newIntVar((4 to 6).union(1 to 2).union(10 to 11))

      satisfy(x !== c(5))

      satisfy(x < c(7))

      satisfy(x > c(8))

      def solutionStoreToSolution(s: Store) = s(x).min

    }

    val allSolutions = simpleModel.findAllSolutions

    assertEquals(0, allSolutions.size)

  }

  @Test
  def findFirstSolutionTest {

    val simpleModel = new CSPModel[Int] {

      branching = Branching(FirstUnassigned, MinValue)

      searchMethod = DepthFirstSearch

      val x = newIntVar((4 to 6).union(1 to 2).union(10 to 11))

      satisfy(x !== c(5))

      satisfy(x < c(7))

      def solutionStoreToSolution(s: Store) = s(x).min

    }

    val firstSolution = simpleModel.findFirstSolution

    assertEquals(Some(1), firstSolution)

  }

  @Test
  def noFirstSolutionTest {

    val simpleModel = new CSPModel[Int] {

      branching = Branching(FirstUnassigned, MinValue)

      searchMethod = DepthFirstSearch

      val x = newIntVar((4 to 6).union(1 to 2).union(10 to 11))

      satisfy(x !== c(5))

      satisfy(x > c(30))

      def solutionStoreToSolution(s: Store) = s(x).min

    }

    val firstSolution = simpleModel.findFirstSolution

    assertEquals(None, firstSolution)

  }
  
  
    @Test
  def findBestSolutionTest1 {

    val simpleModel = new ConstraintSatisfactionProblemModel[Int] {

      branching = Branching(FirstUnassigned, MinValue)

      searchMethod = DepthFirstSearch

      val x = newIntVar((4 to 6).union(1 to 2).union(10 to 11))

      satisfy(x !== c(5))

      satisfy(x < c(7))

      override def constrain(s:Store) = {
    	  
    	  val xValue = s(x).min

    	  satisfy(x > c(xValue))
      }
      
      def solutionStoreToSolution(s: Store) = s(x).min

    }

    val firstSolution = simpleModel.findBestSolution

    assertEquals(Some(6), firstSolution)

  }
    
       @Test
  def findBestSolutionTest2 {

    val simpleModel = new ConstraintSatisfactionProblemModel[Int] {

      branching = Branching(FirstUnassigned, MinValue)

      searchMethod = DepthFirstSearch

      val x = newIntVar((4 to 6).union(1 to 2).union(10 to 11))

      satisfy(x !== c(5))

      satisfy(x < c(7))

      override def constrain(s:Store) = {
    	  
    	  val xValue = s(x).min

    	  satisfy(x < c(xValue))
      }
      
      def solutionStoreToSolution(s: Store) = s(x).min

    }

    val firstSolution = simpleModel.findBestSolution

    assertEquals(Some(1), firstSolution)

  }

}