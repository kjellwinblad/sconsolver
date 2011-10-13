package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import org.junit._
import Assert._

@Test
class LogicalAssignmentTest {

  @Test
  def equalsNotTrueTest{

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(5 to 15)
      val y = newIntVar(16 to 20)
      val z = newBoolVar()
      
      isEqual(x,y,z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(0), newS(z))
      assertTrue(newP.isEmpty)

    }

  }

  @Test
  def equalsTrueTest{

    val simpleCSP = new BasicCSPModel {

      val x = c(1)
      val y = c(1)
      val z = newBoolVar()
      
      isEqual(x,y,z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(1), newS(z))
      assertTrue(newP.isEmpty)

    }

  }
  
  @Test
  def solutionTest{

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(1 to 2)
      val y = newIntVar(2 to 3)
      val z = newBoolVar()
      
      isEqual(x,y,z)
    }
    
    simpleCSP.findAllSolutions.foreach((solution)=>{
      val x = solution(simpleCSP.x).value
      val y = solution(simpleCSP.y).value
      val z = if(solution(simpleCSP.z).value==1)true else false
      assert((x==y)==z)
    })

  }
  
}