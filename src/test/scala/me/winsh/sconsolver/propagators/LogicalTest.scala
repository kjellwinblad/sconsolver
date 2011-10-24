package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import org.junit._
import Assert._

@Test
class LogicalTest {

  @Test
  def andSuccessTest {

    val simpleCSP = new BasicCSPModel {
      val x1:Var = true
      val y1:Var = newBoolVar()
      and(x1,y1)
      val x2:Var = newBoolVar()
      val y2:Var = newBoolVar()
      val z1:Var = true
      and(x2,y2, z1)
      val x3:Var = newBoolVar()
      val y3:Var = newBoolVar()
      val z2:Var = newBoolVar()
      and(x3,y3, z2)
      val x4:Var = newBoolVar()
      val y4:Var = false
      val z3:Var = false
      and(x4,y4, z3)
      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)
      assertEquals(Domain(1), newS(x1))
      assertEquals(Domain(1), newS(y1))
      assertEquals(Domain(1), newS(x2))
      assertEquals(Domain(1), newS(y2))
      assertEquals(Domain(1), newS(z1))
      assertEquals(Domain(0 to 1), newS(x3))
      assertEquals(Domain(0 to 1), newS(y3))
      assertEquals(Domain(0 to 1), newS(z2))
      assertEquals(Domain(0 to 1), newS(x4))
      assertEquals(Domain(0), newS(y4))
      assertEquals(Domain(0), newS(z3))
      assertTrue(newP.size == 1)
    }

  }

  @Test
  def andFailTest {
    val simpleCSP = new BasicCSPModel {
      val x:Var = newBoolVar()
      val y:Var = false
      val z:Var = true
      and(x,y, z)
      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)
      assertEquals(Domain(), newS(x))
      assertTrue(newP.size == 1)
    }
  }
  
  @Test
  def orSuccessTest {

    val simpleCSP = new BasicCSPModel {
      val x1:Var = true
      val y1:Var = newBoolVar()
      or(x1,y1)
      val x2:Var = newBoolVar()
      val y2:Var = newBoolVar()
      val z1:Var = false
      or(x2,y2, z1)
      val x3:Var = newBoolVar()
      val y3:Var = false
      val z2:Var = true
      or(x3,y3, z2)
      val x4:Var = newBoolVar()
      val y4:Var = false
      val z3:Var = newBoolVar()
      or(x4,y4, z3)
      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)
      assertEquals(Domain(1), newS(x1))
      assertEquals(Domain(0 to 1), newS(y1))
      assertEquals(Domain(0), newS(x2))
      assertEquals(Domain(0), newS(y2))
      assertEquals(Domain(0), newS(z1))
      assertEquals(Domain(1), newS(x3))
      assertEquals(Domain(0), newS(y3))
      assertEquals(Domain(1), newS(z2))
      assertEquals(Domain(0 to 1), newS(x4))
      assertEquals(Domain(0), newS(y4))
      assertEquals(Domain(0 to 1), newS(z3))
      assertTrue(newP.size == 1)
    }

  }

  @Test
  def orFailTest {
    val simpleCSP = new BasicCSPModel {
      val x:Var = false
      val y:Var = false
      val z:Var = true
      or(x,y, z)
      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)
      assertEquals(Domain(), newS(x))
      assertTrue(newP.size == 1)
    }
  }

}