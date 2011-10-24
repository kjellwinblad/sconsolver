package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import org.junit._
import Assert._

@Test
class IntRelationsAssignmentTest {

  @Test
  def equalsNotTrueTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(5 to 15)
      val y = newIntVar(16 to 20)
      val z = newBoolVar()

      isEqual(x, y, z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(0), newS(z))
      assertTrue(newP.isEmpty)

    }

  }

  @Test
  def equalsTrueTest {

    val simpleCSP = new BasicCSPModel {

      val x = c(1)
      val y = c(1)
      val z = newBoolVar()

      isEqual(x, y, z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(1), newS(z))
      assertTrue(newP.isEmpty)

    }

  }

  @Test
  def equalsSolutionTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(1 to 2)
      val y = newIntVar(2 to 3)
      val z = newBoolVar()

      isEqual(x, y, z)
    }

    val nrOfSolutions = simpleCSP.findAllSolutions.map((solution) => {
      val x = solution(simpleCSP.x).value
      val y = solution(simpleCSP.y).value
      val z = if (solution(simpleCSP.z).value == 1) true else false
      assert((x == y) == z)
    }).size

    assertEquals(nrOfSolutions, 4)

  }

  @Test
  def notEqualsNotTrueTest {

    val simpleCSP = new BasicCSPModel {

      val x = c(1)
      val y = c(1)
      val z = newBoolVar()

      isNotEqual(x, y, z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(0), newS(z))

    }

  }

  @Test
  def notEqualsTrueTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(1 to 5)
      val y = newIntVar(6 to 10)
      val z = newBoolVar()

      isNotEqual(x, y, z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(1), newS(z))

    }

  }

  @Test
  def notEqualsSolutionTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(1 to 2)
      val y = newIntVar(2 to 3)
      val z = newBoolVar()

      isNotEqual(x, y, z)
    }

    val nrOfSolutions = simpleCSP.findAllSolutions.map((solution) => {
      val x = solution(simpleCSP.x).value
      val y = solution(simpleCSP.y).value
      val z = if (solution(simpleCSP.z).value == 1) true else false

      assert((x != y) == z)
    }).size

    assertEquals(nrOfSolutions, 4)

  }

  @Test
  def lessThanNotTrueTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(10 to 20)
      val y = newIntVar(1 to 9)
      val z = newBoolVar()

      isLessThan(x, y, z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(0), newS(z))

    }

  }

  @Test
  def lessThanTrueTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(1 to 4)
      val y = newIntVar(5 to 6)
      val z = newBoolVar()

      isLessThan(x, y, z)

      val s = initialStore
      val p = initialPropagators
      val (newP, newS) = Propagate.propagate(p, s)

      assertEquals(newP, Nil)
      assertEquals(Domain(1), newS(z))

    }

  }

  @Test
  def lessThanSolutionTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(1 to 3)
      val y = newIntVar(2 to 4)
      val z = newBoolVar()

      isLessThan(x, y, z)
    }

    val nrOfSolutions = simpleCSP.findAllSolutions.map((solution) => {
      val x = solution(simpleCSP.x).value
      val y = solution(simpleCSP.y).value
      val z = if (solution(simpleCSP.z).value == 1) true else false
      assert((x < y) == z)
    }).size

    assertEquals(nrOfSolutions, 9)

  }

  @Test
  def lessThanOrEqualSolutionTest {

    val simpleCSP = new BasicCSPModel {

      val x = newIntVar(1 to 3)
      val y = newIntVar(2 to 4)
      val z = newBoolVar()

      isLessThanOrEqual(x, y, z)
    }

    val nrOfSolutions = simpleCSP.findAllSolutions.map((solution) => {
      val x = solution(simpleCSP.x).value
      val y = solution(simpleCSP.y).value
      val z = if (solution(simpleCSP.z).value == 1) true else false
      assert((x <= y) == z)
    }).size

    assertEquals(nrOfSolutions, 9)

  }
}