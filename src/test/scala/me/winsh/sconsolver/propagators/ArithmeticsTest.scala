package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._
import org.junit._
import Assert._

@Test
class ArithmeticsTest {

  @Test
  def add {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = newIntVar(-100, 100)

      add(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions

    assertEquals(100, solutions.size)

    assertEquals(Set() ++ (2 to 20), solutions.toSet)

    val simpleCSP2 = new CSPModel[Int] {

      val x = newIntVar(-2 to 2)

      val y = newIntVar(-2 to 2)

      val result = newIntVar(-100, 100)

      add(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions2 = simpleCSP2.findAllSolutions

    assertEquals(List(-4, -3, -3, -2, -2, -2, -1, -1, -1, -1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
      solutions2.sortWith(_ < _))
  }

  @Test
  def addFail {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = c(1)

      add(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions.toSet

    assertTrue(solutions.isEmpty)
  }

  @Test
  def sub {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = newIntVar(-100, 100)

      sub(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions

    assertEquals(100, solutions.size)

    assertEquals(Set() ++ (-9 to 9), solutions.toSet)

    val simpleCSP2 = new CSPModel[Int] {

      val x = newIntVar(-2 to 2)

      val y = newIntVar(-2 to 2)

      val result = newIntVar(-100, 100)

      sub(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions2 = simpleCSP2.findAllSolutions

    assertEquals(List(-4, -3, -3, -2, -2, -2, -1, -1, -1, -1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
      solutions2.sortWith(_ < _))
  }

  @Test
  def subFail {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = c(10)

      sub(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions.toSet

    assertTrue(solutions.isEmpty)
  }

  @Test
  def multTest1 {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = newIntVar(-100, 100)

      mult(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions

    assertEquals(100, solutions.size)

  }

  @Test
  def multTest2 {

    val simpleCSP2 = new CSPModel[Int] {

      val x = newIntVar(0 to 1)

      val y = newIntVar(-1 to 1)

      val result = newIntVar(-1, 1)

      mult(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions2 = simpleCSP2.findAllSolutions

    assertEquals(List(-1) ::: List.fill(4)(0) ::: List(1), solutions2.sortWith(_ < _))

  }

  @Test
  def multTest3 {

    val simpleCSP3 = new CSPModel[Int] {

      val x = newIntVar(-2 to 2)

      val y = newIntVar(-2 to 2)

      val result = newIntVar(-5, 5)

      mult(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val testStore = simpleCSP3.initialStore

    val testProps = simpleCSP3.initialPropagators

    val solutions3 = simpleCSP3.findAllSolutions

    assertEquals(List(-4, -4, -2, -2, -2, -2, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 2, 2, 4, 4), solutions3.sortWith(_ < _))

  }

  @Test
  def multFail {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = c(101)

      mult(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions.toSet

    assertTrue(solutions.isEmpty)
  }
  /*
  @Test
  def divTest1 {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = newIntVar(-100, 100)

      div(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions
    println(solutions)
    assertEquals(27, solutions.size)
  }
  @Test
  def divTest2 {
    val simpleCSP2 = new CSPModel[Int] {

      val x = newIntVar(-1 to 1)

      val y = newIntVar(-1 to 1)

      val result = newIntVar(-1 to 1)

      div(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions2 = simpleCSP2.findAllSolutions

    assertEquals(List(-1, -1) ::: List.fill(2)(0) ::: List(1, 1), solutions2.sort(_ < _))

  }
*/
  @Test
  def divTest3 {
    val simpleCSP3 = new CSPModel[Int] {

      val x = newIntVar(-2 to 2)

      val y = newIntVar(-2 to 2)

      val result = newIntVar(-5, 5)

      div(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions3 = simpleCSP3.findAllSolutions

    assertEquals(List(-2, -2, -1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2), solutions3.sortWith(_ < _))

  }

  @Test
  def divFail {

    val simpleCSP = new CSPModel[Int] {

      val x = newIntVar(1 to 10)

      val y = newIntVar(1 to 10)

      val result = c(101)

      mult(x, y, result)

      def solutionStoreToSolution(s: Store) = {

        s(result).value
      }
    }

    val solutions = simpleCSP.findAllSolutions.toSet

    assertTrue(solutions.isEmpty)
  }

  @Test
  def testModel {

    val simpleCSP = new BasicCSPModel {

      //Vars
      val a = newIntVar(1, 5)
      val b = newIntVar(1, 5)
      val c = newIntVar(8, 10)
      
      //Constrainsts
      satisfy((a + b) === c)

      println(initialPropagators)
      println(initialStore)
      override def solutionStoreToSolution(st: Store) = {
        val av = st(a).value
        val bv = st(b).value
        val cv = st(c).value

        assertTrue(av + bv == cv)

        st
      }
    }

    val solutions = simpleCSP.findAllSolutions

    assertEquals(solutions.size, 6)
  }

  @Test
  def sendMoreMoney {

    val simpleCSP = new BasicCSPModel {

      //Vars
      val vars = List.fill(8)(newIntVar(0 to 9))
      val List(s, e, n, d, m, o, r, y) = vars

      //Constrainsts
      distinct(vars)
      satisfy(s !== c(0))
      satisfy(m !== c(0))
      satisfy((s * c(1000) + e * c(100) + n * c(10) + d +
        m * c(1000) + o * c(100) + r * c(10) + e) ===
        (m * c(10000) + o * c(1000) + n * c(100) + e * c(10) + y))

      override def solutionStoreToSolution(st: Store) = {
        //print(List(s, e, n, d).map(st(_).value).mkString("") + " + ")
        //print(List(m, o, r, e).map(st(_).value).mkString("") + " = ")
        //print(List(m, o, n, e, y).map(st(_).value).mkString(""))
        //println()
        val send = List(s, e, n, d).map(st(_).value).mkString("").toInt
        val more = List(m, o, r, e).map(st(_).value).mkString("").toInt
        val money = List(m, o, n, e, y).map(st(_).value).mkString("").toInt

        assertTrue((send + more) == money)
        st
      }
    }

    val solutions = simpleCSP.findAllSolutions

    assertEquals(solutions.size, 1)
  }
}