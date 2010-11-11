package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._
import org.junit._
import Assert._

@Test
class DistinctTest {

  @Test
  def testFail {

    val solver = new CSPModel {

      val varList = List.fill(3)(newIntVar(1 to 2))

      distinct(varList)

    }

    solver.findFirstSolution match {
      case Some(s) => fail("The model is not consistent so no solution can be found.")
      case None => Unit
    }

    val solver2 = new CSPModel {

      val varList = List.fill(2)(c(1))

      distinct(varList)

    }

    solver2.findFirstSolution match {
      case Some(s) => fail("The model is not consistent so no solution can be found.")
      case None => Unit
    }

  }

  @Test
  def testSuccess {

    val solver = new CSPModel {

      val varList = List.fill(3)(newIntVar(1 to 3))

      distinct(varList)

    }

    solver.findFirstSolution match {
      case Some(s) => Unit
      case None => fail("The model should not fail here.")
    }

    val solver2 = new CSPModel {

      val varList = List.fill(2)(newIntVar(1 to 2))

      distinct(varList)

    }

    solver2.findFirstSolution match {
      case Some(s) => Unit
      case None => fail("The model should not fail here.")
    }

  }

  @Test
  def sudokuTest {

    //Beatufier for Soduku Strings
    def formatCompactSudokuString(s: String) =
      s.toCharArray.map(_ + " ").foldLeft("")((sum, e) => sum + e)

    //Type representing a soduku matrix
    type SudokuBoard = List[List[Option[Int]]]

    //Helper method that transform a Soduku String to a SodukuBoard
    def sudokuStringToBoard(s: String): SudokuBoard = {

      val preparedRows =
        s.split("\n").map(_.trim).filter(_ != "").map(_.toCharArray)

      val sudukoChars =
        Set() ++ ('#' :: (1 to 9).map(_.toString.head).toList)

      val rows =
        preparedRows.map(_.filter(sudukoChars.contains(_))).filter(_ != Array())

      rows.toList.map(_.toList.map((c) => {
        if (c == '#')
          None
        else
          Some(c.toString.toInt)
      }))
    }

    //The Constraint Satisfaction Problem (CSP) for Soduku
    class SudokuSolver(val sudokuBord: SudokuBoard) extends CSPModel {

      //Declare vars representing the soduku matrix
      val rows =
        Array.fill(9)(Array.fill(9)(newIntVar(1 to 9)))

      //Pick out all columns from the rows
      val columns =
        (0 to 8).map((c) => rows.map((row) => row(c))).toArray

      //Pick out the squares
      val squares =
        (for (x <- 0 to 6 by 3; y <- 0 to 6 by 3) yield {
          (y to (y + 2)).flatMap(rows(_).slice(x, x + 3)).toArray
        }).toArray

      //Put all areas in a list
      val areas = List(rows, columns, squares).flatten

      //Define constraints so every value in an area is distinct in the area 
      areas.foreach(distinct(_))

      //Define constraints defined by this particular soduku board instance
      sudokuBord.zipWithIndex.foreach((rowIndex) => {

        val (row, y) = rowIndex

        row.zipWithIndex.foreach((columnIndex) => columnIndex match {
          case (Some(value), x) => c(value) === rows(y)(x)
          case _ => Unit
        })
      })

    }

    {
      val failSoduku =
        """ 
 4 4 # # 9 5 # # 3 
 1 # # 8 4 # # # # 
 # # 5 # # 7 8 4 2 
 # 1 7 4 # 8 2 3 # 
 # # # # 2 # # # # 
 # 9 4 7 # 1 5 6 # 
 3 4 2 9 # # 1 # # 
 # # # # 8 4 # # 6 
 8 # # 3 7 # # # 4
"""
      val solverForFailSoduku =
        new SudokuSolver(sudokuStringToBoard(failSoduku))

      solverForFailSoduku.findFirstSolution match {
        case Some(s) => {

          val solution = (for (row <- solverForFailSoduku.rows) yield row.map(s(_).min).mkString(" ") + "\n").foldLeft("")(_ + _)

          fail("There should not be a solution here " + solution)

        }
        case None => Unit
      }
    }

    {
      val simpleSoduku =
        """ 
 4 # # # 9 5 # # 3 
 1 # # 8 4 # # # # 
 # # 5 # # 7 8 4 2 
 # 1 7 4 # 8 2 3 # 
 # # # # 2 # # # # 
 # 9 4 7 # 1 5 6 # 
 3 4 2 9 # # 1 # # 
 # # # # 8 4 # # 6 
 8 # # 3 7 # # # 4
"""
      val solverForSimpleSoduku =
        new SudokuSolver(sudokuStringToBoard(simpleSoduku))

      solverForSimpleSoduku.findFirstSolution match {
        case Some(s) => {

          val solution = (for (row <- solverForSimpleSoduku.rows) yield row.map(s(_).min).mkString(" ") + "\n").foldLeft("")(_ + _)

          assertEquals(
            """
4 7 8 2 9 5 6 1 3
1 2 6 8 4 3 7 9 5
9 3 5 6 1 7 8 4 2
5 1 7 4 6 8 2 3 9
6 8 3 5 2 9 4 7 1
2 9 4 7 3 1 5 6 8
3 4 2 9 5 6 1 8 7
7 5 9 1 8 4 3 2 6
8 6 1 3 7 2 9 5 4
""", "\n" + solution)

        }
        case None => fail("There should be solutions here")
      }
    }
    {
      val mediumSoduku =
        """ 
 # # # 5 4 # 8 # 3 
 # # # # # # 6 2 # 
 # # 5 7 # 8 # # # 
 5 # # # 7 # 2 8 # 
 # # 2 8 # 3 1 # # 
 # 6 8 # 5 # # # 9 
 # # # 2 # 1 5 # # 
 # 5 9 # # # # # # 
 2 # 7 # 3 5 # # #
"""
      val solverForMediumSoduku =
        new SudokuSolver(sudokuStringToBoard(mediumSoduku))

      solverForMediumSoduku.findFirstSolution match {
        case Some(s) => {

          val solution = (for (row <- solverForMediumSoduku.rows) yield row.map(s(_).min).mkString(" ") + "\n").foldLeft("")(_ + _)

          assertEquals(
            """
9 2 1 5 4 6 8 7 3
8 7 4 3 1 9 6 2 5
6 3 5 7 2 8 4 9 1
5 1 3 9 7 4 2 8 6
7 9 2 8 6 3 1 5 4
4 6 8 1 5 2 7 3 9
3 8 6 2 9 1 5 4 7
1 5 9 4 8 7 3 6 2
2 4 7 6 3 5 9 1 8
""", "\n" + solution)

        }
        case None => fail("There should be solutions here")
      }
    }

    {
      val dificuldSoduku =
        """ 
 # # 9 # # # # # # 
 6 # # # # 4 # # 1 
 # 5 # # 2 8 # # # 
 # # # # # 2 # 3 # 
 # 4 # 3 # # # 9 # 
 2 # # # # # # # 6 
 1 # # # 8 # 7 # 4 
 # 8 # # # # # 2 # 
 # 9 # # 5 1 # # #
"""
      val solverForDificuldSoduku =
        new SudokuSolver(sudokuStringToBoard(dificuldSoduku))

      solverForDificuldSoduku.findFirstSolution match {
        case Some(s) => {

          val solution = (for (row <- solverForDificuldSoduku.rows) yield row.map(s(_).min).mkString(" ") + "\n").foldLeft("")(_ + _)

          assertEquals(
            """
8 7 9 1 3 5 6 4 2
6 2 3 7 9 4 5 8 1
4 5 1 6 2 8 9 7 3
9 1 6 8 7 2 4 3 5
5 4 8 3 1 6 2 9 7
2 3 7 5 4 9 8 1 6
1 6 2 9 8 3 7 5 4
3 8 5 4 6 7 1 2 9
7 9 4 2 5 1 3 6 8
""", "\n" + solution)

        }
        case None => fail("There should be solutions here")
      }
    }
  }
}