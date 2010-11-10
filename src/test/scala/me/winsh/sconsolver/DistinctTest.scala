package me.winsh.sconsolver

import me.winsh.sconsolver.core._
import me.winsh.sconsolver.propagators._
import org.junit._
import Assert._

@Test
class DistinctTest {

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
    class SudokuSolver(val sudokuBord: SudokuBoard) extends SimpleCSPBase {

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
          case (Some(value), x) => newIntVar(value) === rows(y)(x)
          case _ => Unit
        })
      })

    }

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

    println("Simple Soduku")
    println(simpleSoduku)

    solverForSimpleSoduku.findFirstSolution match {
      case Some(s) => {

        println("Solution:")

        for (row <- solverForSimpleSoduku.rows)
          println(row.map(s(_).min).mkString(" "))

      }
      case None => fail("There should be solutions here")
    }

  }
}