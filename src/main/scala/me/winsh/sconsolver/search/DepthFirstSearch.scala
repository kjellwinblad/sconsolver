package me.winsh.sconsolver.search

import me.winsh.sconsolver.core._

object DepthFirstSearch extends SearchMethod {

  def find[R](
    propagators: List[Propagator],
    store: Store,
    branching: Branching,
    numberOfSolutions: Int = -1,
    f: (List[R], Store) => List[R] = ((l: List[R], s: Store) => s :: l),
    constrain: (Store) => (List[Propagator], List[(Var, Domain)]) = ((s: Store) => (Nil, Nil))): List[R] = {

    case class EnoughSolutionsException(solutions: List[R]) extends Exception

    def find(
      propagators: List[Propagator],
      store: Store,
      resultSoFar: List[R],
      numberOfSolutions: Int): (List[R], Int) = {

      val (newPropagators, newStore1) = Propagate.propagate(propagators, store)

      if (newStore1.failed)
        (resultSoFar, numberOfSolutions)
      else if (newStore1.fixPoint) {
        if (numberOfSolutions == 1)
          throw EnoughSolutionsException(f(resultSoFar, newStore1))
        else
          (f(resultSoFar, newStore1), numberOfSolutions - 1)
      } else {

        val (branches, newStore2) = branching.branches(newPropagators, newStore1)

        def solutionsInBranches(
          branches: List[List[Propagator]],
          bSolutionsSoFar: List[R],
          numberOfSolutions: Int): (List[R], Int) = branches match {
          case Nil => (bSolutionsSoFar, numberOfSolutions)
          case bProps :: rest => {

            val (solutionsInBranch, newNumberOfSolutions) =
              find(bProps ::: newPropagators, newStore2, Nil, numberOfSolutions)

            solutionsInBranches(rest, solutionsInBranch ::: bSolutionsSoFar, newNumberOfSolutions)

          }
        }

        solutionsInBranches(branches, Nil, numberOfSolutions)
      }

    }

    val solutions = try {
      val (solutions, _) = find(propagators, store, Nil, numberOfSolutions)
      solutions
    } catch {
      case EnoughSolutionsException(solutions) => solutions
    }

    solutions
  }

}