package me.winsh.sconsolver.core

trait SearchMethod {

  def findFirstSolution[R](propagators: List[Propagator],
    store: Store,
    branching: Branching,
    f: (Store) => R = ((s: Store) => s)): Option[R] = {

    val solutionTransformation =
      ((l: List[R], s: Store) => f(s) :: Nil)

    val solList = find[R](propagators, store, branching, 1, solutionTransformation)

    solList match {
      case Nil => None
      case sol :: _ => Some(sol)
    }

  }

  def findBestSolution[R](
    propagators: List[Propagator],
    store: Store,
    branching: Branching,
    constrain: (Store) => (List[Propagator], List[(Var, Domain)]),
    f: (Store) => R = ((s: Store) => s)): Option[R] = {

    val solutionTransformation =
      ((l: List[R], s: Store) => f(s) :: Nil)

    val solList = find[R](propagators, store, branching, -1, solutionTransformation, constrain)

    solList match {
      case Nil => None
      case sol :: _ => Some(sol)
    }

  }

  def findAllSolutions[R](
    propagators: List[Propagator],
    store: Store,
    branching: Branching,
    f: (List[R], Store) => List[R] = ((l: List[R], s: Store) => s :: l)): List[R] = {

    find[R](propagators, store, branching, -1, f)

  }

  def find[R](
    propagators: List[Propagator],
    store: Store,
    branching: Branching,
    numberOfSolutions: Int = 0,
    f: (List[R], Store) => List[R] = ((l: List[R], s: Store) => s :: l),
    constrain: (Store) => (List[Propagator], List[(Var, Domain)]) = ((s: Store) => (Nil, Nil))): List[R]
}