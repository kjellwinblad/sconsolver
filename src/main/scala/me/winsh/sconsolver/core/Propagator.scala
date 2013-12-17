package me.winsh.sconsolver.core

abstract class PropagatorMessage

case object Subsumed extends PropagatorMessage

case object FixPoint extends PropagatorMessage

case object NoFixPoint extends PropagatorMessage

case object Failed extends PropagatorMessage

trait Propagator {

  val parameters: List[Var]

  def propagate(s: Store): (PropagatorMessage, Store)

  override def equals(that: Any) = {
    val thatP = that.asInstanceOf[Propagator]
    thatP.parameters == this.parameters && this.getClass.getName == thatP.getClass.getName
  }

  override def hashCode = parameters.hashCode  + this.getClass.getName.hashCode

  override def toString = this.getClass.getName + "(" + parameters.mkString(", ") + ")"

}
