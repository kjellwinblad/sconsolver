package me.winsh.sconsolver.core

trait Var {

  val id: Int

  override def equals(that: Any) = that.asInstanceOf[Var].id == this.id

  override def hashCode = id

  override def toString = "Var(" + id + ")"
}

trait IntVar extends Var {
  def <=(that: IntVar): Propagator
  def >=(that: IntVar): Propagator
  def <(that: IntVar): Propagator
  def >(that: IntVar): Propagator
  def ===(that: IntVar): Propagator
  def !==(that: IntVar): Propagator
}

trait MVar extends Var {
  def <=(that: MVar): MVar
  def >=(that: MVar): MVar
  def <(that: MVar): MVar
  def >(that: MVar): MVar
  def ===(that: MVar): MVar
  def !==(that: MVar): MVar
  def +(that: MVar): MVar
  def -(that: MVar): MVar
  def *(that: MVar): MVar
  def /(that: MVar): MVar
}

object Var {
  def apply(): Var = new VarImpl(getNextVarIdAndIncreaseCounter())

  private class VarImpl(idP: Int) extends Var {
    val id = idP
  }
}
