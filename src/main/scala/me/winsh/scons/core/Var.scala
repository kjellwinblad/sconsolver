package me.winsh.scons.core

trait Var {

	val id:Int
	
	override def equals (that: Any) = that.asInstanceOf[Var].id == this.id
	
	override def hashCode = id.hashCode
	
	override def toString = "Var(" + id +")"
}

trait IntVar extends Var{
	def <=(that:IntVar):Propagator
	def >=(that:IntVar):Propagator
	def <(that:IntVar):Propagator
	def >(that:IntVar):Propagator
	def ===(that:IntVar):Propagator
	def !==(that:IntVar):Propagator
	
}
object Var {
	
	
  def apply(): Var = { 

    new VarImpl(getNextVarIdAndIncreaseCounter())

  }
	
	private class VarImpl(idP:Int) extends Var {
		
		val id = idP

	}
	
}