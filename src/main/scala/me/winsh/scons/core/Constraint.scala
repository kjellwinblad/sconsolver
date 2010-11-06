package me.winsh.scons.core

trait Constraint{

	def initialStore:Store
	
	def add(propagator:Propagator):Propagator
	
}