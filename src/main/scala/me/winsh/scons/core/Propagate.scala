package me.winsh.scons.core

import scala.collection.immutable.HashSet
object Propagate {

	def propagate(propagatorSet:Set[Propagator], store:Store) ={
		
		def p(props:Set[Propagator], subsumedProps:Set[Propagator], s:Store):(Set[Propagator], Store) =
			if(props.isEmpty) 
				(propagatorSet.diff(subsumedProps), s)
			else {
			 
				val prop = props.first
				 
				val(propMessage, newS) = prop.propagate(s)
				
				val newProps1 = props - prop
				 
				val (newSubsumedProps, removedPropList) =  propMessage match {
					case Subsumed() => (subsumedProps - prop, List(prop))
					case _ => (subsumedProps, Nil)
				}
				
				val changedVars = s.changedVars(newS)
			
				val effectedProps = (props--removedPropList).filter((propToFilter)=>{
						propToFilter.parameters.exists((parVar)=>changedVars.contains(parVar))
					})--(if(propMessage==FixPoint()) List(prop) else Nil)
				
				val newProps2 = newProps1.union(effectedProps)
					
				p(newProps2, newSubsumedProps,newS)
			}
		
			p(propagatorSet, HashSet[Propagator](), store)
	}
	
	
}