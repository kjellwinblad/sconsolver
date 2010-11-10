package me.winsh.sconsolver.core

import scala.collection.immutable.HashSet




object Propagate {

	def propagate(propagatorSet:Set[Propagator], store:Store) ={
		
		def p(props:List[Propagator], 
			  sleepingProps:Set[Propagator],
			  subsumedProps:Set[Propagator], 
			  s:Store):(Set[Propagator], Store) =
			if(props.isEmpty) 
				(propagatorSet.diff(subsumedProps), s)
			else if(s.failed)
				(propagatorSet.diff(subsumedProps), s)
			else {
			 
				val prop::propsRest = props
				 
				val(propMessage, newS) = prop.propagate(s)
				
				val (newSubsumedProps, newSleepingProps1, newProps1) =  propMessage match {
					case Subsumed() => (subsumedProps + prop, sleepingProps, propsRest)
					case FixPoint() => (subsumedProps, sleepingProps + prop, propsRest) 
					case NoFixPoint() if(s==newS)=> (subsumedProps, sleepingProps, propsRest)
					case NoFixPoint() => (subsumedProps, sleepingProps, props)
					case Failed() => (subsumedProps, sleepingProps, propsRest) 
				}
				
				val changedVars = s.changedVars(newS)
			
				val effectedProps = sleepingProps.filter((propToFilter)=>{
						propToFilter.parameters.exists((parVar)=>changedVars.contains(parVar))
					})
				
				val newProps2 = (HashSet[Propagator]()++newProps1).union(effectedProps).toList
				
				val newSleepingProps2 = newSleepingProps1.diff(effectedProps)
				
				p(newProps2, newSleepingProps2, newSubsumedProps,newS)
			}
		
			p(propagatorSet.toList, HashSet[Propagator](), HashSet[Propagator](), store)
	}
	
	
}