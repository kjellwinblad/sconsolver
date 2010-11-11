package me.winsh.sconsolver.core

import scala.collection.immutable.HashSet




object Propagate {

	def propagate(propagators:List[Propagator], store:Store) ={
		
		def p(props:List[Propagator], 
			  sleepingProps:List[Propagator],
			  subsumedProps:List[Propagator], 
			  s:Store):(List[Propagator], Store) =
			if(props.isEmpty) 
				(propagators.diff(subsumedProps), s)
			else if(s.failed)
				(propagators.diff(subsumedProps), s)
			else {
			 
				val prop::propsRest = props
				 
				val(propMessage, newS) = prop.propagate(s)
				
				val (newSubsumedProps, newSleepingProps1, newProps1) =  propMessage match {
					case Subsumed() => (prop::subsumedProps, sleepingProps, propsRest)
					case FixPoint() => (subsumedProps, prop::sleepingProps, propsRest) 
					case NoFixPoint() if(s==newS)=> (subsumedProps, sleepingProps, propsRest)
					case NoFixPoint() => (subsumedProps, sleepingProps, props)
					case Failed() => (subsumedProps, sleepingProps, propsRest) 
				}
				
				val changedVars = s.changedVars(newS)
			
				val effectedProps = sleepingProps.filter((propToFilter)=>{
						propToFilter.parameters.exists((parVar)=>changedVars.contains(parVar))
					})
				
				val newProps2 = effectedProps:::newProps1
				
				val newSleepingProps2 = newSleepingProps1.diff(effectedProps)
				
				p(newProps2, newSleepingProps2, newSubsumedProps,newS)
			}
		
			p(propagators, List[Propagator](), List[Propagator](), store)
	}
	
	
}