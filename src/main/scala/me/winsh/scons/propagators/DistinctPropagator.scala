package me.winsh.scons.propagators

import me.winsh.scons.core._

abstract class DistinctAlgorithmType

case class Naive extends DistinctAlgorithmType

class DistinctPropagator(val vars:List[Var], algorithmType:DistinctAlgorithmType = Naive()) extends Propagator{
	
	val parameters:List[Var] = vars 
	
	def propagate(s:Store) = algorithmType match {
		case Naive() =>  propagateNaive(s)
	}
	
	private def propagateNaive(s:Store) = {
		
		def single(d:Domain) = if(d.fixPoint) d else Domain()
		
		val parametersArray = parameters.toArray
		
		val domainList = parametersArray.map((v)=>s(v))
		
		val subtractList = domainList.map((d)=>single(d))
		
		val subtractSet = subtractList.foldLeft(Domain())((domainSum,d)=>domainSum.union(d))
		
		val newDomainList = domainList.zipWithIndex.map((domainIndex)=>{
			val (domain, index) = domainIndex
			
			(domain.difference(subtractSet.difference(subtractList(index))), index)
		})
		
		val newStore = newDomainList.foldLeft(s)((newS,domainIndex)=>{
			val (newDomain, index) = domainIndex
			newS(parametersArray(index),newDomain)
		})
		
		if(newDomainList.exists((d)=>d._1.failed))
			(Failed(), newStore)
		else
			(NoFixPoint(), newStore)

	}
}