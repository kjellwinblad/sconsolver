package me.winsh.sconsolver.propagators

import me.winsh.sconsolver.core._

abstract class DistinctAlgorithmType

case class Naive extends DistinctAlgorithmType

class DistinctPropagator(val vars:List[Var], algorithmType:DistinctAlgorithmType = Naive()) extends Propagator{
	
	val parameters:List[Var] = vars  
	
	def propagate(s:Store) = algorithmType match {
		case Naive() =>  propagateNaive(s)
	}
	
	private def propagateNaive(s:Store) = {
		
		def single(d:Domain) = if(d.fixPoint) d else Domain()
		
		val parametersArray = parameters
		
		val domainList = parametersArray.map((v)=>s(v))
		
		val subtractList = domainList.map((d)=>single(d)).zipWithIndex
		
		val newDomainList = domainList.zipWithIndex.map((domainIndex)=>{
			val (domain, index) = domainIndex
			
			val withoutThisDomain = subtractList.filterNot((e)=>e._2==index).map(_._1)
			
			val subtractStore = withoutThisDomain.foldLeft(Domain())((sum, d)=>sum.union(d))
			
			(domain.difference(subtractStore), index)
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