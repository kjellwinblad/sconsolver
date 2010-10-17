/**
 * 
 */
package me.winsh.scons.core

import scala.collection.immutable.Iterable

/**
 * @author Kjell Winblad
 *
 */
trait Domain extends Iterable[Int] {
	
	protected val domainRanges: List[Range]
	
	val min:Int
    val max:Int
    
    def union(that:Domain):Domain
	
}

object Domain {
	
	def apply(min:Int,max:Int):Domain = {
		require(min<=max, "The min parameter is bigger than the max parameter")
		
		new DomainImpl(List(min to max))
	}
	
	def apply(domainValues:Iterable[Int]):Domain = {
		
		val domainValuesList = domainValues.toList.sort(_ < _)
		
		val ranges = domainValuesList.foldLeft(Nil:List[Range])((rangesList, value) => rangesList match{
				case Nil => List(value to value)
				case e::ls if e.max == value =>  e::ls
				case e::ls if e.max == value + 1 =>  (e.min to value)::ls
				case ls => (value to value)::ls
			}).reverse
		
		
		new DomainImpl(ranges)
	}
	
	def apply(domainValues:Int*):Domain = apply(domainValues.toList)

  private class DomainImpl(val domainRanges: List[Range]) extends Domain {

    val min = domainRanges.first.min
    
    val max = domainRanges.last.max

    def union(that:Domain):Domain = 
    	if(this.max < that.min)
    		new DomainImpl(this.domainRanges ++ that.domainRanges)
    	else if(that.max < this.min)
    		new DomainImpl(that.domainRanges ++ this.domainRanges)
    	else{
    		//They are intersecting in some way 
    		//Find out which ranges are intersecting and change them
    		
    		
    		new DomainImpl(that.domainRanges ++ this.domainRanges) //TODO
    		
    	}
    	
    
    val iterator = new Iterator[Int] {

      var domainRangesPos = 0
      var rangePos = 0

      def next = {
        val domainRange = domainRanges(domainRangesPos)
        val element = domainRange(rangePos)

        //Increase pos
        if (domainRange.size > 1 + rangePos)
          rangePos = rangePos + 1
        else {
          rangePos = 0
          domainRangesPos = domainRangesPos + 1
        }

        element
      }

      def hasNext = domainRanges.size > domainRangesPos

    }
    
    private def rangeUnion(range1:Range, range2:Range) = 1 to 1	//TODO
    
    private def rangeIntersection(range1:Range, range2:Range) = 1 to 1 //TODO	
    
    private val emptyRange = 0 until 0
    
    private def rangesHasIntersection(range1:Range, range2:Range) =
    	range1.contains(range2.min) || range1.contains(range2.max) ||
    	range2.contains(range1.min) || range2.contains(range1.max)
  }

}

/**
 * @author Kjell Winblad
 *
 */

