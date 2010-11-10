/**
 * 
 */
package me.winsh.sconsolver.core

import scala.collection.immutable.Iterable

/**
 * @author Kjell Winblad
 *
 */
trait Domain extends Iterable[Int] {

  protected val domainRanges: List[Range]

  val min: Int

  val max: Int 

  def union(that: Domain): Domain

  def intersection(that: Domain): Domain

  def difference(that: Domain): Domain

  def onlyOneContains(that: Domain): Domain

  def domainWithSmallestMinFirst(that: Domain): (Domain, Domain)
  
  def greaterThan(number:Int):Domain

  def lessThan(number:Int):Domain

  def greaterThanOrEqual(number:Int):Domain

  def lessThanOrEqual(number:Int):Domain
  
  def fixPoint:Boolean = this.size==1
	
  def failed:Boolean = this.isEmpty

}

object Domain {

  val empty = this(Nil)

  def apply(min: Int, max: Int): Domain = {
    require(min <= max, "The min parameter is bigger than the max parameter")

    new DomainImpl(List(min to max))
  }

  def apply(domainValues: Iterable[Int]): Domain = {

    val domainValuesList = domainValues.toList.sort(_ < _)

    val ranges = domainValuesList.foldLeft(Nil: List[Range])((rangesList, value) => rangesList match {
      case Nil => List(value to value)
      case e :: ls if e.max == value => e :: ls
      case e :: ls if e.max + 1 == value => (e.min to value) :: ls
      case ls => (value to value) :: ls
    }).reverse

    new DomainImpl(ranges)
  }

  def apply(domainValues: Int*): Domain = apply(domainValues.toList)

  private class DomainImpl(val domainRanges: List[Range]) extends Domain {

    lazy val min = {
      require(domainRanges != Nil)
      domainRanges.first.min
    }

    lazy val max = {
      require(domainRanges != Nil)
      domainRanges.last.max
    }
 
    def union(that: Domain): Domain =
      if(this.isEmpty)
    	  that
      else if(that.isEmpty)
    	  this 
      else if (this.max < that.min)
        new DomainImpl(concatenateOrderedRangesIfAdjecent(this.domainRanges ++ that.domainRanges))
      else if (that.max < this.min)
        new DomainImpl(concatenateOrderedRangesIfAdjecent(that.domainRanges ++ this.domainRanges))
      else { 

        def union(subtractFrom: List[Range],
          subtract: List[Range],
          result: List[Range]): Domain = (subtractFrom, subtract) match {
          case (Nil, domain) =>
            new DomainImpl(concatenateOrderedRangesIfAdjecent((domain.reverse ::: result).reverse))
          case (domain, Nil) =>
            new DomainImpl(concatenateOrderedRangesIfAdjecent((domain.reverse ::: result).reverse))
          case (set1First :: set1Second :: set1Rest, set2) if (rangesHasIntersection(set1First, set1Second)) =>
            union((set1First.min.min(set1Second.min) to set1First.max.max(set1Second.max)) :: set1Rest, set2, result)
          case (set1First :: set1Rest, set2First :: set2Rest) if (set1First.max < set2First.min) =>
            union(set1Rest, set2First :: set2Rest, set1First :: result)
          case (set1First :: set1Rest, set2First :: set2Rest) if (set2First.max < set1First.min) =>
            union(set1First :: set1Rest, set2Rest, set2First :: result)
          case (set1First :: set1Rest, set2First :: set2Rest) =>
            union((set1First.min.min(set2First.min) to set1First.max.max(set2First.max)) :: set1Rest, set2Rest, result)

        }

        union(this.domainRanges, that.domainRanges, Nil)

      }

    def intersection(that: Domain): Domain =
      if (this == empty || that == empty)
        empty
      else if (this.max < that.min)
        empty
      else if (that.max < this.min)
        empty
      else {
        //They may be intersecting in some way 
        //Find out which ranges are intersecting and change them

        def intersectionDomain(smallestMinDomainbigestMinDomain: (Domain, Domain),
          result: List[Range]): Domain = smallestMinDomainbigestMinDomain match {
          case (Domain.empty, _) => new DomainImpl(result.reverse)
          case (_, Domain.empty) => new DomainImpl(result.reverse)
          case (smallestMinDomain, bigestMinDomain) => {
            val checkRanges = smallestMinDomain.domainRanges

            val firstInBigestMinDomain = bigestMinDomain.domainRanges.first

            val checkedRanges =
              checkRanges.dropWhile((range) => range.min < firstInBigestMinDomain.min &&
                !rangesHasIntersection(range, firstInBigestMinDomain))

            checkedRanges match {
              case Nil => new DomainImpl(result.reverse)
              case intersectingRange :: rest if (rangesHasIntersection(intersectingRange, firstInBigestMinDomain)) => {

                val newResultRange = rangeIntersection(intersectingRange, firstInBigestMinDomain)

                //Do recursive call with the intersecting range droped in in
                //firstElementIntersectingRanges and in bigestMinDomain

                val newDomain1 = intersectingRange.max match {
                  case value if (value == newResultRange.max) => new DomainImpl(checkedRanges.drop(1))
                  case _ => new DomainImpl(((newResultRange.max + 1) to intersectingRange.max) :: checkedRanges.drop(1))
                }

                val newDomain2 = firstInBigestMinDomain.max match {
                  case value if (value == newResultRange.max) => new DomainImpl(bigestMinDomain.domainRanges.drop(1))
                  case _ => new DomainImpl(((newResultRange.max + 1) to firstInBigestMinDomain.max) :: bigestMinDomain.domainRanges.drop(1))
                }

                intersectionDomain(newDomain1.domainWithSmallestMinFirst(newDomain2),
                  newResultRange :: result)
              }
              case _ => {

                //In this case it is not certain that checkedRanges don't intersect with
                //the second range in bigestMinDomain.domainRanges so we can just drop the
                //first element in bigestMinDomain.domainRanges

                val newDomain1 = new DomainImpl(checkedRanges)

                val newDomain2 = new DomainImpl(bigestMinDomain.domainRanges.drop(1))

                intersectionDomain(newDomain1.domainWithSmallestMinFirst(newDomain2),
                  result)

              }
            }
          }
        }

        intersectionDomain(this.domainWithSmallestMinFirst(that), Nil)

      }

    def difference(that: Domain): Domain =
      if (this == empty ||
        that == empty ||
        this.max < that.min ||
        that.max < this.min)
        this
      else {
        def difference(subtractFrom: List[Range],
          subtract: List[Range],
          result: List[Range]): Domain = (subtractFrom, subtract) match {
          case (Nil, domain) =>
            new DomainImpl(result.reverse)
          case (domain, Nil) =>
            new DomainImpl((domain.reverse ::: result).reverse)
          case (subtractFrom :: subtractFromRest, subtract :: subtractRest) if (subtractFrom.min > subtract.max) =>
            difference(subtractFrom :: subtractFromRest, subtractRest, result)
          case (subtractFrom :: subtractFromRest, subtract :: subtractRest) if (subtractFrom.max < subtract.min) =>
            difference(subtractFromRest, subtract :: subtractRest, subtractFrom :: result)
          case (subtractFrom :: subtractFromRest, subtract :: subtractRest) =>
            (subtractFrom, subtract) match {

              case (f, s) if (f.min >= s.min && s.max > f.max) =>
                difference(subtractFromRest, (f.max + 1 to s.max) :: subtractRest, result)
              case (f, s) if (f.min >= s.min && s.max == f.max) =>
                difference(subtractFromRest, subtractRest, result)
              case (f, s) if (f.min >= s.min && s.max < f.max) =>
                difference((s.max + 1 to f.max) :: subtractFromRest, subtractRest, result)
              case (f, s) if (f.min < s.min && s.max > f.max) =>
                difference(subtractFromRest, (f.max + 1 to s.max) :: subtractRest, (f.min to s.min - 1) :: result)
              case (f, s) if (f.min < s.min && s.max == f.max) =>
                difference(subtractFromRest, subtractRest, (f.min to s.min - 1) :: result)
              case (f, s) if (f.min < s.min && s.max < f.max) =>
                (f.min to s.min - 1) :: Nil
                difference((s.max + 1 to f.max) :: subtractFromRest, subtractRest, (f.min to s.min - 1) :: result)

            }

        }

        difference(this.domainRanges, that.domainRanges, Nil)
      }

    def onlyOneContains(that: Domain): Domain =
      if (this == empty)
        that
      else if (that == empty)
        this
      else if (this.max < that.min)
        new DomainImpl(concatenateOrderedRangesIfAdjecent(this.domainRanges ::: that.domainRanges))
      else if (that.max < this.min)
        new DomainImpl(concatenateOrderedRangesIfAdjecent(that.domainRanges ::: this.domainRanges))
      else {
        //They may be intersecting in some way 
        //Find out which ranges are intersecting and change them

        def onlyOneContains(smallestMinDomainbigestMinDomain: (Domain, Domain),
          result: List[Range]): Domain = smallestMinDomainbigestMinDomain match {
          case (Domain.empty, domain) =>
            new DomainImpl(concatenateOrderedRangesIfAdjecent((domain.domainRanges.reverse ::: result).reverse))
          case (domain, Domain.empty) =>
            new DomainImpl(concatenateOrderedRangesIfAdjecent((domain.domainRanges.reverse ::: result).reverse))
          case (smallestMinDomain, bigestMinDomain) => {
            val checkRanges = smallestMinDomain.domainRanges

            val firstInBigestMinDomain = bigestMinDomain.domainRanges.first

            val (differ, mayDiffer) =
              checkRanges.span((range) => range.min < firstInBigestMinDomain.min &&
                !rangesHasIntersection(range, firstInBigestMinDomain))

            mayDiffer match {
              case Nil => new DomainImpl((differ.reverse ::: result).reverse)
              case intersectingRange :: rest if (intersectingRange.min == firstInBigestMinDomain.min &&
                intersectingRange.max == firstInBigestMinDomain.max) => {

                val newDomain1 = new DomainImpl(mayDiffer.drop(1))

                val newDomain2 = new DomainImpl(bigestMinDomain.domainRanges.drop(1))

                onlyOneContains(newDomain1.domainWithSmallestMinFirst(newDomain2),
                  differ.reverse ::: result)
              }
              case intersectingRange :: rest if (rangesHasIntersection(intersectingRange, firstInBigestMinDomain)) => {

                val newResultRanges = (intersectingRange, firstInBigestMinDomain) match {
                  case (r1, r2) if (r1.min == r2.min) => (r1.max.min(r2.max) + 1 to r1.max.max(r2.max)) :: Nil
                  case (r1, r2) if (r1.max == r2.max) => (r1.min.min(r2.min) to r1.min.max(r2.min) - 1) :: Nil
                  case (r1, r2) => (r1.max.min(r2.max) + 1 to r1.max.max(r2.max)) :: (r1.min.min(r2.min) to r1.min.max(r2.min) - 1) :: Nil
                }

                val maxNewResultRange = newResultRanges.first
                //Do recursive call with the intersecting range droped in in
                //firstElementIntersectingRanges and in bigestMinDomain

                val (newResultRangesToReturn, newDomain1, newDomain2) =
                  (intersectingRange.max, firstInBigestMinDomain.max) match {
                    case (iMax, fMax) if (iMax == fMax) =>
                      (newResultRanges,
                        new DomainImpl(mayDiffer.drop(1)),
                        new DomainImpl(bigestMinDomain.domainRanges.drop(1)))
                    case (iMax, fMax) if (iMax > fMax) =>
                      (if (newResultRanges.size == 2) newResultRanges.drop(1) else Nil,
                        new DomainImpl(maxNewResultRange :: mayDiffer.drop(1)),
                        new DomainImpl(bigestMinDomain.domainRanges.drop(1)))
                    case _ =>
                      (if (newResultRanges.size == 2) newResultRanges.drop(1) else Nil,
                        new DomainImpl(mayDiffer.drop(1)),
                        new DomainImpl(maxNewResultRange :: bigestMinDomain.domainRanges.drop(1)))
                  }

                onlyOneContains(newDomain1.domainWithSmallestMinFirst(newDomain2),
                  newResultRangesToReturn ::: differ.reverse ::: result)
              }
              case _ => {

                val newDomain1 = new DomainImpl(mayDiffer)

                val newDomain2 = new DomainImpl(bigestMinDomain.domainRanges)

                onlyOneContains(newDomain1.domainWithSmallestMinFirst(newDomain2),
                  differ.reverse ::: result)

              }
            }
          }
        }

        onlyOneContains(this.domainWithSmallestMinFirst(that), Nil)

      }
    
      def greaterThan(number:Int) = {
    	  
    	  val ranges =domainRanges.dropWhile((r)=>(r.max < number))
    	  
    	  ranges match {
    	 	  case Nil => new DomainImpl(Nil) 
    	 	  case e::rest if( e.min > number) => new DomainImpl(e::rest)
    	 	  case e::rest => new DomainImpl((number +1 to e.max)::rest) 
    	  }
      }
    	  
    	  

      def lessThan(number:Int) = {
    	  
    	  val ranges =(domainRanges.takeWhile((r)=>(r.min < number))).reverse
    	  
    	  new DomainImpl((ranges match {
    	 	  case Nil => Nil 
    	 	  case e::rest if( e.max < number) => e::rest
    	 	  case e::rest => (e.min to number -1)::rest
    	  }).reverse)
      }

      def greaterThanOrEqual(number:Int) = greaterThan(number - 1)

      def lessThanOrEqual(number:Int) = lessThan(number +1)
    

      override def toString = "Domain(" + domainRanges.map((d)=>(""+(if(d.min == d.max) d.min 
    		  									        else  d.min + " to " + d.max))).mkString(", ") + ")"
      
    def domainWithSmallestMinFirst(that: Domain) = (this, that) match {
      case (Domain.empty, _) => (this, that)
      case (_, Domain.empty) => (this, that)
      case _ => if (this.min <= that.min) (this, that) else (that, this)
    }

    def iterator = new Iterator[Int] {

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

      def hasNext =  domainRanges.size > domainRangesPos

    }

    override def equals(that: Any): Boolean = {
      that.isInstanceOf[Domain] && this.domainRanges == that.asInstanceOf[Domain].domainRanges
    }

    //Private helper functions

    private def concatenateOrderedRangesIfAdjecent(ranges: List[Range]) = {
      def concatenateRangesIfAdjecentInternal(ranges: List[Range],
        result: List[Range]): List[Range] = ranges match {
        case Nil => result.reverse
        case e :: Nil => (e :: result).reverse
        case e1 :: e2 :: rest if e1.max == (e2.min - 1) =>
          concatenateRangesIfAdjecentInternal((e1.min to e2.max) :: rest, result)
        case e1 :: rest =>
          concatenateRangesIfAdjecentInternal(rest, e1 :: result)

      }
      concatenateRangesIfAdjecentInternal(ranges, Nil)
    }

    private def rangeIntersection(range1: Range, range2: Range) =
      if (rangesHasIntersection(range1, range2))
        range1.min.max(range2.min) to range1.max.min(range2.max)
      else
        emptyRange

    private val emptyRange = 0 until 0

    private def rangesHasIntersection(range1: Range, range2: Range) =
      range1.contains(range2.min) ||
        range1.contains(range2.max) ||
        range2.contains(range1.min) ||
        range2.contains(range1.max)
  }

}

/**
 * @author Kjell Winblad
 *
 */

