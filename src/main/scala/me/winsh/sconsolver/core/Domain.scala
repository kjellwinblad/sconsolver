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

  val value: Int

  def union(that: Domain): Domain

  def intersection(that: Domain): Domain

  def contains(number: Int): Boolean = !this.intersection(Domain(number)).isEmpty

  def difference(that: Domain): Domain

  def onlyOneContains(that: Domain): Domain

  def domainWithSmalleststartFirst(that: Domain): (Domain, Domain)

  def greaterThan(number: Int): Domain

  def lessThan(number: Int): Domain

  def greaterThanOrEqual(number: Int): Domain

  def lessThanOrEqual(number: Int): Domain

  def fixPoint: Boolean

  def fixPoint(valueP: Int): Boolean = fixPoint && this.value == valueP

  def failed: Boolean = this.isEmpty

}

object Domain {

  val empty = this(Nil)

  def apply(min: Int, max: Int): Domain = {
    require(min <= max, "The start parameter is bigger than the end parameter")

    new DomainImpl(List(min to max))
  }

  def apply(domainValues: Iterable[Int]): Domain = {

    val domainValuesList = domainValues.toList.sortWith(_ < _)

    val ranges = domainValuesList.foldLeft(Nil: List[Range])((rangesList, value) => rangesList match {
      case Nil => List(value to value)
      case e :: ls if e.end == value => e :: ls
      case e :: ls if e.end + 1 == value => (e.start to value) :: ls
      case ls => (value to value) :: ls
    }).reverse

    new DomainImpl(ranges)
  }

  def apply(n: Int): Domain = apply(List(n))
  def apply(): Domain = empty
  //def apply(domainValues: Int*): Domain = apply(domainValues.toList)

  private class DomainImpl(domainRangesParam: List[Range]) extends Domain {

    val domainRanges: List[Range] = domainRangesParam match {
      case r :: Nil if (r.isEmpty) => Nil
      case d => d
    }

    lazy val min = {
      require(domainRanges != Nil)
      domainRanges.head.start
    }

    lazy val max = {
      require(domainRanges != Nil)
      domainRanges.last.end
    }

    lazy val value = {
      require(this.fixPoint, "The domain needs to have a single value in order to have a value.")
      domainRanges.head.start
    }

    def union(that: Domain): Domain =
      if (this.isEmpty)
        that
      else if (that.isEmpty)
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
            union((set1First.start.min(set1Second.start) to set1First.end.max(set1Second.end)) :: set1Rest, set2, result)
          case (set1First :: set1Rest, set2First :: set2Rest) if (set1First.end < set2First.start) =>
            union(set1Rest, set2First :: set2Rest, set1First :: result)
          case (set1First :: set1Rest, set2First :: set2Rest) if (set2First.end < set1First.start) =>
            union(set1First :: set1Rest, set2Rest, set2First :: result)
          case (set1First :: set1Rest, set2First :: set2Rest) =>
            union((set1First.start.min(set2First.start) to set1First.end.max(set2First.end)) :: set1Rest, set2Rest, result)

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

        def intersectionDomain(smalleststartDomainbigeststartDomain: (Domain, Domain),
          result: List[Range]): Domain = smalleststartDomainbigeststartDomain match {
          case (Domain.empty, _) => new DomainImpl(result.reverse)
          case (_, Domain.empty) => new DomainImpl(result.reverse)
          case (smalleststartDomain, bigeststartDomain) => {
            val checkRanges = smalleststartDomain.domainRanges

            val firstInBigeststartDomain = bigeststartDomain.domainRanges.head

            val checkedRanges =
              checkRanges.dropWhile((range) => range.start < firstInBigeststartDomain.start &&
                !rangesHasIntersection(range, firstInBigeststartDomain))

            checkedRanges match {
              case Nil => new DomainImpl(result.reverse)
              case intersectingRange :: rest if (rangesHasIntersection(intersectingRange, firstInBigeststartDomain)) => {

                val newResultRange = rangeIntersection(intersectingRange, firstInBigeststartDomain)

                //Do recursive call with the intersecting range droped in in
                //firstElementIntersectingRanges and in bigeststartDomain

                val newDomain1 = intersectingRange.end match {
                  case value if (value == newResultRange.end) => new DomainImpl(checkedRanges.drop(1))
                  case _ => new DomainImpl(((newResultRange.end + 1) to intersectingRange.end) :: checkedRanges.drop(1))
                }

                val newDomain2 = firstInBigeststartDomain.end match {
                  case value if (value == newResultRange.end) => new DomainImpl(bigeststartDomain.domainRanges.drop(1))
                  case _ => new DomainImpl(((newResultRange.end + 1) to firstInBigeststartDomain.end) :: bigeststartDomain.domainRanges.drop(1))
                }

                intersectionDomain(newDomain1.domainWithSmalleststartFirst(newDomain2),
                  newResultRange :: result)
              }
              case _ => {

                //In this case it is not certain that checkedRanges don't intersect with
                //the second range in bigeststartDomain.domainRanges so we can just drop the
                //first element in bigeststartDomain.domainRanges

                val newDomain1 = new DomainImpl(checkedRanges)

                val newDomain2 = new DomainImpl(bigeststartDomain.domainRanges.drop(1))

                intersectionDomain(newDomain1.domainWithSmalleststartFirst(newDomain2),
                  result)

              }
            }
          }
        }

        intersectionDomain(this.domainWithSmalleststartFirst(that), Nil)

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
          case (subtractFrom :: subtractFromRest, subtract :: subtractRest) if (subtractFrom.start > subtract.end) =>
            difference(subtractFrom :: subtractFromRest, subtractRest, result)
          case (subtractFrom :: subtractFromRest, subtract :: subtractRest) if (subtractFrom.end < subtract.start) =>
            difference(subtractFromRest, subtract :: subtractRest, subtractFrom :: result)
          case (subtractFrom :: subtractFromRest, subtract :: subtractRest) =>
            (subtractFrom, subtract) match {

              case (f, s) if (f.start >= s.start && s.end > f.end) =>
                difference(subtractFromRest, (f.end + 1 to s.end) :: subtractRest, result)
              case (f, s) if (f.start >= s.start && s.end == f.end) =>
                difference(subtractFromRest, subtractRest, result)
              case (f, s) if (f.start >= s.start && s.end < f.end) =>
                difference((s.end + 1 to f.end) :: subtractFromRest, subtractRest, result)
              case (f, s) if (f.start < s.start && s.end > f.end) =>
                difference(subtractFromRest, (f.end + 1 to s.end) :: subtractRest, (f.start to s.start - 1) :: result)
              case (f, s) if (f.start < s.start && s.end == f.end) =>
                difference(subtractFromRest, subtractRest, (f.start to s.start - 1) :: result)
              case (f, s) if (f.start < s.start && s.end < f.end) =>
                (f.start to s.start - 1) :: Nil
                difference((s.end + 1 to f.end) :: subtractFromRest, subtractRest, (f.start to s.start - 1) :: result)

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

        def onlyOneContains(smalleststartDomainbigeststartDomain: (Domain, Domain),
          result: List[Range]): Domain = smalleststartDomainbigeststartDomain match {
          case (Domain.empty, domain) =>
            new DomainImpl(concatenateOrderedRangesIfAdjecent((domain.domainRanges.reverse ::: result).reverse))
          case (domain, Domain.empty) =>
            new DomainImpl(concatenateOrderedRangesIfAdjecent((domain.domainRanges.reverse ::: result).reverse))
          case (smalleststartDomain, bigeststartDomain) => {
            val checkRanges = smalleststartDomain.domainRanges

            val firstInBigeststartDomain = bigeststartDomain.domainRanges.head

            val (differ, mayDiffer) =
              checkRanges.span((range) => range.start < firstInBigeststartDomain.start &&
                !rangesHasIntersection(range, firstInBigeststartDomain))

            mayDiffer match {
              case Nil => new DomainImpl((differ.reverse ::: result).reverse)
              case intersectingRange :: rest if (intersectingRange.start == firstInBigeststartDomain.start &&
                intersectingRange.end == firstInBigeststartDomain.end) => {

                val newDomain1 = new DomainImpl(mayDiffer.drop(1))

                val newDomain2 = new DomainImpl(bigeststartDomain.domainRanges.drop(1))

                onlyOneContains(newDomain1.domainWithSmalleststartFirst(newDomain2),
                  differ.reverse ::: result)
              }
              case intersectingRange :: rest if (rangesHasIntersection(intersectingRange, firstInBigeststartDomain)) => {

                val newResultRanges = (intersectingRange, firstInBigeststartDomain) match {
                  case (r1, r2) if (r1.start == r2.start) => (r1.end.min(r2.end) + 1 to r1.end.max(r2.end)) :: Nil
                  case (r1, r2) if (r1.end == r2.end) => (r1.start.min(r2.start) to r1.start.max(r2.start) - 1) :: Nil
                  case (r1, r2) => (r1.end.min(r2.end) + 1 to r1.end.max(r2.end)) :: (r1.start.min(r2.start) to r1.start.max(r2.start) - 1) :: Nil
                }

                val endNewResultRange = newResultRanges.head
                //Do recursive call with the intersecting range droped in in
                //firstElementIntersectingRanges and in bigeststartDomain

                val (newResultRangesToReturn, newDomain1, newDomain2) =
                  (intersectingRange.end, firstInBigeststartDomain.end) match {
                    case (iend, fend) if (iend == fend) =>
                      (newResultRanges,
                        new DomainImpl(mayDiffer.drop(1)),
                        new DomainImpl(bigeststartDomain.domainRanges.drop(1)))
                    case (iend, fend) if (iend > fend) =>
                      (if (newResultRanges.size == 2) newResultRanges.drop(1) else Nil,
                        new DomainImpl(endNewResultRange :: mayDiffer.drop(1)),
                        new DomainImpl(bigeststartDomain.domainRanges.drop(1)))
                    case _ =>
                      (if (newResultRanges.size == 2) newResultRanges.drop(1) else Nil,
                        new DomainImpl(mayDiffer.drop(1)),
                        new DomainImpl(endNewResultRange :: bigeststartDomain.domainRanges.drop(1)))
                  }

                onlyOneContains(newDomain1.domainWithSmalleststartFirst(newDomain2),
                  newResultRangesToReturn ::: differ.reverse ::: result)
              }
              case _ => {

                val newDomain1 = new DomainImpl(mayDiffer)

                val newDomain2 = new DomainImpl(bigeststartDomain.domainRanges)

                onlyOneContains(newDomain1.domainWithSmalleststartFirst(newDomain2),
                  differ.reverse ::: result)

              }
            }
          }
        }

        onlyOneContains(this.domainWithSmalleststartFirst(that), Nil)

      }

    def greaterThan(number: Int) =
      if (this.isEmpty)
        this
      else {

        val ranges = domainRanges.dropWhile((r) => (r.end <= number))

        ranges match {
          case Nil => new DomainImpl(Nil)
          case e :: rest if (e.start > number) => new DomainImpl(e :: rest)
          case e :: rest => new DomainImpl((number + 1 to e.end) :: rest)
        }
      }

    def lessThan(number: Int) =
      if (this.isEmpty)
        this
      else {

        val ranges = (domainRanges.takeWhile((r) => (r.start < number))).reverse

        new DomainImpl((ranges match {
          case Nil => Nil
          case e :: rest if (e.end < number) => e :: rest
          case e :: rest => (e.start to number - 1) :: rest
        }).reverse)
      }

    def greaterThanOrEqual(number: Int) = greaterThan(number - 1)

    def lessThanOrEqual(number: Int) = lessThan(number + 1)

    override def toString =
      if (isEmpty)
        "Domain()"
      else
        "Domain(" + domainRanges.map((d) => ("" + (if (d.start == d.end) d.start
        else d.start + " to " + d.end))).mkString(", ") + ")"

    def domainWithSmalleststartFirst(that: Domain) = (this, that) match {
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

      def hasNext = (domainRanges.size > domainRangesPos && domainRanges.size != 0)

    }

    def fixPoint: Boolean = domainRanges match {
      case List(range) => range.start == range.end
      case _ => false
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
        case e1 :: e2 :: rest if e1.end == (e2.start - 1) =>
          concatenateRangesIfAdjecentInternal((e1.start to e2.end) :: rest, result)
        case e1 :: rest =>
          concatenateRangesIfAdjecentInternal(rest, e1 :: result)

      }
      concatenateRangesIfAdjecentInternal(ranges, Nil)
    }

    private def rangeIntersection(range1: Range, range2: Range) =
      if (rangesHasIntersection(range1, range2))
        range1.start.max(range2.start) to range1.end.min(range2.end)
      else
        emptyRange

    private val emptyRange = 0 until 0

    private def rangesHasIntersection(range1: Range, range2: Range) =
      range1.contains(range2.start) ||
        range1.contains(range2.end) ||
        range2.contains(range1.start) ||
        range2.contains(range1.end)
  }

}

