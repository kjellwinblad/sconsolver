package me.winsh.scons

import me.winsh.scons.core._
import org.junit._
import Assert._

@Test
class DomainTest {

  @Test
  def iterateOverNewDomain() = {

	   
    {
      val domain = Domain(4, 10)

      val iteratedVals = (for (intVal <- domain) yield intVal).toList

      assertEquals(List(4, 5, 6, 7, 8, 9, 10), iteratedVals)
    }

    //In order with holes
    {
      val domain = Domain(1, 2, 3, 4, 10, 11, 12, 15, 16)

      val iteratedVals = (for (intVal <- domain) yield intVal).toList

      assertEquals(List(1, 2, 3, 4, 10, 11, 12, 15, 16), iteratedVals)
    }

    //Out of order with holes
    {
      val domain = Domain(10, 11, 1, 4, 12, 15, 16, 2, 3)

      val iteratedVals = (for (intVal <- domain) yield intVal).toList

      assertEquals(List(1, 2, 3, 4, 10, 11, 12, 15, 16), iteratedVals)
    }

    //Out of order with holes and duplicates
    {
      val domain = Domain(10, 10, 10, 11, 1, 4, 12, 15, 16, 2, 3, 1, 2)

      val iteratedVals = (for (intVal <- domain) yield intVal).toList

      assertEquals(List(1, 2, 3, 4, 10, 11, 12, 15, 16), iteratedVals)
    }

    //From range
    {
      val domain = Domain(1 to 10 by 2)

      val iteratedVals = (for (intVal <- domain) yield intVal).toList

      assertEquals((1 to 10 by 2).toList, iteratedVals)
    }

  }
  
  @Test
  def equals() = {
	  assertEquals(Domain(1,2,3), Domain(1 to 3))
	  
	  assertEquals(Domain(1,2,3,9,10,11), Domain((1 to 3).union(9 to 11)))
	  
	  assertTrue(Domain(1,2,3,4,9,10,11) != Domain((1 to 3).union(9 to 11)))
  }
  
  @Test
  def intersection() = {
	
	  //Everything intersects
	/*{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(1 to 10)

      assertEquals(domain1, domain1.intersection(domain2))
      
      assertEquals(domain1, domain2.intersection(domain1))
    }
	
		  //A part of simple domains intersects
	{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(5 to 15)

      assertEquals(Domain(5 to 10), domain1.intersection(domain2))
      
      assertEquals(Domain(5 to 10), domain2.intersection(domain1))
    }*/
	
	//Complex intersection 1
	{
	  val set1 = Set() ++ (1 to 10).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)

      assertEquals(Domain(set1.intersect(set2)), domain1.intersection(domain2))
      
      assertEquals(Domain(set1.intersect(set2)), domain2.intersection(domain1))
    }
	
		//Complex intersection 2
	{
	  val set1 = Set() ++ (1 to 15 by 2).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (2 to 7 by 2).union(10 to 16).union(35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)

      assertEquals(Domain(set1.intersect(set2)), domain1.intersection(domain2))
      
      assertEquals(Domain(set1.intersect(set2)), domain2.intersection(domain1))
    }
	  
  
  }
  
      @Test
  def difference() = {
	
	  //Everything intersects
	{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(1 to 10)
      
      assertEquals(Domain(), domain1.difference(domain2))
      
      assertEquals(Domain(), domain2.difference(domain1))
    }
	
		  //A part of simple domains intersects
	{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(5 to 15)

      assertEquals(Domain(1 to 4), domain1.difference(domain2))
      
      assertEquals(Domain(11 to 15), domain2.difference(domain1))
    }
	
	//Complex intersection 1
	{
	  val set1 = Set() ++ (1 to 10).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)
     
      assertEquals(Domain(set1.diff(set2)), domain1.difference(domain2))
      
      assertEquals(Domain(set2.diff(set1)), domain2.difference(domain1))
    } 
	
		//Complex intersection 2
	{
	  val set1 = Set() ++ (1 to 15 by 2).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (2 to 7 by 2).union(10 to 16).union(35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)

      
      assertEquals(Domain(set1.diff(set2)), domain1.difference(domain2))
      
      assertEquals(Domain(set2.diff(set1)), domain2.difference(domain1))
    }
	  
  
  }
  
      
      @Test
  def union() = {
	
	  //Everything intersects
	{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(1 to 10)

      assertEquals(Domain(1 to 10), domain1.union(domain2))
      
      assertEquals(Domain(1 to 10), domain2.union(domain1))
    }
	
		  //A part of simple domains intersects
	{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(5 to 15)

      assertEquals(Domain(1 to 15), domain1.union(domain2))
      
      assertEquals(Domain(1 to 15), domain2.union(domain1))
    }
	
	//Complex intersection 1
	{
	  val set1 = Set() ++ (1 to 10).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)
     
      //assertEquals(Domain(set1.union(set2)), domain1.union(domain2))
      
      assertEquals(Domain(set2.union(set1)), domain2.union(domain1))
    } 
	
		//Complex intersection 2
	{
	  val set1 = Set() ++ (1 to 15 by 2).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (2 to 7 by 2).union(10 to 16).union(35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)

      
      assertEquals(Domain(set1.union(set2)), domain1.union(domain2))
      
      assertEquals(Domain(set2.union(set1)), domain2.union(domain1))
    }
	  
  
  }
  
    @Test
  def onlyOneContains() = {
	
	  //Everything intersects
	{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(1 to 10)

      assertEquals(Domain(), domain1.onlyOneContains(domain2))
      
      assertEquals(Domain(), domain2.onlyOneContains(domain1))
    }
	
		  //A part of simple domains intersects
	{
      val domain1 = Domain(1 to 10)
      
      val domain2 = Domain(5 to 15)

      assertEquals(Domain((1 to 4).union(11 to 15)), domain1.onlyOneContains(domain2))
      
      assertEquals(Domain((1 to 4).union(11 to 15)), domain2.onlyOneContains(domain1))
    }
	
	//Complex intersection 1
	{
	  val set1 = Set() ++ (1 to 10).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)
     
      assertEquals(Domain(set1.diff(set2).union(set2.diff(set1))), domain1.onlyOneContains(domain2))
      
      assertEquals(Domain(set2.diff(set1).union(set1.diff(set2))), domain2.onlyOneContains(domain1))
    } 
	
		//Complex intersection 2
	{
	  val set1 = Set() ++ (1 to 15 by 2).union(30 to 40).union(50 to 60)
      val domain1 = Domain(set1)
      
      val set2 = Set() ++ (2 to 7 by 2).union(10 to 16).union(35 to 37).union(40 to 40).union(45 to 65)
      val domain2 = Domain(set2)

      
      assertEquals(Domain(set1.diff(set2).union(set2.diff(set1))), domain1.onlyOneContains(domain2))
      
      assertEquals(Domain(set2.diff(set1).union(set1.diff(set2))), domain2.onlyOneContains(domain1))
    }
	  
  
  }

  //    @Test
  //    def testKO() = assertTrue(false)

}

