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

  //    @Test
  //    def testKO() = assertTrue(false)

}

