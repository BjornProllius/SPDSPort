package boomerang.guided

import org.junit.Test
import org.junit.Assert._

class SpecificationParserTest {

  @Test
  def specificationParserTest1(): Unit = {
    val specification = Specification.create("<GO{F}java.lang.String: void <init>(ON{F}java.lang.String)>")
    assertEquals(1, specification.getMethodAndQueries().size())
  }

  @Test
  def specificationParserTest2(): Unit = {
    val specification = Specification.create("<ON{B}java.lang.String: void <init>(GO{B}java.lang.String)>)")
    assertEquals(1, specification.getMethodAndQueries().size())
  }
}