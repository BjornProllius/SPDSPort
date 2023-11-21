package boomerang.guided

import org.junit.Test

class SpecificationParserTest {
  @Test def specificationParserTest1(): Unit = {
    val specification = Specification.create("<GO{F}java.lang.String: void <init>(ON{F}java.lang.String)>")
    assert(specification.getMethodAndQueries.size eq 1)
  }

  @Test def specificationParserTest2(): Unit = {
    val specification = Specification.create("<ON{B}java.lang.String: void <init>(GO{B}java.lang.String)>)")
    assert(specification.getMethodAndQueries.size eq 1)
  }
}