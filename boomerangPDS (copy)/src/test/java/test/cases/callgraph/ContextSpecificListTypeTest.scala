package test.cases.callgraph

import boomerang.BoomerangOptions
import boomerang.DefaultBoomerangOptions
import java.util
import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object ContextSpecificListTypeTest {
  private class WrongList extends Nothing {
    @Override def add(e: Nothing): Boolean = {
      unreachable()
      false
    }

    def unreachable(): Unit = {
    }
  }
}

class ContextSpecificListTypeTest extends Nothing {
  def wrongContext(): Unit = {
    val list = new ContextSpecificListTypeTest.WrongList
    method(list)
  }

  def method(list: Nothing): Nothing = {
    val alloc = new Nothing
    list.add(alloc)
    alloc
  }

  @Ignore
  @Test def testListType(): Unit = {
    wrongContext()
    val list = new Nothing
    val query = method(list)
    queryFor(query)
  }

  @Override protected def createBoomerangOptions: Nothing = new Nothing() {
    @Override def onTheFlyCallGraph: Boolean = return true
  }
}