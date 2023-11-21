package test.cases.callgraph

import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object ContextSensitivityFieldTest {
  class SuperClass {
    private[callgraph] val o: Nothing = null

    def foo(o: Nothing): Unit = {
      this.o = o
    }

    def getO: Nothing = o
  }

  private[callgraph] class CorrectSubclass extends ContextSensitivityFieldTest.SuperClass {
    override def foo(o: Nothing): Unit = {
      super.foo(o)
    }
  }

  private[callgraph] class WrongSubclass extends ContextSensitivityFieldTest.SuperClass {
    override def foo(o: Nothing): Unit = {
      unreachable(o)
    }

    def unreachable(o: Nothing): Unit = {
    }
  }
}

class ContextSensitivityFieldTest extends Nothing {
  def wrongContext(): Unit = {
    val `type` = new ContextSensitivityFieldTest.WrongSubclass
    method(`type`)
  }

  def method(`type`: ContextSensitivityFieldTest.SuperClass): Nothing = {
    val alloc = new Nothing
    `type`.foo(alloc)
    `type`.getO
  }

  // Method WrongSubclass.foo(Object o) is incorrectly marked as reachable.
  @Ignore
  @Test def testOnlyCorrectContextInCallGraph(): Unit = {
    wrongContext()
    val `type` = new ContextSensitivityFieldTest.CorrectSubclass
    val alloc = method(`type`)
    queryFor(alloc)
  }
}