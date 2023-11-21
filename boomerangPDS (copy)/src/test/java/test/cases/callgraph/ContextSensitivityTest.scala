package test.cases.callgraph

import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

class ContextSensitivityTest extends Nothing {
  def wrongContext(): Unit = {
    val `type` = new ContextSensitivityTest#WrongSubclass
    method(`type`)
  }

  def method(`type`: ContextSensitivityTest#SuperClass): Nothing = {
    val alloc = new Nothing
    `type`.foo(alloc)
    alloc
  }

  @Ignore
  @Test def testOnlyCorrectContextInCallGraph(): Unit = {
    wrongContext()
    val `type` = new ContextSensitivityTest#CorrectSubclass
    val alloc = method(`type`)
    queryFor(alloc)
  }

  class SuperClass {
    def foo(o: Nothing): Unit = {
      unreachable(o)
    }
  }

  private[callgraph] class CorrectSubclass extends ContextSensitivityTest#SuperClass {
    override def foo(o: Nothing): Unit = {
    }
  }

  private[callgraph] class WrongSubclass extends ContextSensitivityTest#SuperClass {
    override def foo(o: Nothing): Unit = {
      unreachable(o)
    }
  }
}