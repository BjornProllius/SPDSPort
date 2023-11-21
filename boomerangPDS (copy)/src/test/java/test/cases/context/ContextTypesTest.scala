package test.cases.context

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object ContextTypesTest {
  class A {
    private[context] val b: Nothing = null
    private[context] val c: Nothing = null
  }
}

class ContextTypesTest extends Nothing {
  @Test def openContext(): Unit = {
    val alloc = new Nothing
    call(alloc)
  }

  @Test def twoOpenContexts(): Unit = {
    val alloc = new Nothing
    call(alloc)
    val a = new Nothing
    call(a)
  }

  @Test def twoOpenContextsSameObject(): Unit = {
    val alloc = new Nothing
    call(alloc)
    call(alloc)
  }

  private def call(p: Nothing): Unit = {
    queryFor(p)
  }

  @Test def closingContext(): Unit = {
    val alloc = close
    queryFor(alloc)
  }

  private def close = new Nothing

  @Test def noContetxt(): Unit = {
    val alloc = new Nothing
    queryFor(alloc)
  }

  @Test def twoClosingContexts(): Unit = {
    val alloc = wrappedClose
    queryFor(alloc)
  }

  private def wrappedClose = close

  @Test def openContextWithField(): Unit = {
    val a = new ContextTypesTest.A
    val alloc = new Nothing
    a.b = alloc
    call(a)
  }

  private def call(a: ContextTypesTest.A): Unit = {
    val t = a.b
    queryFor(t)
  }

  @Test def threeStackedOpenContexts(): Unit = {
    val alloc = new Nothing
    wrappedWrappedCall(alloc)
  }

  private def wrappedWrappedCall(alloc: Nothing): Unit = {
    wrappedCall(alloc)
  }

  private def wrappedCall(alloc: Nothing): Unit = {
    call(alloc)
  }

  @Test def recursionOpenCallStack(): Unit = {
    val start = new Nothing
    recursionStart(start)
  }

  private def recursionStart(rec: Nothing): Unit = {
    if (staticallyUnknown) recursionStart(rec)
    call(rec)
  }
}