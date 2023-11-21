package test.cases.unbalanced

import org.junit.Test
import test.cases.fields.Alloc
import test.cases.fields.B
import test.core.AbstractBoomerangTest

class UnbalancedScopesTest extends Nothing {
  @Test def closingContext(): Unit = {
    val `object` = create
    queryFor(`object`)
  }

  @Test def openingContext(): Unit = {
    val `object` = create
    val y = `object`
    inner(y)
  }

  @Test def doubleClosingContext(): Unit = {
    val `object` = wrappedCreate
    queryFor(`object`)
  }

  @Test def branchedReturn(): Unit = {
    val `object` = aOrB
    queryFor(`object`)
  }

  @Test def summaryReuse(): Unit = {
    val `object` = createA
    val y = `object`
    val x = id(y)
    queryFor(x)
  }

  private def createA = {
    val c = new Nothing
    val d = id(c)
    d
  }

  private def id(c: Nothing) = c

  private def aOrB: Nothing = {
    if (staticallyUnknown) return new Nothing
    new Nothing
  }

  def wrappedCreate: Nothing = {
    if (staticallyUnknown) return create
    wrappedCreate
  }

  private def inner(inner: Nothing): Unit = {
    val x = inner
    queryFor(x)
  }

  private def create = new Nothing
}