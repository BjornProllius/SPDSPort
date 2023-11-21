package test.cases.basic

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

class UnbalancedInterprocedural extends Nothing {
  @Test def unbalancedCreation(): Unit = {
    val alias1 = create
    val query = alias1
    queryFor(query)
  }

  @Test def doubleUnbalancedCreation(): Unit = {
    val alias1 = wrappedCreate
    val query = alias1
    queryFor(query)
  }

  private def wrappedCreate = create

  private def create = new Nothing
}