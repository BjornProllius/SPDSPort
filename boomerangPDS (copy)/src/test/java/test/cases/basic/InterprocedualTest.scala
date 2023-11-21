/**
 * ***************************************************************************** Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package test.cases.basic

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class InterprocedualTest extends Nothing {
  @Test def identityTest(): Unit = {
    val alias1 = new Nothing() {}
    val alias2 = identity(alias1)
    queryFor(alias2)
  }

  @Test def simpleAnonymous(): Unit = {
    val alias1 = new Nothing() {}
    queryFor(alias1)
  }

  @Test def simpleNonAnonymous(): Unit = {
    val alias1 = new Nothing
    queryFor(alias1)
  }

  @Test def identityTest1(): Unit = {
    val alias1 = new Nothing
    val alias2 = alias1
    identity(alias1)
    otherCall(alias2)
    queryFor(alias1)
  }

  private def otherCall(alias2: Nothing): Unit = {
  }

  @Test def summaryReuseTest1(): Unit = {
    val alias1 = new Nothing() {}
    var alias2: Nothing = null
    var alias3: Nothing = null
    var alias4: Nothing = null
    alias2 = identity(alias1)
    alias3 = identity(alias2)
    alias4 = alias1
    queryFor(alias4)
  }

  @Test def failedCast(): Unit = {
    val o = new Nothing
    val returned = flow(o)
    val t = returned.asInstanceOf[Nothing]
    queryFor(t)
  }

  private def flow(o: Nothing) = o

  @Test def summaryReuseTest4(): Unit = {
    var alias2: Nothing = null
    if (staticallyUnknown) {
      val alias1 = new Nothing
      alias2 = nestedIdentity(alias1)
    }
    else {
      val alias1 = new Nothing
      alias2 = nestedIdentity(alias1)
    }
    queryFor(alias2)
  }

  @Test def branchWithCall(): Unit = {
    val a1 = new Nothing
    val a2 = new Nothing
    var a: Nothing = null
    if (staticallyUnknown) a = a1
    else a = a2
    wrappedFoo(a)
    queryFor(a)
  }

  private def wrappedFoo(param: Nothing): Unit = {
  }

  private def nestedIdentity(param2: Nothing) = {
    val shouldNotSeeThis = 1
    val returnVal = param2
    returnVal
  }

  @Test def summaryReuseTest2(): Unit = {
    val alias1 = new Nothing() {}
    var alias2: Nothing = null
    var alias3: Nothing = null
    var alias4: Nothing = null
    alias2 = identity(alias1)
    alias3 = identity(alias2)
    alias4 = alias1
    queryFor(alias3)
  }

  @Test def summaryReuseTest3(): Unit = {
    val alias1 = new Nothing() {}
    var alias2: Nothing = null
    var alias3: Nothing = null
    var alias4: Nothing = null
    alias2 = identity(alias1)
    alias3 = identity(alias2)
    alias4 = alias1
    queryFor(alias2)
  }

  @Test def interLoop(): Unit = {
    val alias = new Nothing() {}
    var aliased2: Nothing = null
    var aliased = new Nothing() {}
    val notAlias = new Nothing
    for (i <- 0 until 20) {
      aliased = identity(alias)
    }
    aliased2 = aliased.asInstanceOf[Nothing]
    queryFor(aliased)
  }

  @Test def wrappedAllocationSite(): Unit = {
    val alias1 = wrappedCreate
    queryFor(alias1)
  }

  @Test def branchedObjectCreation(): Unit = {
    var alias1: Nothing = null
    if (staticallyUnknown) alias1 = create
    else {
      val intermediate = create
      alias1 = intermediate
    }
    val query = alias1
    queryFor(query)
  }

  @Test def unbalancedCreation(): Unit = {
    val alias1 = create
    val query = alias1
    queryFor(query)
  }

  @Test def unbalancedCreationStatic(): Unit = {
    val alias1 = createStatic
    val query = alias1
    queryFor(query)
  }

  private def createStatic = new Nothing

  def wrappedCreate: Nothing = create

  def create: Nothing = {
    val alloc1 = new Nothing() {}
    alloc1
  }

  private def identity(param: Nothing) = {
    val mapped = param
    mapped
  }

  @Test def heavySumary(): Unit = {
    val alias1 = new Nothing
    var q: Nothing = null
    if (staticallyUnknown) q = doSummarize(alias1)
    else if (staticallyUnknown) {
      val alias2 = new Nothing
      q = doSummarize(alias2)
    }
    else {
      val alias3 = new Nothing
      q = doSummarize(alias3)
    }
    queryFor(q)
  }

  private def doSummarize(alias1: Nothing) = {
    val a = alias1
    val b = a
    val c = b
    val d = c
    val e = d
    val f = evenFurtherNested(e)
    var g = alias1
    if (staticallyUnknown) g = f
    val h = g
    f
  }

  private def evenFurtherNested(e: Nothing) = e

  @Test def summaryTest(): Unit = {
    val alias1 = new Nothing
    var q: Nothing = null
    if (staticallyUnknown) q = summary(alias1)
    else {
      val alias2 = new Nothing
      q = summary(alias2)
    }
    queryFor(q)
  }

  private def summary(inner: Nothing) = {
    val ret = inner
    ret
  }

  @Test def doubleNestedSummary(): Unit = {
    val alias1 = new Nothing
    var q: Nothing = null
    if (staticallyUnknown) q = nestedSummary(alias1)
    else {
      val alias2 = new Nothing
      q = nestedSummary(alias2)
    }
    queryFor(q)
  }

  private def nestedSummary(inner: Nothing) = {
    val ret = summary(inner)
    ret
  }
}