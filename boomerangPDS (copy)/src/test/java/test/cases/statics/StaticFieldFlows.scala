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
package test.cases.statics

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object StaticFieldFlows {
  private var alloc: Nothing = null
  private var instance: Nothing = null
  private var i: Nothing = null

  private def returns = StaticFieldFlows.v

  private def v = {
    if (instance == null) instance = new Nothing
    val loaded = instance
    loaded
  }
}

class StaticFieldFlows extends Nothing {
  @Test def simple(): Unit = {
    StaticFieldFlows.alloc = new Nothing
    val alias = StaticFieldFlows.alloc
    queryFor(alias)
  }

  @Test def simple2(): Unit = {
    StaticFieldFlows.alloc = new Nothing
    val sr = new Nothing
    val r = new Nothing
    queryFor(StaticFieldFlows.alloc)
  }

  @Test def withCallInbetween(): Unit = {
    StaticFieldFlows.alloc = new Nothing
    StaticFieldFlows.alloc.toString
    foo()
    queryFor(StaticFieldFlows.alloc)
  }

  private def foo(): Unit = {
  }

  @Test def singleton(): Unit = {
    val singleton = StaticFieldFlows.v
    val alias = singleton
    queryFor(alias)
  }

  @Test def getAndSet(): Unit = {
    setStatic()
    val alias = getStatic
    queryFor(alias)
  }

  private def getStatic = StaticFieldFlows.i

  private def setStatic(): Unit = {
    StaticFieldFlows.i = new Nothing
  }

  @Test def doubleUnbalancedSingleton(): Unit = {
    val singleton = StaticFieldFlows.returns
    val alias = singleton
    queryFor(alias)
  }

  @Test def overwriteStatic(): Unit = {
    StaticFieldFlows.alloc = new Nothing
    StaticFieldFlows.alloc = new Nothing
    val alias = StaticFieldFlows.alloc
    queryFor(alias)
  }

  @Test def overwriteStaticInter(): Unit = {
    StaticFieldFlows.alloc = new Nothing
    update()
    irrelevantFlow
    val alias = StaticFieldFlows.alloc
    queryFor(alias)
  }

  private def irrelevantFlow = {
    var x = 1
    x = 2
    x = 3
    x = 4
    x
  }

  private def update(): Unit = {
    StaticFieldFlows.alloc = new Nothing
  }

  @Test def intraprocedural(): Unit = {
    setStaticField()
    val alias = StaticFieldFlows.alloc
    queryFor(alias)
  }

  private def setStaticField(): Unit = {
    StaticFieldFlows.alloc = new Nothing
  }
}