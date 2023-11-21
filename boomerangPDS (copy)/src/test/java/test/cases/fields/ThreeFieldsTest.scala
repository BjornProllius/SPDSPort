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
package test.cases.fields

import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object ThreeFieldsTest {
  class Level1 {
    private[fields] val l2 = new ThreeFieldsTest.Level2
  }

  class Level2 {
    private[fields] val l3 = new ThreeFieldsTest.Level3
  }

  class Level3 {
    private[fields] val l4: ThreeFieldsTest.Level4 = null
  }

  class Level4 extends Nothing {}
}

class ThreeFieldsTest extends Nothing {
  @Test def indirectAllocationSite(): Unit = {
    val l = new ThreeFieldsTest.Level1
    val x = l.l2
    setField(l)
    val intermediate = x.l3
    val alias2 = intermediate.l4
    queryFor(alias2)
  }

  @Test def indirectAllocationSite3Address(): Unit = {
    val l = new ThreeFieldsTest.Level1
    val x = l.l2
    setField3Address(l)
    val intermediate = x.l3
    val alias2 = intermediate.l4
    queryFor(alias2)
  }

  def setField3Address(l: ThreeFieldsTest.Level1): Unit = {
    val xAlias = l.l2
    val level3 = xAlias.l3
    val alloc = new ThreeFieldsTest.Level4
    level3.l4 = alloc
  }

  @Test def indirectAllocationSiteNoRead(): Unit = {
    val l = new ThreeFieldsTest.Level1
    setField(l)
    val alias = l.l2.l3.l4
    queryFor(alias)
  }

  def setField(l: ThreeFieldsTest.Level1): Unit = {
    l.l2.l3.l4 = new ThreeFieldsTest.Level4
  }

  @Test def indirectAllocationSiteNoReadOnlyTwoLevel(): Unit = {
    val l = new ThreeFieldsTest.Level2
    setFieldLevel2(l)
    val alias = l.l3.l4
    queryFor(alias)
  }

  def setFieldLevel2(l: ThreeFieldsTest.Level2): Unit = {
    l.l3.l4 = new ThreeFieldsTest.Level4
  }

  @Test def test(): Unit = {
    val l = new ThreeFieldsTest.Level1
    val x = l.l2
    wrappedSetField(l)
    val alias = l.l2.l3.l4
    val alias2 = x.l3.l4
    queryFor(alias2)
  }

  private def wrappedSetField(l: ThreeFieldsTest.Level1): Unit = {
    setField(l)
  }
}