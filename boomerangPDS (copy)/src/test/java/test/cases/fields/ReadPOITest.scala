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

import org.junit.Ignore
import org.junit.Test
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

object ReadPOITest {
  private class EmptyNode {
    private[fields] val left: ReadPOITest.EmptyNode = null
    private[fields] val right: ReadPOITest.EmptyNode = null
  }

  private class EmptyAllocNode extends ReadPOITest.EmptyNode with Nothing {}

  private class OWithRecField {
    private[fields] val field = new ReadPOITest.AllocRec
  }

  private class AllocRec extends ReadPOITest.OWithRecField {}

  private class OWithField {
    private[fields] val field = new ReadPOITest.Alloc
  }

  private class Node {
    private[fields] val left = new ReadPOITest.Node
    private[fields] val right: ReadPOITest.Node = null
  }

  private class AllocNode extends ReadPOITest.Node with Nothing {}

  private class Alloc extends Nothing {}
}

class ReadPOITest extends Nothing {
  class A {
    private[fields] val b: ReadPOITest.Alloc = null
  }

  @Test def indirectAllocationSite(): Unit = {
    val a = new ReadPOITest#A
    val e = a
    e.b = new ReadPOITest.Alloc
    val query = a.b
    queryFor(query)
  }

  @Test def indirectAllocationSiteTwoFields3Address(): Unit = {
    val a = new ReadPOITest.Node
    val firstLoad = a.left
    val alloc = new ReadPOITest.AllocNode
    firstLoad.right = alloc
    val secondLoad = a.left
    val query = secondLoad.right
    queryFor(query)
  }

  @Test def indirectAllocationSiteTwoFields3Address2(): Unit = {
    val a = new ReadPOITest.EmptyNode
    a.left = new ReadPOITest.EmptyNode
    val firstLoad = a.left
    val alloc = new ReadPOITest.EmptyAllocNode
    firstLoad.right = alloc
    val secondLoad = a.left
    val query = secondLoad.right
    queryFor(query)
  }

  @Test def unbalancedField(): Unit = {
    val a = new ReadPOITest.OWithField
    val query = a.field
    queryFor(query)
  }

  @Ignore
  @Test def loadTwice(): Unit = {
    val a = new ReadPOITest.OWithRecField
    val query = a.field.field
    queryFor(query)
  }

  @Ignore
  @Test def indirectAllocationSiteTwoFields(): Unit = {
    val a = new ReadPOITest.Node
    a.left.right = new ReadPOITest.AllocNode
    val query = a.left.right
    queryFor(query)
  }

  @Test def twoFieldsBranched(): Unit = {
    val a = new ReadPOITest.Node
    init(a)
    var query: ReadPOITest.Node = null
    if (staticallyUnknown) query = a.left
    else query = a.right
    queryFor(query)
  }

  private def init(a: ReadPOITest.Node): Unit = {
    a.left = new ReadPOITest.AllocNode
    a.right = new ReadPOITest.AllocNode
  }

  @Test def oneFieldBranched(): Unit = {
    val a = new ReadPOITest#A
    set(a)
    val query = a.b
    queryFor(query)
  }

  private def set(p: ReadPOITest#A): Unit = {
    p.b = new ReadPOITest.Alloc
  }

  @Test def overwriteFieldWithItself(): Unit = {
    var query = new ReadPOITest#List
    query = query.next
    queryFor(query)
  }

  private class List {
    private[fields] val next = new ReadPOITest#AllocListElement
  }

  private class AllocListElement extends ReadPOITest#List with Nothing {}
}