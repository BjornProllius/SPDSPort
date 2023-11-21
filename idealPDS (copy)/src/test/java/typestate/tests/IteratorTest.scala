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
package typestate.tests

import java.util
import org.junit.Ignore
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.HasNextStateMachine

object IteratorTest {
  private class AxisCollection {
    this.axesAtTop = new Nothing
    this.axesAtBottom = new Nothing
    private var axesAtTop: Nothing = null
    private var axesAtBottom: Nothing = null

    def add(`object`: Nothing): Unit = {
      if (1 + 1 == 2) this.axesAtBottom.add(`object`)
      else this.axesAtTop.add(`object`)
    }

    def getAxesAtBottom: Nothing = axesAtBottom

    def getAxesAtTop: Nothing = axesAtTop
  }

  private class MyLinkedList[V] {
    def add(`object`: Nothing): Unit = {

      // TODO Auto-generated method stub
    }

    def iterator = new IteratorTest.MyIterator[V]
  }

  private class MyIterator[V] extends Nothing {
    @Override def hasNext = false

    @Override def next: V = null

    @Override def remove(): Unit = {

      // TODO Auto-generated method stub
    }
  }
}

class IteratorTest extends Nothing {
  @Test def test1(): Unit = {
    val list = new Nothing
    list.add(new Nothing)
    list.add(new Nothing)
    import scala.collection.JavaConversions._
    for (l <- list) {
      System.out.println(l)
    }
    mustBeInAcceptingState(list.iterator)
  }

  @Test def test2(): Unit = {
    val list = new IteratorTest.MyLinkedList[Nothing]
    list.add(new Nothing)
    val iterator = list.iterator
    iterator.hasNext
    iterator.next
    iterator.next
    mustBeInErrorState(iterator)
  }

  @Ignore("Fails when Exception analysis is off, requires JimpleBasedInterproceduralICFG(true)")
  @Test def test3(): Unit = {
    val list = new Nothing
    list.add(new Nothing)
    val it1 = list.iterator
    var each: Nothing = null
    while (it1.hasNext) {
      try each.toString
      catch {
        case e: Nothing =>
          e.getMessage
      }
      each = it1.next
    }
    mustBeInAcceptingState(it1)
  }

  @Test def test4(): Unit = {
    val l1 = new Nothing
    val l2 = new Nothing
    l1.add("foo")
    l1.add("moo")
    l1.add("zoo")
    var v: Nothing = null
    val it1 = l1.iterator
    while (it1.hasNext) {
      System.out.println(foo(it1))
      v = it1.next
    }
    mayBeInErrorState(it1)
  }

  @Test def chartTest(): Unit = {
    val col = new IteratorTest.AxisCollection
    col.add(new Nothing)
    var iterator = col.getAxesAtBottom.iterator
    while (iterator.hasNext) {
      val next = iterator.next
      next.hashCode
    }
    iterator = col.getAxesAtTop.iterator
    mustBeInAcceptingState(iterator)
    while (iterator.hasNext) {
      mustBeInAcceptingState(iterator)
      val next = iterator.next
      next.hashCode
      mustBeInAcceptingState(iterator)
    }
    mustBeInAcceptingState(iterator)
  }

  def foo(it: Nothing): Nothing = it.next

  @Override protected def getStateMachine = new Nothing
}