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
import typestate.impl.statemachines.VectorStateMachine

@SuppressWarnings("deprecation") object StackTest {
  private class OwithStack {
    private[tests] var stack: Nothing = null

    def pushStack(o: Nothing): Unit = {
      if (this.stack == null) this.stack = new Nothing
      this.stack.push(o)
    }

    def get: Nothing = {
      if (stack == null || stack.empty) return null
      val peek = this.stack.peek
      mustBeInAcceptingState(this.stack)
      peek
    }
  }
}

@SuppressWarnings("deprecation") class StackTest extends Nothing {
  @Ignore("Broken since refactoring scope")
  @Test def test1(): Unit = {
    val s = new Nothing
    if (staticallyUnknown) s.peek
    else {
      val r = s
      r.pop
      mustBeInErrorState(r)
    }
    mustBeInErrorState(s)
  }

  @Test def test4simple(): Unit = {
    val s = new Nothing
    s.peek
    mustBeInErrorState(s)
    s.pop
    mustBeInErrorState(s)
  }

  @Test def test2(): Unit = {
    val s = new Nothing
    s.add(new Nothing)
    if (staticallyUnknown) s.peek
    else s.pop
    mustBeInAcceptingState(s)
  }

  @Test def test6(): Unit = {
    val l = new Nothing
    val s = new Nothing
    if (staticallyUnknown) s.push(new Nothing)
    if (staticallyUnknown) s.push(new Nothing)
    if (!s.isEmpty) {
      val pop = s.pop
      mayBeInErrorState(s)
    }
  }

  @Test def test3(): Unit = {
    val s = new Nothing
    s.peek
    mustBeInErrorState(s)
    s.pop
    mustBeInErrorState(s)
  }

  @Test def test5(): Unit = {
    val s = new Nothing
    s.peek
    mustBeInErrorState(s)
  }

  @Test def test4(): Unit = {
    val s = new Nothing
    s.peek
    s.pop
    val c = new Nothing
    c.add(new Nothing)
    c.peek
    c.pop
    mustBeInErrorState(s)
    mustBeInAcceptingState(c)
  }

  @Ignore("Broken since refactoring scope")
  @Test def testInNewObject(): Unit = {
    val owithStack = new StackTest.OwithStack
    owithStack.pushStack(new Nothing)
    owithStack.get
    mustBeInAcceptingState(owithStack.stack)
  }

  @Override protected def getStateMachine = new Nothing
}