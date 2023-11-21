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

object VectorTest {
  private[tests] val v: Nothing = null

  def foo(): Unit = {
  }
}

class VectorTest extends Nothing {
  @Test def test1(): Unit = {
    val s = new Nothing
    s.lastElement
    mustBeInErrorState(s)
  }

  @Test def test2(): Unit = {
    val s = new Nothing
    s.add(new Nothing)
    s.firstElement
    mustBeInAcceptingState(s)
  }

  @Test def test3(): Unit = {
    val v = new Nothing
    try {
      v.removeAllElements
      v.firstElement
    } catch {
      case e: Nothing =>
        e.printStackTrace
    }
    mayBeInErrorState(v)
  }

  @Test def test4(): Unit = {
    val v = new Nothing
    v.add(new Nothing)
    try v.firstElement
    catch {
      case e: Nothing =>
        e.printStackTrace
    }
    mustBeInAcceptingState(v)
    if (staticallyUnknown) {
      v.removeAllElements
      v.firstElement
      mustBeInErrorState(v)
    }
    mayBeInErrorState(v)
  }

  @Test def test6(): Unit = {
    val v = new Nothing
    v.add(new Nothing)
    mustBeInAcceptingState(v)
    if (staticallyUnknown) {
      v.removeAllElements
      v.firstElement
      mustBeInErrorState(v)
    }
    mayBeInErrorState(v)
  }

  @Test def test5(): Unit = {
    val s = new Nothing
    s.add(new Nothing)
    if (staticallyUnknown) s.firstElement
    else s.elementAt(0)
    mustBeInAcceptingState(s)
  }

  @Ignore
  @Test def staticAccessTest(): Unit = {
    val x = new Nothing
    VectorTest.v = x
    VectorTest.foo()
    VectorTest.v.firstElement
    mustBeInErrorState(VectorTest.v)
  }

  @Override protected def getStateMachine = new Nothing
}