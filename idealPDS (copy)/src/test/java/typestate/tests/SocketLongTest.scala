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

import java.io.IOException
import java.net.Socket
import java.net.SocketAddress
import java.util
import org.junit.Ignore
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.SocketStateMachine

@Ignore object SocketLongTest {
  def createSocket = new Nothing

  def createSockets: Nothing = {
    val result = new Nothing
    for (i <- 0 until 5) {
      result.add(new Nothing)
    }
    result
  }

  @throws[IOException]
  def talk(s: Nothing): Unit = {
    s.getChannel
  }
}

@Ignore class SocketLongTest extends Nothing {
  @Test
  @throws[IOException]
  def test1(): Unit = {
    val socket = new Nothing
    socket.connect(new Nothing() {})
    socket.sendUrgentData(2)
    mustBeInAcceptingState(socket)
  }

  @Test
  @throws[IOException]
  def test2(): Unit = {
    val socket = new Nothing
    socket.sendUrgentData(2)
    mustBeInErrorState(socket)
  }

  @Test
  @throws[IOException]
  def test3(): Unit = {
    val socket = new Nothing
    socket.sendUrgentData(2)
    socket.sendUrgentData(2)
    mustBeInErrorState(socket)
  }

  @Test
  @throws[IOException]
  def test4(): Unit = {
    val sockets = SocketLongTest.createSockets
    val it = sockets.iterator
    while (it.hasNext) {
      val s = it.next.asInstanceOf[Nothing]
      s.connect(null)
      SocketLongTest.talk(s)
      mustBeInAcceptingState(s)
    }
    val s1 = createOther
  }

  private def createOther = {
    val result = new Nothing
    for (i <- 0 until 5) {
      result.add(new Nothing)
    }
    result
  }

  @Test
  @throws[IOException]
  def test5(): Unit = {
    val sockets = SocketLongTest.createSockets
    val it = sockets.iterator
    while (it.hasNext) {
      val s = it.next.asInstanceOf[Nothing]
      SocketLongTest.talk(s)
      mayBeInErrorState(s)
    }
  }

  @Override protected def getStateMachine = new Nothing
}