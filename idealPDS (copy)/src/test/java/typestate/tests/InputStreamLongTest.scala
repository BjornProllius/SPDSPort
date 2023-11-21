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

import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.InputStreamStateMachine

class InputStreamLongTest extends Nothing {
  @Test
  @throws[IOException]
  def test1(): Unit = {
    val inputStream = new Nothing("")
    inputStream.close
    inputStream.read
    mustBeInErrorState(inputStream)
  }

  @Test
  @throws[IOException]
  def test2(): Unit = {
    val inputStream = new Nothing("")
    inputStream.close
    inputStream.close
    inputStream.read
    mustBeInErrorState(inputStream)
  }

  @Test
  @throws[IOException]
  def test3(): Unit = {
    val inputStream = new Nothing("")
    inputStream.read
    inputStream.close
    mustBeInAcceptingState(inputStream)
  }

  @Override protected def getStateMachine = new Nothing
}