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

import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.PrintStream
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.PrintStreamStateMachine

class PrintStreamLongTest extends Nothing {
  @Test
  @throws[FileNotFoundException]
  def test1(): Unit = {
    val inputStream = new Nothing("")
    inputStream.close
    inputStream.flush
    mustBeInErrorState(inputStream)
  }

  @Test def test(): Unit = {
    try {
      val out = new Nothing("foo.txt")
      val p = new Nothing(out)
      p.close
      p.println("foo!")
      p.write(42)
      mustBeInErrorState(p)
    } catch {
      case e: Nothing =>
        e.printStackTrace
    }
  }

  @Override protected def getStateMachine = new Nothing
}