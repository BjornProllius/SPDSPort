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

import org.junit.Ignore
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.FileMustBeClosedStateMachine
import typestate.test.helper.File

@Ignore("Needs further investigation. Occurs since refactoring from data-flow propagation (Statement,Val) was changed to (CFGEdge,Val).") object FileMustBeClosedInterfaceTest {
  class ImplFlow1 extends FileMustBeClosedInterfaceTest.Flow {
    @Override override def flow(file: Nothing): Unit = {
      file.open
    }
  }

  class ImplFlow2 extends FileMustBeClosedInterfaceTest.Flow {
    @Override override def flow(file: Nothing): Unit = {
      file.close
    }
  }

  private trait Flow {
    def flow(file: Nothing): Unit
  }
}

@Ignore("Needs further investigation. Occurs since refactoring from data-flow propagation (Statement,Val) was changed to (CFGEdge,Val).") class FileMustBeClosedInterfaceTest extends Nothing {
  @Test def main(): Unit = {
    val file = new Nothing
    val flow = if (staticallyUnknown) new FileMustBeClosedInterfaceTest.ImplFlow1
    else new FileMustBeClosedInterfaceTest.ImplFlow2
    flow.flow(file)
    mayBeInErrorState(file)
    mayBeInAcceptingState(file)
  }

  @Test def other(): Unit = {
    val file = new Nothing
    if (staticallyUnknown) {
      new FileMustBeClosedInterfaceTest.ImplFlow1().flow(file)
      mustBeInErrorState(file)
    }
    else {
      new FileMustBeClosedInterfaceTest.ImplFlow2().flow(file)
      mustBeInAcceptingState(file)
    }
    mayBeInAcceptingState(file)
    mayBeInErrorState(file)
  }

  @Override protected def getStateMachine = new Nothing
}