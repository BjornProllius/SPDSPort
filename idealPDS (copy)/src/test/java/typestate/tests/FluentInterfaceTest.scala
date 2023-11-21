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

import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.FileMustBeClosedStateMachine
import typestate.test.helper.File

class FluentInterfaceTest extends Nothing {
  @Test def fluentOpen(): Unit = {
    var file = new Nothing
    file = file.open
    mustBeInErrorState(file)
  }

  @Test def fluentOpenAndClose(): Unit = {
    var file = new Nothing
    file = file.open
    mustBeInErrorState(file)
    file = file.close
    mustBeInAcceptingState(file)
  }

  @Override protected def getStateMachine = new Nothing
}