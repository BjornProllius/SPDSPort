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
import typestate.test.helper.ObjectWithField

class FileMustBeClosedStrongUpdateTest extends Nothing {
  @Test def noStrongUpdatePossible(): Unit = {
    var b: Nothing = null
    val a = new Nothing
    a.open
    val e = new Nothing
    e.open
    if (staticallyUnknown) b = a
    else b = e
    b.close
    mayBeInErrorState(a)
    mustBeInAcceptingState(b)
  }

  @Test def aliasSensitive(): Unit = {
    val a = new Nothing
    val b = a
    val file = new Nothing
    file.open
    a.field = file
    val loadedFromAlias = b.field
    loadedFromAlias.close
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(a.field)
  }

  @Override protected def getStateMachine = new Nothing
}