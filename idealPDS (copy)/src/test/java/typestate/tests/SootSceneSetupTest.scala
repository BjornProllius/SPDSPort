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
import typestate.impl.statemachines.FileMustBeClosedStateMachineCallToReturn
import typestate.test.helper.File

class SootSceneSetupTest extends Nothing {
  @Test def simple(): Unit = {
    val file = new Nothing
    file.open
    mustBeInErrorState(file)
    file.close
    mustBeInAcceptingState(file)
  }

  @Test
  @Ignore def aliassimple(): Unit = {
    val file = new Nothing
    val alias = file
    alias.open
    mustBeInErrorState(file)
    mustBeInErrorState(alias)
  }

  @Override protected def getStateMachine = new Nothing

  @Override def excludedPackages: Nothing = {
    val exlcuded = super.excludedPackages
    exlcuded.add("typestate.test.helper.File")
    exlcuded
  }
}