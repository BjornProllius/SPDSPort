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
package ideal

import boomerang.WeightedBoomerang
import boomerang.results.ForwardBoomerangResults
import wpds.impl.Weight

/** Created by johannesspath on 01.12.17. */
class IDEALSeedTimeout(private val solver: Nothing, private var timedoutSolver: Nothing, private var lastResults: Nothing) extends Nothing {
  def getSolver: Nothing = solver

  def getTimedoutSolver: Nothing = timedoutSolver

  def getLastResults: Nothing = lastResults

  @Override def toString = "IDEAL Seed TimeoutException \n"
}