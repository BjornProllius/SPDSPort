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
package sync.pds.solver

import sync.pds.solver.nodes.Node
import wpds.impl.Weight

trait WeightFunctions[Stmt, Fact, Field, W <: Weight] {
  def push(curr: Nothing, succ: Nothing, field: Field): W

  def normal(curr: Nothing, succ: Nothing): W

  def pop(curr: Nothing): W

  def getOne: W
}