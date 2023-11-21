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
package boomerang.scene

import boomerang.scene.ControlFlowGraph.Edge

abstract class StaticFieldVal extends Nothing {
  def this(m: Nothing) {
    this()
    super (m)
  }

  def this(m: Nothing, unbalanced: Nothing) {
    this()
    super (m, unbalanced)
  }

  def field: Nothing

  def asUnbalanced(stmt: Nothing): Nothing
}