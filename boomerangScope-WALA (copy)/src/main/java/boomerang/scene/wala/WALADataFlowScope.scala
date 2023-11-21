/**
 * ***************************************************************************** Copyright (c) 2020
 * CodeShield GmbH, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package boomerang.scene.wala

import boomerang.scene.DataFlowScope
import boomerang.scene.DeclaredMethod
import boomerang.scene.Method

object WALADataFlowScope {
  def make: Nothing = new Nothing() {
    @Override def isExcluded(method: Nothing): Boolean = return false

    @Override def isExcluded(method: Nothing): Boolean = return false
  }

  var APPLICATION_ONLY: Nothing = new Nothing() {
    @Override def isExcluded(method: Nothing): Boolean = !method.getDeclaringClass.isApplicationClass || method.isNative

    @Override def isExcluded(method: Nothing): Boolean = !method.getDeclaringClass.isApplicationClass || method.isNative
  }
}