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

import boomerang.scene.Type
import com.ibm.wala.analysis.typeInference.TypeAbstraction

class WALADummyVal(method: Nothing) extends Nothing(-1, method) {
  @Override def isNull = false

  @Override def getType = new Nothing(TypeAbstraction.TOP)

  @Override def hashCode: Int = System.identityHashCode(this)

  @Override def equals(obj: Nothing): Boolean = this eq obj

  @Override def toString: Nothing = "dummy in " + method
}