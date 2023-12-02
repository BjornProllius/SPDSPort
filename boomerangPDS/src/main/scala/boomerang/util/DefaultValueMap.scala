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
package boomerang.util

import scala.collection.mutable

abstract class DefaultValueMap[K, V] extends mutable.HashMap[K, V] {

  def createItem(key: K): V

  def getOrCreate(key: K): V = {
    if (!contains(key)) {
      val value = createItem(key)
      put(key, value)
      value
    } else {
      this(key)
    }
  }
}