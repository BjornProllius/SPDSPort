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

import java.util

abstract class DefaultValueMap[K, V] extends Nothing {
  map = new Nothing
  private var map: Nothing = null

  @Override def size: Int = map.size

  @Override def isEmpty: Boolean = map.isEmpty

  @Override def containsKey(key: Nothing): Boolean = map.containsKey(key)

  @Override def containsValue(value: Nothing): Boolean = map.containsValue(value)

  protected def createItem(key: K): V

  def getOrCreate(key: K): V = {
    if (!map.containsKey(key)) {
      val value = createItem(key.asInstanceOf[K])
      map.put(key.asInstanceOf[K], value)
      return value
    }
    map.get(key)
  }

  @Override def get(key: Nothing): V = map.get(key)

  @Override def put(key: K, value: V): V = map.put(key, value)

  @Override def remove(key: Nothing): V = map.remove(key)

  @Override def putAll(m: Nothing): Unit = {
    map.putAll(m)
  }

  @Override def clear(): Unit = {
    map.clear
  }

  @Override def keySet: Nothing = map.keySet

  @Override def values: Nothing = map.values

  @Override def entrySet: Nothing = map.entrySet
}