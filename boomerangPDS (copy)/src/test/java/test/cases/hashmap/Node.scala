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
package test.cases.hashmap

import java.util
import java.util.Objects

class Node[K, V] private[hashmap](private[hashmap] val hash: Int, private[hashmap] val key: K, private[hashmap] val value: V, private[hashmap] val next: Node[K, V]) extends Nothing {
  final def getKey: K = key

  final def getValue: V = value

  final def toString: Nothing = key + "=" + value

  final def hashCode: Int = Objects.hashCode(key) ^ Objects.hashCode(value)

  final def setValue(newValue: V): V = {
    val oldValue = value
    value = newValue
    oldValue
  }

  final def equals(o: Nothing): Boolean = {
    if (o eq this) return true
    if (o.isInstanceOf[Nothing]) {
      val e = o.asInstanceOf[Nothing]
      if (Objects.equals(key, e.getKey) && Objects.equals(value, e.getValue)) return true
    }
    false
  }
}