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
package test.cases.realworld

import java.util
import test.core.selfrunning.AllocatedObject

object FixAfterInsertion {
  private val RED = false
  private val BLACK = true

  final private[realworld] class Entry[K, V] private[realworld](private[realworld] val key: K, private[realworld] val value: V, private[realworld] val parent: FixAfterInsertion.Entry[K, V])

  /**
   * Make a new cell with given key, value, and parent, and with {@code null} child links, and
   * BLACK color.
   */
    extends Nothing with Nothing {
    private[realworld] val left: FixAfterInsertion.Entry[K, V] = null
    private[realworld] val right: FixAfterInsertion.Entry[K, V] = null
    private[realworld] val color = BLACK

    /**
     * Returns the key.
     *
     * @return the key
     */
    def getKey: K = key

    /**
     * Returns the value associated with the key.
     *
     * @return the value associated with the key
     */
    def getValue: V = value

    /**
     * Replaces the value currently associated with the key with the given value.
     *
     * @return the value associated with the key before this method was called
     */
    def setValue(value: V): V = {
      val oldValue = this.value
      this.value = value
      oldValue
    }

    def hashCode: Int = {
      val keyHash = if (key == null) 0
      else key.hashCode
      val valueHash = if (value == null) 0
      else value.hashCode
      keyHash ^ valueHash
    }

    def toString: Nothing = key + "=" + value
  }

  private def colorOf[K, V](p: FixAfterInsertion.Entry[K, V]) = if (p == null) BLACK
  else p.color

  private def parentOf[K, V](p: FixAfterInsertion.Entry[K, V]) = if (p == null) null
  else p.parent

  private def leftOf[K, V](p: FixAfterInsertion.Entry[K, V]) = if (p == null) null
  else p.left

  private def rightOf[K, V](p: FixAfterInsertion.Entry[K, V]) = if (p == null) null
  else p.right
}

class FixAfterInsertion[K, V] {
  @transient private var root: FixAfterInsertion.Entry[K, V] = null

  private[realworld] def fixAfterInsertion(x: FixAfterInsertion.Entry[K, V]): Unit = {
    while (x != null && (x ne root) && x.parent.color == FixAfterInsertion.RED) if (FixAfterInsertion.parentOf(x) eq FixAfterInsertion.leftOf(FixAfterInsertion.parentOf(FixAfterInsertion.parentOf(x)))) {
      val y = FixAfterInsertion.rightOf(FixAfterInsertion.parentOf(FixAfterInsertion.parentOf(x)))
      if (FixAfterInsertion.colorOf(y) == FixAfterInsertion.RED) x = FixAfterInsertion.parentOf(FixAfterInsertion.parentOf(x))
      else {
        if (x eq FixAfterInsertion.rightOf(FixAfterInsertion.parentOf(x))) {
          x = FixAfterInsertion.parentOf(x)
          rotateLeft(x)
        }
        rotateRight(FixAfterInsertion.parentOf(FixAfterInsertion.parentOf(x)))
      }
    }
    else {
      val y = FixAfterInsertion.leftOf(FixAfterInsertion.parentOf(FixAfterInsertion.parentOf(x)))
      if (FixAfterInsertion.colorOf(y) == FixAfterInsertion.RED) x = FixAfterInsertion.parentOf(FixAfterInsertion.parentOf(x))
      else {
        if (x eq FixAfterInsertion.leftOf(FixAfterInsertion.parentOf(x))) {
          x = FixAfterInsertion.parentOf(x)
          rotateRight(x)
        }
        rotateLeft(FixAfterInsertion.parentOf(FixAfterInsertion.parentOf(x)))
      }
    }
    root.color = FixAfterInsertion.BLACK
  }

  /** From CLR */
  private[realworld] def rotateLeft(p: FixAfterInsertion.Entry[K, V]): Unit = {
    if (p != null) {
      val r = p.right
      p.right = r.left
      if (r.left != null) r.left.parent = p
      r.parent = p.parent
      if (p.parent == null) root = r
      else if (p.parent.left eq p) p.parent.left = r
      else p.parent.right = r
      r.left = p
      p.parent = r
    }
  }

  /** From CLR */
  private[realworld] def rotateRight(p: FixAfterInsertion.Entry[K, V]): Unit = {
    if (p != null) {
      val l = p.left
      p.left = l.right
      if (l.right != null) l.right.parent = p
      l.parent = p.parent
      if (p.parent == null) root = l
      else if (p.parent.right eq p) p.parent.right = l
      else p.parent.left = l
      l.right = p
      p.parent = l
    }
  }
}