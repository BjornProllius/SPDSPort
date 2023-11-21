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

import org.junit.Test
import test.core.AbstractBoomerangTest

class AllAliasLongTest extends Nothing {
  @Test def test(): Unit = {
    val a = new Nothing(0, new Nothing, new Nothing, null)
    val t = new Nothing(0, null, new Nothing, a)
    t.balanceDeletion(t, a)
    // t.balanceInsertion(t, t);
    t.treeify(Array[Nothing](a, t))
    // t.moveRootToFront(new TreeNode[]{a,t},a);
    queryFor(t)
  }
}