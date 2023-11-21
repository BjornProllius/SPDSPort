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

import boomerang.scene.DataFlowScope
import boomerang.scene.SootDataFlowScope
import java.util
import org.junit.Test
import soot.Scene
import test.cases.array.ArrayTest.NoAllocation
import test.cases.basic.Allocation
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

class KeySensitiveTest extends Nothing {
  @Test def directAccess(): Unit = {
    val someValue = new Nothing
    val x = new Nothing
    x.put("key", someValue)
    val t = x.get("key")
    queryFor(t)
  }

  @Test def directAccess2Keys(): Unit = {
    val someValue = new Nothing
    val x = new Nothing
    x.put("key", someValue)
    x.put("key2", new Nothing)
    val t = x.get("key")
    queryFor(t)
  }

  @Test def overwrite(): Unit = {
    val someValue = new Nothing
    val x = new Nothing
    // False Positive: Overapproximation. We do not kill during the forward analysis.
    x.put("key", new Nothing)
    x.put("key", someValue)
    val t = x.get("key")
    queryFor(t)
  }

  @Test def accessWithAliasedKey(): Unit = {
    val someValue = new Nothing
    val x = new Nothing
    val key = "key"
    x.put(key, someValue)
    x.put("key2", new Nothing)
    val t = x.get(key)
    queryFor(t)
  }

  @Test def accessWithKeyFromReturn(): Unit = {
    val someValue = new Nothing
    val x = new Nothing
    x.put(getKey, someValue)
    x.put("key2", new Nothing)
    val t = x.get(getKey)
    queryFor(t)
  }

  @Test def interprocedural(): Unit = {
    val someValue = new Nothing
    val x = new Nothing
    x.put(getKey, someValue)
    x.put("key2", new Nothing)
    val t = wrap(x)
    queryFor(t)
  }

  private def wrap(mp: Nothing) = {
    val i = mp.get(getKey)
    i
  }

  private def getKey = "KEY"

  @Override protected def getDataFlowScope: Nothing = SootDataFlowScope.excludeComplex(Scene.v)
}