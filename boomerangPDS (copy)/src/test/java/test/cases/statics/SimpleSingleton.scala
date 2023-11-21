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
package test.cases.statics

import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object SimpleSingleton {
  private var alloc = new Nothing
}

class SimpleSingleton extends Nothing {
  @Test def singletonDirect(): Unit = {
    val singleton = SimpleSingleton.alloc
    queryForAndNotEmpty(singleton)
  }

  @Test def staticInnerAccessDirect(): Unit = {
    val r = new Nothing() {
      @Override def run(): Unit = {
        val singleton = SimpleSingleton.alloc
        queryForAndNotEmpty(singleton)
      }
    }
    r.run
  }

  @Test def simpleWithAssign(): Unit = {
    SimpleSingleton.alloc = new Nothing
    val b = SimpleSingleton.alloc
    queryFor(b)
  }

  @Test def simpleWithAssign2(): Unit = {
    SimpleSingleton.alloc = new Nothing
    val b = SimpleSingleton.alloc
    val a = SimpleSingleton.alloc
    queryFor(b)
  }
}