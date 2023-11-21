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

import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

object Singleton {
  private val instance: Nothing = null

  def i: Nothing = {
    val getter = objectGetter
    val allocation = getter.getG
    allocation
  }

  trait GlobalObjectGetter {
    def getG: Nothing

    def reset(): Unit
  }

  private val alloc: Nothing = null
  private val objectGetter = new Singleton.GlobalObjectGetter() {
    private[statics] var instance = new Nothing

    override def getG: Nothing = instance

    override def reset(): Unit = {
      instance = new Nothing
    }
  }
}

class Singleton extends Nothing {
  @Test
  @Ignore def doubleSingleton(): Unit = {
    val singleton = Singleton.i
    val alias = singleton
    queryFor(alias)
  }

  @Test def doubleSingletonDirect(): Unit = {
    val singleton = Singleton.objectGetter.getG
    val alias = singleton
    queryFor(alias)
  }

  @Test def singletonDirect(): Unit = {
    val singleton = Singleton.alloc
    queryFor(singleton)
  }
}