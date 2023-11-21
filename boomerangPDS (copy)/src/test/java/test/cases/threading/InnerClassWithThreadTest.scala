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
package test.cases.threading

import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest
import test.core.selfrunning.AllocatedObject

@Ignore object InnerClassWithThreadTest {
  private var param: Nothing = null

  private class MyThread extends Nothing with Nothing {}
}

@Ignore class InnerClassWithThreadTest extends Nothing {
  @Ignore
  @Test def runWithThreadStatic(): Unit = {
    InnerClassWithThreadTest.param = new Nothing
    val r = new Nothing() {
      @Override def run(): Unit = {
        val cmd = System.getProperty("")
        // if(cmd!=null){
        // param = new Allocation();
        // }
        for (i <- 1 until 3) {
          val t = InnerClassWithThreadTest.param
          val a = t
          queryFor(a)
        }
      }
    }
    val t = new Nothing(r)
    t.start
  }

  @Test def runWithThread(): Unit = {
    val u = new Nothing
    val r = new Nothing() {
      @Override def run(): Unit = {
        // String cmd = System.getProperty("");
        // if(cmd!=null){
        // param = new Allocation();
        // }
        for (i <- 1 until 3) {
          queryFor(u)
        }
      }
    }
    val t = new Nothing(r)
    t.start
  }

  @Test def threadQuery(): Unit = {
    for (i <- 1 until 3) {
      val t = new InnerClassWithThreadTest.MyThread
      t.start
      queryFor(t)
    }
  }

  protected def getAnalyses: Array[Nothing] = Array[Nothing](AnalysisMode.DemandDrivenBackward)

  @Override protected def includeJDK = true
}