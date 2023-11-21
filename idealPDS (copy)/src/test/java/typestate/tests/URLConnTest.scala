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
package typestate.tests

import java.io.IOException
import java.net.HttpURLConnection
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.URLConnStateMachine

class URLConnTest extends Nothing {
  @Test
  @throws[IOException]
  def test1(): Unit = {
    val httpURLConnection = new Nothing(null) {
      @Override
      @throws[IOException]
      def connect(): Unit = {
        // TODO Auto-generated method stub
        System.out.println("")
      }

      @Override def usingProxy: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override def disconnect(): Unit = {

        // TODO Auto-generated method stub
      }
    }
    httpURLConnection.connect
    httpURLConnection.setDoOutput(true)
    mustBeInErrorState(httpURLConnection)
    httpURLConnection.setAllowUserInteraction(false)
    mustBeInErrorState(httpURLConnection)
  }

  @Test
  @throws[IOException]
  def test2(): Unit = {
    val httpURLConnection = new Nothing(null) {
      @Override
      @throws[IOException]
      def connect(): Unit = {
        // TODO Auto-generated method stub
        System.out.println("")
      }

      @Override def usingProxy: Boolean = {
        // TODO Auto-generated method stub
        false
      }

      @Override def disconnect(): Unit = {

        // TODO Auto-generated method stub
      }
    }
    httpURLConnection.setDoOutput(true)
    httpURLConnection.setAllowUserInteraction(false)
    httpURLConnection.connect
    mustBeInAcceptingState(httpURLConnection)
  }

  @Override protected def getStateMachine = new Nothing
}