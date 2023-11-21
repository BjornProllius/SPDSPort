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
import java.security.KeyStore
import java.security.KeyStoreException
import java.security.NoSuchAlgorithmException
import java.security.cert.CertificateException
import org.junit.Ignore
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.KeyStoreStateMachine

class KeystoreLongTest extends Nothing {
  @Test
  @throws[KeyStoreException]
  @throws[NoSuchAlgorithmException]
  @throws[CertificateException]
  @throws[IOException]
  def test1(): Unit = {
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    var fis: Nothing = null
    try {
      fis = new Nothing("keyStoreName")
      ks.load(fis, null)
    } finally if (fis != null) fis.close
    mustBeInAcceptingState(ks)
  }

  @Test
  @throws[KeyStoreException]
  @throws[NoSuchAlgorithmException]
  @throws[CertificateException]
  @throws[IOException]
  def test4(): Unit = {
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    val x = ks
    val fis: Nothing = null
    ks.load(fis, null)
    mustBeInAcceptingState(ks)
    mustBeInAcceptingState(x)
  }

  @Ignore("TODO: Needs further inspection on how to properly handle catch within method aliases")
  @Test
  @throws[KeyStoreException]
  def test2(): Unit = {
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    ks.aliases
    mustBeInErrorState(ks)
  }

  @Test
  @throws[KeyStoreException]
  @throws[NoSuchAlgorithmException]
  @throws[CertificateException]
  @throws[IOException]
  def test3(): Unit = {
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    var fis: Nothing = null
    try {
      fis = new Nothing("keyStoreName")
      ks.load(fis, null)
    } finally if (fis != null) fis.close
    ks.aliases
    mustBeInAcceptingState(ks)
  }

  @Ignore("TODO: Needs further inspection on how to properly handle catch clasues")
  @Test def catchClause(): Unit = {
    try {
      val keyStore = KeyStore.getInstance("JKS")
      // ... Some code
      val size = keyStore.size // Hit !
      mustBeInErrorState(keyStore)
    } catch {
      case e: Nothing =>
        e.printStackTrace
    }
  }

  @Override protected def getStateMachine = new Nothing
}