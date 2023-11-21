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
package test.cases.sets

class MyMap {
  private[sets] val m = new Nothing

  def add(o: Nothing): Unit = {
    val map = this.m
    map.innerAdd(o)
    val alias = this.m
    val retrieved = alias.content
  }

  def get: Nothing = {
    val map = this.m
    map.get
  }
}