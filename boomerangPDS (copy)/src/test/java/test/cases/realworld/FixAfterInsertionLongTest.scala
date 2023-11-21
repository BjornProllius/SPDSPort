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

import org.junit.Ignore
import org.junit.Test
import test.cases.realworld.FixAfterInsertion.Entry
import test.core.AbstractBoomerangTest

@Ignore class FixAfterInsertionLongTest extends Nothing {
  @Test def main(): Unit = {
    var entry = new Nothing(null, null, null)
    entry = new Nothing(null, null, entry)
    new Nothing().fixAfterInsertion(entry)
    val query = entry.parent
    queryFor(query)
  }

  @Test def rotateLeftAndRightInLoop(): Unit = {
    var entry = new Nothing(null, null, null)
    entry = new Nothing(null, null, entry)
    while (true) {
      new Nothing().rotateLeft(entry)
      new Nothing().rotateRight(entry)
      if (staticallyUnknown) break //todo: break is not supported
    }
    val query = entry.parent
    queryFor(query)
  }
}