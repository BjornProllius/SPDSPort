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
package boomerang.example

object BoomerangExampleTarget1 {
  def main(args: Nothing*): Unit = {
    val a = new BoomerangExampleTarget1.ClassWithField
    a.field = new BoomerangExampleTarget1.ObjectOfInterest
    val b = a
    val n = new BoomerangExampleTarget1.NestedClassWithField
    n.nested = b
    staticCallOnFile(a, n)
  }

  private def staticCallOnFile(x: BoomerangExampleTarget1.ClassWithField, n: BoomerangExampleTarget1.NestedClassWithField): Unit = {
    val queryVariable = x.field
    // The analysis triggers a query for the following variable
    queryFor(queryVariable)
  }

  private def queryFor(queryVariable: BoomerangExampleTarget1.ObjectOfInterest): Unit = {
  }

  class ClassWithField {
    var field: BoomerangExampleTarget1.ObjectOfInterest = null
  }

  class ObjectOfInterest {}

  class NestedClassWithField {
    var nested: BoomerangExampleTarget1.ClassWithField = null
  }
}