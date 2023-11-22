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
  def main(args: Array[String]): Unit = {
    val a = new ClassWithField()
    a.field = new ObjectOfInterest()
    val b = a
    val n = new NestedClassWithField()
    n.nested = b
    staticCallOnFile(a, n)
  }

  private def staticCallOnFile(x: ClassWithField, n: NestedClassWithField): Unit = {
    val queryVariable = x.field
    // The analysis triggers a query for the following variable
    queryFor(queryVariable)
  }

  private def queryFor(queryVariable: ObjectOfInterest): Unit = {}

  class ClassWithField {
    var field: ObjectOfInterest = _
  }

  class ObjectOfInterest {}

  class NestedClassWithField {
    var nested: ClassWithField = _
  }
}