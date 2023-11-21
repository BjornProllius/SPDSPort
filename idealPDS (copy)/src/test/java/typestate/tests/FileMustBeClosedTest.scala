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

import org.junit.Ignore
import org.junit.Test
import test.IDEALTestingFramework
import typestate.finiteautomata.TypeStateMachineWeightFunctions
import typestate.impl.statemachines.FileMustBeClosedStateMachine
import typestate.test.helper.File
import typestate.test.helper.ObjectWithField

object FileMustBeClosedTest {
  def call(alias: Nothing): Unit = {
    alias.open
  }

  def call3(other: Nothing, alias: Nothing): Unit = {
    alias.open
    mustBeInErrorState(alias)
  }

  def flows(other: Nothing, b: Boolean): Unit = {
    if (b) other.close
  }

  def flows2(other: Nothing, b: Boolean): Unit = {
    other.close
  }

  def flows(container: Nothing): Unit = {
    container.field = new Nothing
    val field = container.field
    field.open
  }

  def clse(first: Nothing): Unit = {
    first.close
  }

  def createOpenedFile: Nothing = {
    val f = new Nothing
    f.open
    mustBeInErrorState(f)
    f
  }

  def cls(o2: Nothing): Unit = {
    o2.close
    val x = 1
  }

  private[tests] val v: Nothing = null

  def foo(): Unit = {
  }

  class InnerObject {
    this.file = new Nothing
    this.file.open
    var file: Nothing = null

    def this(string: Nothing) {
      this()
      this.file = new Nothing
    }

    def doClose(): Unit = {
      mustBeInErrorState(file)
      this.file.close
      mustBeInAcceptingState(file)
    }

    def doOpen(): Unit = {
      this.file.open
      mustBeInErrorState(file)
    }
  }
}

class FileMustBeClosedTest extends Nothing {
  @Test def simple(): Unit = {
    val file = new Nothing
    file.open
    mustBeInErrorState(file)
    file.close
    mustBeInAcceptingState(file)
  }

  @Test def simple2(): Unit = {
    val file = new Nothing
    file.open
    mustBeInErrorState(file)
  }

  @Test def simple0(): Unit = {
    val file = new Nothing
    file.open
    escape(file)
    mustBeInErrorState(file)
  }

  @Test def simple0a(): Unit = {
    val file = new Nothing
    file.open
    val alias = file
    escape(alias)
    mustBeInErrorState(file)
  }

  @Test def simpleStrongUpdate(): Unit = {
    val file = new Nothing
    val alias = file
    file.open
    // mustBeInErrorState(file);
    mustBeInErrorState(alias)
    alias.close
    // mustBeInAcceptingState(alias);
    mustBeInAcceptingState(file)
  }

  @Test def simpleStrongUpdate1(): Unit = {
    val file = new Nothing
    val alias = file
    file.open
    mustBeInErrorState(alias)
  }

  @Test def simpleStrongUpdate1a(): Unit = {
    val file = new Nothing
    val alias = file
    file.open
    mustBeInErrorState(file)
    mustBeInErrorState(alias)
  }

  @Test def simpleStrongUpdate2(): Unit = {
    val x = new Nothing
    val y = x
    x.open
    x.close
    mustBeInAcceptingState(x)
    mustBeInAcceptingState(y)
  }

  @Test def recursion(): Unit = {
    val file = new Nothing
    file.open
    mustBeInErrorState(file)
    recursive(file)
    mustBeInAcceptingState(file)
  }

  def recursive(file: Nothing): Unit = {
    file.close
    if (!staticallyUnknown) {
      val alias = file
      recursive(alias)
    }
  }

  def escape(other: Nothing): Unit = {
    mustBeInErrorState(other)
  }

  @Test def simple1(): Unit = {
    val file = new Nothing
    val alias = file
    alias.open
    mustBeInErrorState(file)
    mustBeInErrorState(alias)
  }

  @Test def simpleNoStrongUpdate(): Unit = {
    val file = new Nothing
    var alias: Nothing = null
    if (staticallyUnknown) {
      alias = file
      alias.open
      mustBeInErrorState(file)
    }
    else alias = new Nothing
    alias.open
    mayBeInErrorState(file)
    mayBeInErrorState(alias)
  }

  @Test def branching(): Unit = {
    val file = new Nothing
    if (staticallyUnknown) file.open
    mayBeInErrorState(file)
    file.close
    mustBeInAcceptingState(file)
  }

  @Test def test222(): Unit = {
    val file = new Nothing
    if (staticallyUnknown) file.open
    mayBeInAcceptingState(file)
  }

  @Test def branchingMay(): Unit = {
    val file = new Nothing
    if (staticallyUnknown) file.open
    else file.close
    System.out.println(2)
    mayBeInErrorState(file)
    mayBeInAcceptingState(file)
  }

  @Test def continued(): Unit = {
    val file = new Nothing
    file.open
    file.close
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(file)
    System.out.println(2)
  }

  @Test def aliasing(): Unit = {
    val file = new Nothing
    val alias = file
    if (staticallyUnknown) file.open
    mayBeInErrorState(file)
    alias.close
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(alias)
  }

  @Test def summaryTest(): Unit = {
    val file1 = new Nothing
    FileMustBeClosedTest.call(file1)
    val y = 1
    file1.close
    mustBeInAcceptingState(file1)
    val file = new Nothing
    val alias = file
    FileMustBeClosedTest.call(alias)
    file.close
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(alias)
  }

  @Test def simpleAlias(): Unit = {
    val y = new Nothing
    val x = y
    x.open
    val z = 1
    mustBeInErrorState(x)
    y.close
    mustBeInAcceptingState(x)
    mustBeInAcceptingState(y)
  }

  @Test def wrappedOpenCall(): Unit = {
    val file1 = new Nothing
    FileMustBeClosedTest.call3(file1, file1)
    mustBeInErrorState(file1)
  }

  @Test def interprocedural(): Unit = {
    val file = new Nothing
    file.open
    FileMustBeClosedTest.flows(file, true)
    mayBeInAcceptingState(file)
    mayBeInErrorState(file)
  }

  @Test def interprocedural2(): Unit = {
    val file = new Nothing
    file.open
    FileMustBeClosedTest.flows2(file, true)
    mustBeInAcceptingState(file)
  }

  @Test def intraprocedural(): Unit = {
    val file = new Nothing
    file.open
    if (staticallyUnknown) file.close
    mayBeInAcceptingState(file)
    mayBeInErrorState(file)
  }

  @Test def flowViaField(): Unit = {
    val container = new Nothing
    FileMustBeClosedTest.flows(container)
    if (staticallyUnknown) container.field.close
    mayBeInErrorState(container.field)
  }

  @Test def flowViaFieldDirect(): Unit = {
    val container = new Nothing
    container.field = new Nothing
    val field = container.field
    field.open
    val f2 = container.field
    mustBeInErrorState(f2)
  }

  @Test def flowViaFieldDirect2(): Unit = {
    val container = new Nothing
    container.field = new Nothing
    val field = container.field
    field.open
    mustBeInErrorState(container.field)
    val field2 = container.field
    field2.close
    mustBeInAcceptingState(container.field)
  }

  @Test def flowViaFieldNotUnbalanced(): Unit = {
    val container = new Nothing
    container.field = new Nothing
    open(container)
    if (staticallyUnknown) {
      container.field.close
      mustBeInAcceptingState(container.field)
    }
    mayBeInErrorState(container.field)
    mayBeInAcceptingState(container.field)
  }

  def open(container: Nothing): Unit = {
    val field = container.field
    field.open
  }

  @Test def indirectFlow(): Unit = {
    val a = new Nothing
    val b = a
    flows(a, b)
    mayBeInAcceptingState(a.field)
    mayBeInAcceptingState(b.field)
  }

  def flows(aInner: Nothing, bInner: Nothing): Unit = {
    val file = new Nothing
    file.open
    aInner.field = file
    val alias = bInner.field
    mustBeInErrorState(alias)
    alias.close
  }

  @Test def parameterAlias(): Unit = {
    val file = new Nothing
    val alias = file
    call(alias, file)
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(alias)
  }

  def call(file1: Nothing, file2: Nothing): Unit = {
    file1.open
    file2.close
    mustBeInAcceptingState(file1)
  }

  @Test def parameterAlias2(): Unit = {
    val file = new Nothing
    val alias = file
    call2(alias, file)
    mayBeInErrorState(file)
    mayBeInErrorState(alias)
  }

  def call2(file1: Nothing, file2: Nothing): Unit = {
    file1.open
    if (staticallyUnknown) file2.close
  }

  @Test def aliasInInnerScope(): Unit = {
    val a = new Nothing
    val b = a
    val file = new Nothing
    file.open
    bar(a, b, file)
    b.field.close
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(a.field)
  }

  @Test def noStrongUpdate(): Unit = {
    val a = new Nothing
    val b = new Nothing
    val file = new Nothing
    if (staticallyUnknown) b.field = file
    else a.field = file
    a.field.open
    b.field.close
    // Debatable
    mayBeInAcceptingState(file)
  }

  @Test def unbalancedReturn1(): Unit = {
    val second = FileMustBeClosedTest.createOpenedFile
    mustBeInErrorState(second)
  }

  @Test def unbalancedReturn2(): Unit = {
    val first = FileMustBeClosedTest.createOpenedFile
    val x = 1
    FileMustBeClosedTest.clse(first)
    mustBeInAcceptingState(first)
    val second = FileMustBeClosedTest.createOpenedFile
    second.hashCode
    mustBeInErrorState(second)
  }

  @Test def unbalancedReturnAndBalanced(): Unit = {
    val first = FileMustBeClosedTest.createOpenedFile
    val x = 1
    FileMustBeClosedTest.clse(first)
    mustBeInAcceptingState(first)
  }

  def bar(a: Nothing, b: Nothing, file: Nothing): Unit = {
    a.field = file
  }

  @Test def lateWriteToField(): Unit = {
    val a = new Nothing
    val b = a
    val file = new Nothing
    bar(a, file)
    val c = b.field
    c.close
    mustBeInAcceptingState(file)
  }

  def bar(a: Nothing, file: Nothing): Unit = {
    file.open
    a.field = file
    val whoAmI = a.field
    mustBeInErrorState(whoAmI)
  }

  @Test def fieldStoreAndLoad1(): Unit = {
    val container = new Nothing
    val file = new Nothing
    container.field = file
    val a = container.field
    a.open
    mustBeInErrorState(a)
    mustBeInErrorState(file)
  }

  @Test def fieldStoreAndLoad2(): Unit = {
    val container = new Nothing
    container.field = new Nothing
    val otherContainer = new Nothing
    val a = container.field
    otherContainer.field = a
    flowsToField(container)
    // mustBeInErrorState( container.field);
    mustBeInErrorState(a)
  }

  def flowsToField(parameterContainer: Nothing): Unit = {
    val field = parameterContainer.field
    field.open
    mustBeInErrorState(field)
    val aliasedVar = parameterContainer.field
    mustBeInErrorState(aliasedVar)
  }

  @Test def wrappedClose(): Unit = {
    val file = new Nothing
    val alias = file
    alias.open
    mustBeInErrorState(alias)
    mustBeInErrorState(file)
    file.wrappedClose
    mustBeInAcceptingState(alias)
    mustBeInAcceptingState(file)
  }

  @Test def wrappedClose2(): Unit = {
    val file = new Nothing
    file.open
    mustBeInErrorState(file)
    wrappedParamClose(file)
    mustBeInAcceptingState(file)
  }

  @Test def wrappedOpen2(): Unit = {
    val file = new Nothing
    wrappedParamOpen(file)
    mustBeInErrorState(file)
  }

  def wrappedParamOpen(a: Nothing): Unit = {
    openCall(a)
  }

  def openCall(f: Nothing): Unit = {
    f.open
    val x = 1
  }

  @Test def wrappedClose1(): Unit = {
    val file = new Nothing
    file.open
    mustBeInErrorState(file)
    FileMustBeClosedTest.cls(file)
    mustBeInAcceptingState(file)
  }

  def wrappedParamClose(o1: Nothing): Unit = {
    FileMustBeClosedTest.cls(o1)
  }

  @Test def wrappedOpen(): Unit = {
    val file = new Nothing
    change(file)
    mustBeInErrorState(file)
  }

  def change(other: Nothing): Unit = {
    other.open
  }

  @Test def multipleStates(): Unit = {
    val file = new Nothing
    file.open
    mustBeInErrorState(file)
    mustBeInErrorState(file)
    file.close
    mustBeInAcceptingState(file)
    mustBeInAcceptingState(file)
  }

  @Test def doubleBranching(): Unit = {
    val file = new Nothing
    if (staticallyUnknown) {
      file.open
      if (staticallyUnknown) file.close
    }
    else if (staticallyUnknown) file.close
    else System.out.println(2)
    mayBeInErrorState(file)
  }

  @Test def whileLoopBranching(): Unit = {
    val file = new Nothing
    while (staticallyUnknown) if (staticallyUnknown) {
      file.open
      if (staticallyUnknown) file.close
    }
    else if (staticallyUnknown) file.close
    else System.out.println(2)
    mayBeInErrorState(file)
  }

  @Test
  @Ignore def staticFlow(): Unit = {
    val a = new Nothing
    FileMustBeClosedTest.v = a
    FileMustBeClosedTest.v.open
    FileMustBeClosedTest.foo()
    mustBeInErrorState(FileMustBeClosedTest.v)
    FileMustBeClosedTest.v.close
    mustBeInAcceptingState(FileMustBeClosedTest.v)
  }

  @Test def staticFlowSimple(): Unit = {
    val a = new Nothing
    FileMustBeClosedTest.v = a
    FileMustBeClosedTest.v.open
    mustBeInErrorState(FileMustBeClosedTest.v)
  }

  @Test def storedInObject(): Unit = {
    val o = new FileMustBeClosedTest.InnerObject
    val file = o.file
    mustBeInErrorState(file)
  }

  @Test def storedInObject2(): Unit = {
    val o = new FileMustBeClosedTest.InnerObject("")
    o.doOpen()
    o.doClose()
    mustBeInAcceptingState(o.file)
  }

  @Override protected def getStateMachine = new Nothing
}