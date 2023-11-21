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
package test.cases.exceptions

import org.junit.Ignore
import org.junit.Test
import test.cases.fields.Alloc
import test.core.AbstractBoomerangTest

@Ignore object ExceptionTest {
  private class MyRuntimeException(private[exceptions] var field: Nothing) extends Nothing {
  }

  private class MyException(private[exceptions] var field: Nothing) extends Nothing {
  }
}

@Ignore class ExceptionTest extends Nothing {
  @Test def compiletimeExceptionFlow(): Unit = {
    try throwException()
    catch {
      case e: ExceptionTest.MyException =>
        val `object` = e.field
        queryFor(e)
    }
  }

  @Test def runtimeExceptionFlow(): Unit = {
    try throwRuntimeException()
    catch {
      case e: ExceptionTest.MyRuntimeException =>
        val `object` = e.field
        queryFor(e)
    }
  }

  private def throwRuntimeException(): Unit = {
    new ExceptionTest.MyRuntimeException(new Nothing)
  }

  @throws[MyException]
  private def throwException(): Unit = {
    throw new ExceptionTest.MyException(new Nothing)
  }
}