/**
 * ***************************************************************************** Copyright (c) 2020
 * CodeShield GmbH, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package boomerang.scene.wala

import boomerang.scene.ControlFlowGraph
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.scene.WrappedClass
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import com.ibm.wala.analysis.typeInference.TypeInference
import com.ibm.wala.cast.ir.ssa.AstIRFactory.AstIR
import com.ibm.wala.cast.java.analysis.typeInference.AstJavaTypeInference
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.ssa.IR
import java.util

class WALAMethod(private var delegate: Nothing, private var ir: Nothing, private var cha: Nothing) extends Nothing {
  if (ir.isInstanceOf[Nothing]) this.typeInference = new Nothing(ir, true)
  else this.typeInference = TypeInference.make(ir, true)
  this.cfg = new Nothing(this, cha)
  private var cfg: Nothing = null
  private var paramLocalCache: Nothing = null
  private var valueCache: Nothing = null
  private var typeInference: Nothing = null

  @Override def isStaticInitializer: Boolean = delegate.isClinit

  @Override def isParameterLocal(`val`: Nothing): Boolean = getParameterLocals.contains(`val`)

  @Override def isThisLocal(`val`: Nothing): Boolean = getThisLocal.equals(`val`)

  @Override def getLocals: Nothing = {
    if (valueCache == null) {
      valueCache = Sets.newHashSet
      var i = 0
      while (i <= ir.getSymbolTable.getMaxValueNumber) {
        valueCache.add(new Nothing(i, this))
        i += 1
      }
    }
    valueCache
  }

  @Override def getThisLocal = new Nothing(ir.getSymbolTable.getParameter(0), this)

  @Override def getParameterLocals: Nothing = {
    if (paramLocalCache == null) {
      paramLocalCache = Lists.newArrayList
      var i = if (isStatic) 0
      else 1
      while (i < ir.getSymbolTable.getNumberOfParameters) {
        paramLocalCache.add(new Nothing(ir.getParameter(i), this))
        i += 1
      }
    }
    paramLocalCache
  }

  @Override def isStatic: Boolean = delegate.isStatic

  @Override def isNative: Boolean = delegate.isNative

  @Override def getStatements: Nothing = getControlFlowGraph.getStatements

  @Override def getDeclaringClass = new Nothing(delegate.getDeclaringClass.getReference)

  @Override def getControlFlowGraph: Nothing = cfg

  @Override def getSubSignature: Nothing = delegate.getSelector.toString

  @Override def getName: Nothing = delegate.getName.toString

  def getDelegate: Nothing = delegate

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (delegate == null) 0
    else delegate.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[WALAMethod]
    if (delegate == null) if (other.delegate != null) return false
    else if (!delegate.equals(other.delegate)) return false
    true
  }

  private[wala] def getIR = ir

  @Override def toString: Nothing = delegate.getSignature

  def getTypeInference: Nothing = typeInference

  @Override def isConstructor: Boolean = delegate.isInit

  @Override def isPublic: Boolean = delegate.isPublic

  def getBranchTarget(target: Int): Nothing = cfg.getBranchTarget(target)
}