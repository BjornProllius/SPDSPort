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
package boomerang.scene.jimple

import com.google.common.collect.Sets
import java.util
import soot.Body
import soot.BodyTransformer
import soot.Local
import soot.MethodOrMethodContext
import soot.RefType
import soot.Scene
import soot.SootField
import soot.SootMethod
import soot.Unit
import soot.UnitPatchingChain
import soot.Value
import soot.ValueBox
import soot.jimple.ArrayRef
import soot.jimple.AssignStmt
import soot.jimple.ClassConstant
import soot.jimple.Constant
import soot.jimple.IdentityStmt
import soot.jimple.IfStmt
import soot.jimple.InstanceFieldRef
import soot.jimple.NullConstant
import soot.jimple.ReturnStmt
import soot.jimple.StaticFieldRef
import soot.jimple.Stmt
import soot.jimple.internal.JAssignStmt
import soot.jimple.internal.JInstanceFieldRef
import soot.jimple.internal.JNopStmt
import soot.jimple.internal.JReturnStmt
import soot.jimple.internal.JimpleLocal
import soot.jimple.toolkits.callgraph.ReachableMethods
import soot.tagkit.AttributeValueException
import soot.tagkit.LineNumberTag
import soot.tagkit.SourceLnPosTag
import soot.tagkit.Tag
import soot.util.Chain
import soot.util.queue.QueueReader

object BoomerangPretransformer {
  var TRANSFORM_CONSTANTS = true
  var UNITIALIZED_FIELD_TAG_NAME = "UnitializedField"
  var UNITIALIZED_FIELD_TAG: Nothing = new Nothing() {
    @Override def getName: Nothing = UNITIALIZED_FIELD_TAG_NAME

    @Override
    @throws[AttributeValueException]
    def getValue = new Array[Byte](0)
  }
  private var instance: BoomerangPretransformer = null

  private def addNulliefiedFields(cons: Nothing): Unit = {
    val fields = cons.getDeclaringClass.getFields
    val units = cons.getActiveBody.getUnits
    val fieldsDefinedInMethod = getFieldsDefinedInMethod(cons, Sets.newHashSet)
    import scala.collection.JavaConversions._
    for (f <- fields) {
      if (fieldsDefinedInMethod.contains(f)) continue //todo: continue is not supported
      if (f.isStatic) continue //todo: continue is not supported
      if (f.isFinal) continue //todo: continue is not supported
      if (f.getType.isInstanceOf[Nothing]) {
        val jAssignStmt = new Nothing(new Nothing(cons.getActiveBody.getThisLocal, f.makeRef), NullConstant.v)
        jAssignStmt.addTag(new Nothing(2))
        jAssignStmt.addTag(UNITIALIZED_FIELD_TAG)
        val lastIdentityStmt = findLastIdentityStmt(units)
        if (lastIdentityStmt != null) units.insertAfter(jAssignStmt, lastIdentityStmt)
        else units.addFirst(jAssignStmt)
      }
    }
  }

  private def findLastIdentityStmt(units: Nothing): Nothing = {
    import scala.collection.JavaConversions._
    for (u <- units) {
      if (u.isInstanceOf[Nothing] && u.isInstanceOf[Nothing]) continue //todo: continue is not supported
      return u
    }
    null
  }

  private def getFieldsDefinedInMethod(cons: Nothing, visited: Nothing): Nothing = {
    val res = Sets.newHashSet
    if (!visited.add(cons)) return res
    if (!cons.hasActiveBody) return res
    import scala.collection.JavaConversions._
    for (u <- cons.getActiveBody.getUnits) {
      if (u.isInstanceOf[Nothing]) {
        val as = u.asInstanceOf[Nothing]
        val left = as.getLeftOp
        if (left.isInstanceOf[Nothing]) {
          val ifr = left.asInstanceOf[Nothing]
          res.add(ifr.getField)
        }
      }
      if (u.isInstanceOf[Nothing]) {
        val stmt = u.asInstanceOf[Nothing]
        if (stmt.containsInvokeExpr) if (stmt.getInvokeExpr.getMethod.isConstructor) res.addAll(getFieldsDefinedInMethod(stmt.getInvokeExpr.getMethod, visited))
      }
    }
    res
  }

  def v: BoomerangPretransformer = {
    if (instance == null) instance = new BoomerangPretransformer
    instance
  }
}

class BoomerangPretransformer extends Nothing {
  private var replaceCounter = 0
  private var applied = false

  @Override protected def internalTransform(b: Nothing, phaseName: Nothing, options: Nothing): Unit = {
    addNopStmtToMethods(b)
    if (BoomerangPretransformer.TRANSFORM_CONSTANTS) transformConstantAtFieldWrites(b)
  }

  private def transformConstantAtFieldWrites(body: Nothing): Unit = {
    val cwnc = getStmtsWithConstants(body)
    import scala.collection.JavaConversions._
    for (u <- cwnc) {
      if (u.isInstanceOf[Nothing]) {
        val assignStmt = u.asInstanceOf[Nothing]
        if (isFieldRef(assignStmt.getLeftOp) && assignStmt.getRightOp.isInstanceOf[Nothing] && !assignStmt.getRightOp.isInstanceOf[Nothing]) {
          val label = "varReplacer" + new Nothing({
            replaceCounter += 1; replaceCounter - 1
          }).toString
          val paramVal = new Nothing(label, assignStmt.getRightOp.getType)
          val newUnit = new Nothing(paramVal, assignStmt.getRightOp)
          body.getLocals.add(paramVal)
          body.getUnits.insertBefore(newUnit, u)
          val other = new Nothing(assignStmt.getLeftOp, paramVal)
          other.addAllTagsOf(u)
          body.getUnits.insertBefore(other, u)
          body.getUnits.remove(u)
        }
      }
      if (u.isInstanceOf[Nothing] && u.asInstanceOf[Nothing].containsInvokeExpr && !u.toString.contains("test.assertions.Assertions:") && !u.toString.contains("intQueryFor")) {
        val stmt = u.asInstanceOf[Nothing]
        if (stmt.getInvokeExpr.getMethod.getSignature.equals("<java.math.BigInteger: java.math.BigInteger valueOf(long)>")) continue //todo: continue is not supported
        val useBoxes = stmt.getInvokeExpr.getUseBoxes
        import scala.collection.JavaConversions._
        for (v <- stmt.getInvokeExpr.getArgs) {
          if (v.isInstanceOf[Nothing] && !v.isInstanceOf[Nothing]) {
            val label = "varReplacer" + new Nothing({
              replaceCounter += 1; replaceCounter - 1
            }).toString
            val paramVal = new Nothing(label, v.getType)
            val newUnit = new Nothing(paramVal, v)
            newUnit.addAllTagsOf(u)
            body.getLocals.add(paramVal)
            body.getUnits.insertBefore(newUnit, u)
            import scala.collection.JavaConversions._
            for (b <- useBoxes) {
              backPropagateSourceLineTags(b, newUnit)
              if (b.getValue.equals(v)) b.setValue(paramVal)
            }
          }
        }
      }
      if (u.isInstanceOf[Nothing]) {
        val returnStmt = u.asInstanceOf[Nothing]
        val label = "varReplacer" + new Nothing({
          replaceCounter += 1; replaceCounter - 1
        }).toString
        val paramVal = new Nothing(label, returnStmt.getOp.getType)
        val newUnit = new Nothing(paramVal, returnStmt.getOp)
        newUnit.addAllTagsOf(u)
        body.getLocals.add(paramVal)
        body.getUnits.insertBefore(newUnit, u)
        val other = new Nothing(paramVal)
        body.getUnits.insertBefore(other, u)
        body.getUnits.remove(u)
      }
    }
  }

  /**
   * Propagates back the line number tags from the constant value box to the newly created
   * AssignStmt, to revert the forward propagation done in {@link
 * soot.jimple.toolkits.scalar.CopyPropagator}
   *
   * @param valueBox
   * @param assignStmt
   */
  private def backPropagateSourceLineTags(valueBox: Nothing, assignStmt: Nothing): Unit = {
    var tag = valueBox.getTag(SourceLnPosTag.IDENTIFIER)
    if (tag != null) {
      // in case that we copied a line number tag from the original statement, we want to remove
      // that now since the valueBox contains the correct lin number tag for the assign statement as
      // it was before copy propagation
      assignStmt.removeTag(SourceLnPosTag.IDENTIFIER)
      assignStmt.addTag(tag)
    }
    tag = valueBox.getTag(LineNumberTag.IDENTIFIER)
    if (tag != null) {
      // same as for the above case
      assignStmt.removeTag(LineNumberTag.IDENTIFIER)
      assignStmt.addTag(tag)
    }
  }

  /**
   * The first statement of a method must be a nop statement, because the call-flow functions do
   * only map parameters to arguments. If the first statement of a method would be an assign
   * statement, the analysis misses data-flows.
   */
  private def addNopStmtToMethods(b: Nothing): Unit = {
    var nopStmt = new Nothing
    import scala.collection.JavaConversions._
    for (u <- b.getUnits) {
      if (u.getJavaSourceStartLineNumber > 0) {
        nopStmt.addAllTagsOf(u)
        break //todo: break is not supported
      }
    }
    b.getUnits.insertBefore(nopStmt, b.getUnits.getFirst)
    val ifStmts = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (u <- b.getUnits) {
      if (u.isInstanceOf[Nothing]) {
        // ((IfStmt) u).getTarget();
        ifStmts.add(u.asInstanceOf[Nothing])
      }
    }
    // After all if-stmts we add a nop-stmt to make the analysis
    import scala.collection.JavaConversions._
    for (ifStmt <- ifStmts) {
      nopStmt = new Nothing
      nopStmt.addAllTagsOf(ifStmt)
      b.getUnits.insertAfter(nopStmt, ifStmt)
      val target = ifStmt.getTarget
      nopStmt = new Nothing
      nopStmt.addAllTagsOf(target)
      b.getUnits.insertBefore(nopStmt, target)
      ifStmt.setTarget(nopStmt)
    }
  }

  private def getStmtsWithConstants(methodBody: Nothing) = {
    val retMap = Sets.newHashSet
    import scala.collection.JavaConversions._
    for (u <- methodBody.getUnits) {
      if (u.isInstanceOf[Nothing]) {
        val assignStmt = u.asInstanceOf[Nothing]
        if (isFieldRef(assignStmt.getLeftOp) && assignStmt.getRightOp.isInstanceOf[Nothing]) retMap.add(u)
      }
      if (u.isInstanceOf[Nothing] && u.asInstanceOf[Nothing].containsInvokeExpr) {
        val stmt = u.asInstanceOf[Nothing]
        import scala.collection.JavaConversions._
        for (v <- stmt.getInvokeExpr.getArgs) {
          if (v.isInstanceOf[Nothing]) retMap.add(u)
        }
      }
      if (u.isInstanceOf[Nothing]) {
        val assignStmt = u.asInstanceOf[Nothing]
        if (assignStmt.getOp.isInstanceOf[Nothing]) retMap.add(u)
      }
    }
    retMap
  }

  private def isFieldRef(op: Nothing) = op.isInstanceOf[Nothing] || op.isInstanceOf[Nothing] || op.isInstanceOf[Nothing]

  def apply(): Unit = {
    if (applied) return
    val reachableMethods = Scene.v.getReachableMethods
    val listener = reachableMethods.listener
    while (listener.hasNext) {
      val method = listener.next.method
      if (method.hasActiveBody) {
        if (method.isConstructor) BoomerangPretransformer.addNulliefiedFields(method)
        internalTransform(method.getActiveBody, "", new Nothing)
      }
    }
    applied = true
  }

  def isApplied: Boolean = applied

  def reset(): Unit = {
    BoomerangPretransformer.instance = null
  }
}