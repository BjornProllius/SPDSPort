package boomerang.weights

import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import com.google.common.collect.Maps
import com.google.common.collect.Sets
import java.util
import wpds.impl.Weight

object PathConditionWeight {
  private var one: PathConditionWeight = null

  def one: PathConditionWeight = {
    if (one == null) one = new PathConditionWeight("ONE")
    one
  }

  object ConditionDomain extends Enumeration {
    type ConditionDomain = Value
    val TRUE, FALSE, TOP = Value
  }
}

class PathConditionWeight extends Nothing {
  private var ifStatements = Maps.newHashMap
  private var variableToValue = Maps.newHashMap
  private var returnVals = Sets.newHashSet
  private var calleeToCallSite = Maps.newHashMap
  private var rep: Nothing = null

  def this(rep: Nothing) {
    this()
    this.rep = rep
  }

  def this(callSite: Nothing, callee: Nothing) {
    this()
    this.calleeToCallSite.put(callee, callSite)
  }

  def this(returnVal: Nothing) {
    this()
    this.returnVals.add(returnVal)
  }

  def this(ifStatements: Nothing, variableToValue: Nothing, returnVals: Nothing, calleeToCallSiteMapping: Nothing) {
    this()
    this.ifStatements = ifStatements
    this.variableToValue = variableToValue
    this.returnVals = returnVals
    this.calleeToCallSite = calleeToCallSiteMapping
  }

  def this(ifStatement: Nothing, condition: Nothing) {
    this()
    ifStatements.put(ifStatement, if (condition) PathConditionWeight.ConditionDomain.TRUE
    else PathConditionWeight.ConditionDomain.FALSE)
  }

  def this(`val`: Nothing, c: PathConditionWeight.ConditionDomain) {
    this()
    variableToValue.put(`val`, c)
  }

  @Override def extendWith(o: Nothing): Nothing = {
    if (!o.isInstanceOf[PathConditionWeight]) throw new Nothing("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[PathConditionWeight]
    val newIfs = Maps.newHashMap
    newIfs.putAll(ifStatements)
    import scala.collection.JavaConversions._
    for (e <- other.ifStatements.entrySet) {
      if (newIfs.containsKey(e.getKey) && e.getValue.equals(PathConditionWeight.ConditionDomain.TOP)) newIfs.put(e.getKey, PathConditionWeight.ConditionDomain.TOP)
      else newIfs.put(e.getKey, e.getValue)
    }
    val newVals = Maps.newHashMap
    newVals.putAll(variableToValue)
    import scala.collection.JavaConversions._
    for (e <- other.variableToValue.entrySet) {
      if (newVals.containsKey(e.getKey) && e.getValue.equals(PathConditionWeight.ConditionDomain.TOP)) newVals.put(e.getKey, PathConditionWeight.ConditionDomain.TOP)
      else newVals.put(e.getKey, e.getValue)
    }
    // May become a performance bottleneck
    val returnToAssignedVariableMap = Maps.newHashMap
    if (!returnVals.isEmpty) {
      import scala.collection.JavaConversions._
      for (v <- newVals.entrySet) {
        if (returnVals.contains(v.getKey)) {
          val s = calleeToCallSite.get(v.getKey.m)
          if (s != null && s.isAssign) {
            val leftOp = s.getLeftOp
            returnToAssignedVariableMap.put(leftOp, v.getValue)
          }
        }
      }
    }
    newVals.putAll(returnToAssignedVariableMap)
    val newReturnVals = Sets.newHashSet(returnVals)
    newReturnVals.addAll(other.returnVals)
    val calleeToCallSiteMapping = Maps.newHashMap(calleeToCallSite)
    calleeToCallSiteMapping.putAll(other.calleeToCallSite)
    new PathConditionWeight(newIfs, newVals, newReturnVals, calleeToCallSiteMapping)
  }

  @Override def combineWith(o: Nothing): Nothing = {
    if (!o.isInstanceOf[PathConditionWeight]) throw new Nothing("Cannot extend to different types of weight!")
    val other = o.asInstanceOf[PathConditionWeight]
    val newIfs = Maps.newHashMap
    import scala.collection.JavaConversions._
    for (e <- ifStatements.entrySet) {
      if (other.ifStatements.containsKey(e.getKey)) {
        val otherVal = other.ifStatements.get(e.getKey)
        if (e.getValue.equals(otherVal)) newIfs.put(e.getKey, otherVal)
        else newIfs.put(e.getKey, PathConditionWeight.ConditionDomain.TOP)
      }
      else newIfs.put(e.getKey, e.getValue)
    }
    import scala.collection.JavaConversions._
    for (e <- other.ifStatements.entrySet) {
      if (!ifStatements.containsKey(e.getKey)) newIfs.put(e.getKey, e.getValue)
    }
    val newVals = Maps.newHashMap
    import scala.collection.JavaConversions._
    for (e <- variableToValue.entrySet) {
      if (other.variableToValue.containsKey(e.getKey)) {
        val otherVal = other.variableToValue.get(e.getKey)
        if (e.getValue.equals(otherVal)) newVals.put(e.getKey, otherVal)
        else newVals.put(e.getKey, PathConditionWeight.ConditionDomain.TOP)
      }
      else newVals.put(e.getKey, e.getValue)
    }
    import scala.collection.JavaConversions._
    for (e <- other.variableToValue.entrySet) {
      if (!variableToValue.containsKey(e.getKey)) newVals.put(e.getKey, e.getValue)
    }
    val returnToAssignedVariableMap = Maps.newHashMap
    if (!returnVals.isEmpty) {
      import scala.collection.JavaConversions._
      for (v <- newVals.entrySet) {
        if (returnVals.contains(v.getKey)) {
          val s = calleeToCallSite.get(v.getKey.m)
          if (s != null && s.isAssign) {
            val leftOp = s.getLeftOp
            returnToAssignedVariableMap.put(leftOp, v.getValue)
          }
        }
      }
    }
    newVals.putAll(returnToAssignedVariableMap)
    val newReturnVals = Sets.newHashSet(returnVals)
    newReturnVals.addAll(other.returnVals)
    val calleeToCallSiteMapping = Maps.newHashMap(calleeToCallSite)
    calleeToCallSiteMapping.putAll(other.calleeToCallSite)
    new PathConditionWeight(newIfs, newVals, newReturnVals, calleeToCallSiteMapping)
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (ifStatements == null) 0
    else ifStatements.hashCode)
    result = prime * result + (if (variableToValue == null) 0
    else variableToValue.hashCode)
    result = prime * result + (if (rep == null) 0
    else rep.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[PathConditionWeight]
    if (ifStatements == null) if (other.ifStatements != null) return false
    else if (!ifStatements.equals(other.ifStatements)) return false
    if (variableToValue == null) if (other.variableToValue != null) return false
    else if (!variableToValue.equals(other.variableToValue)) return false
    if (rep == null) if (other.rep != null) return false
    else if (!rep.equals(other.rep)) return false
    true
  }

  def getConditions: Nothing = ifStatements

  def getEvaluationMap: Nothing = variableToValue

  @Override def toString: Nothing = "\nIf statements: " + ifStatements + " Vals: " + variableToValue
}