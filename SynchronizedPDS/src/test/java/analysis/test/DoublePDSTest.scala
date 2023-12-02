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
package analysis.test

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import com.google.common.collect.HashMultimap
import com.google.common.collect.Multimap
import java.util
import org.junit.Test
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.OneWeightFunctions
import sync.pds.solver.SyncPDSSolver
import sync.pds.solver.SyncPDSSolver.PDSSystem
import sync.pds.solver.WeightFunctions
import sync.pds.solver.nodes.ExclusionNode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.NodeWithLocation
import sync.pds.solver.nodes.PopNode
import sync.pds.solver.nodes.PushNode
import sync.pds.solver.nodes.SingleNode
import wpds.impl.SummaryNestedWeightedPAutomatons
import wpds.impl.Weight.NoWeight
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.wildcard.ExclusionWildcard
import wpds.wildcard.Wildcard

object DoublePDSTest {
  private val LOGGER = LoggerFactory.getLogger(classOf[DoublePDSTest])

  private def returnSite(call: Int) = new DoublePDSTest.Statement(call)

  private def f(f: Nothing) = new DoublePDSTest.FieldRef(f)

  def node(stmt: Int, `var`: Nothing) = new Nothing(new DoublePDSTest.Statement(stmt), new DoublePDSTest.Variable(`var`))

  private class Statement(name: Int) extends DoublePDSTest.StringBasedObj(Integer.toString(name)) with Nothing {
    @Override def accepts(other: Nothing): Boolean = this == other
  }

  private class Variable(name: Nothing) extends DoublePDSTest.StringBasedObj(name) {
  }

  private class FieldWildCard extends DoublePDSTest.FieldRef("*") with Nothing {
  }

  private class ExclusionWildcardField(private val excludes: DoublePDSTest.FieldRef) extends DoublePDSTest.FieldRef(excl.name) with Nothing {
    @Override def excludes: DoublePDSTest.FieldRef = excludes.asInstanceOf[DoublePDSTest.FieldRef]

    @Override override def toString: Nothing = "not " + super.toString

    @Override override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (excludes == null) 0
      else excludes.hashCode)
      result
    }

    @Override override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[DoublePDSTest.ExclusionWildcardField]
      if (excludes == null) if (other.excludes != null) return false
      else if (!(excludes == other.excludes)) return false
      true
    }
  }

  private class FieldRef(name: Nothing) extends DoublePDSTest.StringBasedObj(name) with Nothing {
    @Override def accepts(other: Nothing): Boolean = this == other
  }

  private class StringBasedObj(private[test] val name: Nothing) {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (name == null) 0
      else name.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[DoublePDSTest.StringBasedObj]
      if (name == null) if (other.name != null) return false
      else if (!name.equals(other.name)) return false
      true
    }

    @Override def toString: Nothing = name
  }
}

class DoublePDSTest {
  private val successorMap = HashMultimap.create
  private val summaryMap = HashMultimap.create

  private def addFieldPop(curr: Nothing, ref: DoublePDSTest.FieldRef, succ: Nothing): Unit = {
    addSucc(curr, new Nothing(new Nothing(succ.stmt, succ.fact, ref), PDSSystem.FIELDS))
  }

  private def addFieldPush(curr: Nothing, push: DoublePDSTest.FieldRef, succ: Nothing): Unit = {
    addSucc(curr, new Nothing(succ.stmt, succ.fact, push, PDSSystem.FIELDS))
  }

  private def addNormal(curr: Nothing, succ: Nothing): Unit = {
    addSucc(curr, succ)
  }

  private def addReturnFlow(curr: Nothing, returns: DoublePDSTest.Variable): Unit = {
    addSucc(curr, new Nothing(returns, PDSSystem.CALLS))
  }

  private def addCallFlow(curr: Nothing, succ: Nothing, returnSite: DoublePDSTest.Statement): Unit = {
    addSucc(curr, new Nothing(succ.stmt, succ.fact, returnSite, PDSSystem.CALLS))
  }

  private def calleeToCallerMapping(ret: Nothing, succOfRet: Nothing): Unit = {
    summaryMap.put(ret, succOfRet)
  }

  private def addSucc(curr: Nothing, succ: Nothing): Unit = {
    successorMap.put(curr, succ)
  }

  private def addExcludeField(curr: Nothing, push: DoublePDSTest.FieldRef, succ: Nothing): Unit = {
    addSucc(curr, new Nothing(succ.stmt, succ.fact, push))
  }

  private val epsilonField = new DoublePDSTest.FieldRef("EMPTY")
  private val epsilonCallSite = new DoublePDSTest.Statement(-1)
  private val solver = new DoublePDSTest#TestSyncPDSSolver

  private class TestSyncPDSSolver extends Nothing(false, new Nothing, false, new Nothing, -1, -1, -1) {
    @Override def computeSuccessor(node: Nothing): Unit = {
      val states = successorMap.get(node)
      import scala.collection.JavaConversions._
      for (s <- states) {
        propagate(node, s)
      }
    }

    @Override def epsilonField = new DoublePDSTest.FieldRef("eps_f")

    @Override def epsilonStmt: DoublePDSTest.Statement = epsilonCallSite

    @Override def emptyField: DoublePDSTest.FieldRef = epsilonField

    @Override def fieldWildCard = new DoublePDSTest.FieldWildCard

    @Override def exclusionFieldWildCard(exclusion: DoublePDSTest.FieldRef) = new DoublePDSTest.ExclusionWildcardField(exclusion)

    @Override def applyCallSummary(callSite: DoublePDSTest.Statement, factInCallee: DoublePDSTest.Variable, sPInCallee: DoublePDSTest.Statement, exitStmt: DoublePDSTest.Statement, exitingFact: DoublePDSTest.Variable): Unit = {
      val exitingNode = new Nothing(exitStmt, exitingFact)
      import scala.collection.JavaConversions._
      for (n <- summaryMap.get(exitingNode)) {
        addNormalFieldFlow(exitingNode, n)
        addNormalCallFlow(new Nothing(callSite, exitingFact), n)
      }
    }

    @Override def getFieldWeights = new Nothing(NoWeight.NO_WEIGHT_ONE)

    @Override def getCallWeights = new Nothing(NoWeight.NO_WEIGHT_ONE)
  }

  private def solve(node: Nothing): Unit = {
    solver.solve(node, epsilonField, new Nothing(DoublePDSTest.node(1, "u")), epsilonCallSite, new Nothing(new DoublePDSTest.Variable("u")))
    DoublePDSTest.LOGGER.info("All reachable states of SPDS: {}", solver.getReachedStates)
  }

  @Test def test1(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addCallFlow(DoublePDSTest.node(2, "v"), DoublePDSTest.node(3, "p"), DoublePDSTest.returnSite(5))
    addFieldPush(DoublePDSTest.node(3, "p"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "q"))
    addReturnFlow(DoublePDSTest.node(4, "q"), `var`("q"))
    addFieldPop(DoublePDSTest.node(5, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(6, "x"))
    addFieldPop(DoublePDSTest.node(6, "x"), DoublePDSTest.f("f"), DoublePDSTest.node(7, "y"))
    // second branch
    addFieldPush(DoublePDSTest.node(8, "r"), DoublePDSTest.f("f"), DoublePDSTest.node(9, "s"))
    addCallFlow(DoublePDSTest.node(9, "s"), DoublePDSTest.node(3, "p"), DoublePDSTest.returnSite(10))
    addReturnFlow(DoublePDSTest.node(4, "q"), `var`("q"))
    addFieldPush(DoublePDSTest.node(10, "t"), DoublePDSTest.f("f"), DoublePDSTest.node(11, "s"))
    calleeToCallerMapping(DoublePDSTest.node(4, "q"), DoublePDSTest.node(5, "w"))
    calleeToCallerMapping(DoublePDSTest.node(4, "q"), DoublePDSTest.node(10, "t"))
    solve(DoublePDSTest.node(1, "u"))
    // TODO needs inspection
    // assertFalse(solver.getReachedStates().contains(node(11, "s")));
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(5, "w")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(6, "x")))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(7, "y")))
  }

  @Test def branching(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("g"), DoublePDSTest.node(3, "x"))
    // can pop
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(5, "y"))
    addFieldPop(DoublePDSTest.node(3, "x"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "y"))
    // but cannot pop
    addFieldPop(DoublePDSTest.node(5, "y"), DoublePDSTest.f("g"), DoublePDSTest.node(6, "y"))
    addFieldPop(DoublePDSTest.node(4, "y"), DoublePDSTest.f("g"), DoublePDSTest.node(7, "y"))
    solve(DoublePDSTest.node(1, "u"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(5, "y")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(4, "y")))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(6, "y")))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(7, "y")))
  }

  @Test def tooMuchPopping(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("g"), DoublePDSTest.node(3, "x"))
    addFieldPop(DoublePDSTest.node(3, "x"), DoublePDSTest.f("g"), DoublePDSTest.node(3, "y"))
    addFieldPop(DoublePDSTest.node(3, "y"), DoublePDSTest.f("g"), DoublePDSTest.node(3, "z"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(3, "y")))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(3, "z")))
  }

  @Test def test1Simple(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addCallFlow(DoublePDSTest.node(2, "v"), DoublePDSTest.node(3, "p"), DoublePDSTest.returnSite(5))
    addFieldPush(DoublePDSTest.node(3, "p"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "q"))
    addReturnFlow(DoublePDSTest.node(4, "q"), `var`("q"))
    addFieldPop(DoublePDSTest.node(5, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(6, "x"))
    addFieldPop(DoublePDSTest.node(6, "x"), DoublePDSTest.f("f"), DoublePDSTest.node(7, "y"))
    calleeToCallerMapping(DoublePDSTest.node(4, "q"), DoublePDSTest.node(5, "w"))
    solve(DoublePDSTest.node(1, "u"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(6, "x")))
  }

  @Test def callOnlyIntraprocedural(): Unit = {
    addNormal(DoublePDSTest.node(1, "u"), DoublePDSTest.node(5, "q"))
    addNormal(DoublePDSTest.node(5, "q"), DoublePDSTest.node(6, "x"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(6, "x")))
  }

  @Test def fieldPushAndPop(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(6, "x"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(6, "x")))
  }

  @Test def simpleNonFieldFlow(): Unit = {
    addNormal(DoublePDSTest.node(1, "v"), DoublePDSTest.node(2, "w"))
    addCallFlow(DoublePDSTest.node(2, "w"), DoublePDSTest.node(3, "p"), DoublePDSTest.returnSite(4))
    addNormal(DoublePDSTest.node(3, "p"), DoublePDSTest.node(5, "q"))
    addNormal(DoublePDSTest.node(5, "q"), DoublePDSTest.node(7, "z"))
    addNormal(DoublePDSTest.node(7, "z"), DoublePDSTest.node(6, "x"))
    addReturnFlow(DoublePDSTest.node(6, "x"), `var`("x"))
    addNormal(DoublePDSTest.node(4, "k"), DoublePDSTest.node(6, "y"))
    calleeToCallerMapping(DoublePDSTest.node(6, "x"), DoublePDSTest.node(4, "k"))
    solve(DoublePDSTest.node(1, "v"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(6, "y")))
  }

  @Test def simpleExclusionFieldFlow(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "v"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "w"))
    addExcludeField(DoublePDSTest.node(4, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(5, "w"))
    addFieldPop(DoublePDSTest.node(5, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(7, "w"))
    solve(DoublePDSTest.node(1, "v"))
    solver.debugOutput
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(7, "w")))
  }

  @Test def simpleNegativeExclusionFieldFlow(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "v"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "w"))
    addExcludeField(DoublePDSTest.node(4, "w"), DoublePDSTest.f("h"), DoublePDSTest.node(5, "w")) // overwrite of h should not affect the subsequent pop

    // operation
    addFieldPop(DoublePDSTest.node(5, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(7, "w"))
    solve(DoublePDSTest.node(1, "v"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "w")))
  }

  @Test def doubleNegativeExclusionFieldFlow(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "v"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "w"))
    addExcludeField(DoublePDSTest.node(4, "w"), DoublePDSTest.f("h"), DoublePDSTest.node(5, "w")) // overwrite of h should not affect the subsequent pop

    // operation
    addExcludeField(DoublePDSTest.node(5, "w"), DoublePDSTest.f("i"), DoublePDSTest.node(6, "w")) // overwrite of h should not affect the subsequent pop

    // operation
    addFieldPop(DoublePDSTest.node(6, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(7, "w"))
    solve(DoublePDSTest.node(1, "v"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "w")))
  }

  @Test def doubleExclusionFieldFlow(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "v"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "w"))
    addExcludeField(DoublePDSTest.node(4, "w"), DoublePDSTest.f("i"), DoublePDSTest.node(5, "w")) // overwrite of i should not affect the subsequent pop

    // operation
    addExcludeField(DoublePDSTest.node(5, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(6, "w"))
    addFieldPop(DoublePDSTest.node(6, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(7, "w"))
    solve(DoublePDSTest.node(1, "v"))
    solver.debugOutput
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(7, "w")))
  }

  @Test def simpleTransitiveExclusionFieldFlow(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "v"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "w"))
    addExcludeField(DoublePDSTest.node(4, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(5, "w"))
    addNormal(DoublePDSTest.node(5, "w"), DoublePDSTest.node(6, "w"))
    addFieldPop(DoublePDSTest.node(6, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(7, "w"))
    solve(DoublePDSTest.node(1, "v"))
    solver.debugOutput
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(7, "w")))
  }

  @Test def simpleNegativeTransitiveExclusionFieldFlow(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "v"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "w"))
    addExcludeField(DoublePDSTest.node(4, "w"), DoublePDSTest.f("h"), DoublePDSTest.node(5, "w")) // overwrite of h should not affect the subsequent pop

    // operation
    addNormal(DoublePDSTest.node(5, "w"), DoublePDSTest.node(6, "w"))
    addFieldPop(DoublePDSTest.node(6, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(7, "w"))
    solve(DoublePDSTest.node(1, "v"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "w")))
  }

  @Test def testWithTwoStacks(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addCallFlow(DoublePDSTest.node(2, "v"), DoublePDSTest.node(3, "p"), DoublePDSTest.returnSite(4))
    addFieldPush(DoublePDSTest.node(3, "p"), DoublePDSTest.f("g"), DoublePDSTest.node(5, "q"))
    addReturnFlow(DoublePDSTest.node(5, "q"), `var`("q"))
    addNormal(DoublePDSTest.node(4, "w"), DoublePDSTest.node(7, "t"))
    calleeToCallerMapping(DoublePDSTest.node(5, "q"), DoublePDSTest.node(4, "w"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "t")))
  }

  @Test def testWithTwoStacksAndTwoField(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addCallFlow(DoublePDSTest.node(2, "v"), DoublePDSTest.node(3, "p"), DoublePDSTest.returnSite(4))
    addFieldPush(DoublePDSTest.node(3, "p"), DoublePDSTest.f("g"), DoublePDSTest.node(5, "q"))
    addFieldPush(DoublePDSTest.node(5, "q"), DoublePDSTest.f("f"), DoublePDSTest.node(6, "q"))
    addReturnFlow(DoublePDSTest.node(6, "q"), `var`("q"))
    addNormal(DoublePDSTest.node(4, "w"), DoublePDSTest.node(7, "t"))
    addFieldPop(DoublePDSTest.node(7, "t"), DoublePDSTest.f("f"), DoublePDSTest.node(8, "s"))
    addFieldPop(DoublePDSTest.node(8, "s"), DoublePDSTest.f("g"), DoublePDSTest.node(9, "x"))
    addFieldPop(DoublePDSTest.node(9, "x"), DoublePDSTest.f("h"), DoublePDSTest.node(10, "y"))
    addFieldPop(DoublePDSTest.node(9, "x"), DoublePDSTest.f("impossibleRead"), DoublePDSTest.node(11, "z"))
    calleeToCallerMapping(DoublePDSTest.node(6, "q"), DoublePDSTest.node(4, "w"))
    solve(DoublePDSTest.node(1, "u"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "t")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(8, "s")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(9, "x")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(10, "y")))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(11, "z")))
  }

  @Test def positiveTestFieldDoublePushAndPop(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPush(DoublePDSTest.node(2, "v"), DoublePDSTest.f("g"), DoublePDSTest.node(3, "w"))
    addFieldPop(DoublePDSTest.node(3, "w"), DoublePDSTest.f("g"), DoublePDSTest.node(4, "x"))
    addNormal(DoublePDSTest.node(3, "w"), DoublePDSTest.node(4, "kkk"))
    addFieldPop(DoublePDSTest.node(4, "x"), DoublePDSTest.f("h"), DoublePDSTest.node(5, "y"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(4, "x")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(5, "y")))
  }

  @Test def negativeTestFieldDoublePushAndPop(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPush(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(3, "w"))
    addFieldPop(DoublePDSTest.node(3, "w"), DoublePDSTest.f("h"), DoublePDSTest.node(4, "x"))
    addFieldPop(DoublePDSTest.node(4, "x"), DoublePDSTest.f("g"), DoublePDSTest.node(5, "y"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(4, "x")))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(5, "y")))
  }

  @Test def positiveTestFieldPushPushAndPop(): Unit = {
    addFieldPush(DoublePDSTest.node(0, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(1, "u"))
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "x"))
    solve(DoublePDSTest.node(0, "u"))
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(2, "x")))
  }

  @Test def negativeTestFieldPushAndPopPop(): Unit = {
    addFieldPush(DoublePDSTest.node(0, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(1, "u"))
    addFieldPop(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "x"))
    solve(DoublePDSTest.node(0, "u"))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(2, "x")))
  }

  @Test def negativeSinglePop(): Unit = {
    addNormal(DoublePDSTest.node(0, "u"), DoublePDSTest.node(1, "u"))
    addFieldPop(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    solve(DoublePDSTest.node(0, "u"))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(2, "v")))
  }

  @Test def negativeJustPop(): Unit = {
    addFieldPop(DoublePDSTest.node(0, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    solve(DoublePDSTest.node(0, "u"))
    System.out.println(solver.getReachedStates)
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(2, "v")))
  }

  @Test def positiveTestFieldPushAndPop(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "x"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(2, "x")))
  }

  @Test def positiveTestFieldIntermediatePushAndPop(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addNormal(DoublePDSTest.node(2, "v"), DoublePDSTest.node(3, "w"))
    addNormal(DoublePDSTest.node(3, "w"), DoublePDSTest.node(4, "w"))
    addFieldPop(DoublePDSTest.node(4, "w"), DoublePDSTest.f("h"), DoublePDSTest.node(5, "w"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(5, "w")))
  }

  @Test def positiveTestFieldLoop(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPush(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(3, "w"))
    addFieldPop(DoublePDSTest.node(3, "w"), DoublePDSTest.f("h"), DoublePDSTest.node(4, "x"))
    addFieldPop(DoublePDSTest.node(4, "x"), DoublePDSTest.f("h"), DoublePDSTest.node(5, "y"))
    solve(DoublePDSTest.node(1, "u"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(4, "x")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(5, "y")))
  }

  @Test def positiveTestFieldLoop2(): Unit = {
    addFieldPush(DoublePDSTest.node(0, "a"), DoublePDSTest.f("g"), DoublePDSTest.node(1, "u"))
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPush(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("h"), DoublePDSTest.node(3, "w"))
    addFieldPop(DoublePDSTest.node(3, "w"), DoublePDSTest.f("h"), DoublePDSTest.node(4, "x"))
    addFieldPop(DoublePDSTest.node(4, "x"), DoublePDSTest.f("g"), DoublePDSTest.node(5, "y"))
    solve(DoublePDSTest.node(0, "a"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(5, "y")))
  }

  @Test def positiveSummaryTest(): Unit = {
    // 1 :a.g = c
    // 4: foo(a)
    // 5: e = a.f
    // 6: foo(e)
    // 7: h = e.f
    // 2: foo(u)
    // 3: u.f = ...
    addFieldPush(DoublePDSTest.node(0, "c"), DoublePDSTest.f("g"), DoublePDSTest.node(1, "a"))
    addCallFlow(DoublePDSTest.node(1, "a"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(4))
    addFieldPush(DoublePDSTest.node(2, "u"), DoublePDSTest.f("f"), DoublePDSTest.node(3, "u"))
    addReturnFlow(DoublePDSTest.node(3, "u"), `var`("u"))
    addNormal(DoublePDSTest.node(4, "a"), DoublePDSTest.node(5, "e"))
    addCallFlow(DoublePDSTest.node(5, "e"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(6))
    addReturnFlow(DoublePDSTest.node(3, "u"), `var`("u"))
    addFieldPop(DoublePDSTest.node(6, "a"), DoublePDSTest.f("f"), DoublePDSTest.node(7, "h"))
    addFieldPop(DoublePDSTest.node(7, "h"), DoublePDSTest.f("f"), DoublePDSTest.node(8, "g"))
    addFieldPop(DoublePDSTest.node(8, "g"), DoublePDSTest.f("g"), DoublePDSTest.node(9, "z"))
    addFieldPop(DoublePDSTest.node(8, "g"), DoublePDSTest.f("f"), DoublePDSTest.node(9, "y"))
    calleeToCallerMapping(DoublePDSTest.node(3, "u"), DoublePDSTest.node(4, "a"))
    calleeToCallerMapping(DoublePDSTest.node(3, "u"), DoublePDSTest.node(6, "a"))
    solve(DoublePDSTest.node(0, "c"))
    System.out.println(solver.getReachedStates)
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "h")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(8, "g")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(9, "z")))
    // assertFalse(solver.getReachedStates().contains( node(9,"y")));//False Positive
  }

  @Test def positiveSummaryWithFieldTest(): Unit = {
    addFieldPush(DoublePDSTest.node(0, "c"), DoublePDSTest.f("g"), DoublePDSTest.node(1, "a"))
    addCallFlow(DoublePDSTest.node(1, "a"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(4))
    addFieldPush(DoublePDSTest.node(2, "u"), DoublePDSTest.f("f"), DoublePDSTest.node(3, "u"))
    addReturnFlow(DoublePDSTest.node(3, "u"), `var`("u"))
    addFieldPop(DoublePDSTest.node(4, "a"), DoublePDSTest.f("f"), DoublePDSTest.node(5, "e"))
    addCallFlow(DoublePDSTest.node(5, "e"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(6))
    // addReturnFlow(node(3,"u"),var("e"));
    addFieldPop(DoublePDSTest.node(6, "a"), DoublePDSTest.f("f"), DoublePDSTest.node(7, "h")) // Due to the summary, we should be able to read f again.
    addFieldPop(DoublePDSTest.node(7, "h"), DoublePDSTest.f("g"), DoublePDSTest.node(8, "l")) // Due to the summary, we should be able to read f again.

    // addNormal(node(6,"e"), node(7,"h"));
    calleeToCallerMapping(DoublePDSTest.node(3, "u"), DoublePDSTest.node(4, "a"))
    calleeToCallerMapping(DoublePDSTest.node(3, "u"), DoublePDSTest.node(6, "a"))
    solve(DoublePDSTest.node(0, "c"))
    System.out.println(solver.getReachedStates)
    solver.debugOutput
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "h")))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(8, "l")))
  }

  @Test def simpleFieldPushAndPopAndContext(): Unit = {
    addFieldPush(DoublePDSTest.node(0, "c"), DoublePDSTest.f("g"), DoublePDSTest.node(1, "a"))
    addCallFlow(DoublePDSTest.node(1, "a"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(4))
    addFieldPush(DoublePDSTest.node(2, "u"), DoublePDSTest.f("f"), DoublePDSTest.node(3, "u"))
    addReturnFlow(DoublePDSTest.node(3, "u"), `var`("u"))
    addFieldPop(DoublePDSTest.node(4, "a"), DoublePDSTest.f("f"), DoublePDSTest.node(5, "e"))
    addFieldPop(DoublePDSTest.node(5, "e"), DoublePDSTest.f("g"), DoublePDSTest.node(6, "f")) // Should be possible
    calleeToCallerMapping(DoublePDSTest.node(3, "u"), DoublePDSTest.node(4, "a"))
    solve(DoublePDSTest.node(0, "c"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(6, "f")))
  }

  @Test def positiveNoFieldsSummaryTest(): Unit = {
    addNormal(DoublePDSTest.node(0, "c"), DoublePDSTest.node(1, "a"))
    addCallFlow(DoublePDSTest.node(1, "a"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(4))
    addNormal(DoublePDSTest.node(2, "u"), DoublePDSTest.node(3, "u"))
    addReturnFlow(DoublePDSTest.node(3, "u"), `var`("u"))
    addNormal(DoublePDSTest.node(4, "a"), DoublePDSTest.node(5, "e"))
    addCallFlow(DoublePDSTest.node(5, "e"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(6))
    addNormal(DoublePDSTest.node(6, "e"), DoublePDSTest.node(7, "h"))
    calleeToCallerMapping(DoublePDSTest.node(3, "u"), DoublePDSTest.node(4, "a"))
    calleeToCallerMapping(DoublePDSTest.node(3, "u"), DoublePDSTest.node(6, "e"))
    solve(DoublePDSTest.node(0, "c"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(7, "h")))
  }

  @Test def positiveSummaryFlowTest(): Unit = {
    addCallFlow(DoublePDSTest.node(1, "a"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(4))
    addReturnFlow(DoublePDSTest.node(2, "u"), `var`("e"))
    addCallFlow(DoublePDSTest.node(4, "e"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(6))
    calleeToCallerMapping(DoublePDSTest.node(2, "u"), DoublePDSTest.node(4, "e"))
    calleeToCallerMapping(DoublePDSTest.node(2, "u"), DoublePDSTest.node(6, "e"))
    solve(DoublePDSTest.node(1, "a"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(6, "e")))
  }

  @Test def recursion(): Unit = {
    addCallFlow(DoublePDSTest.node(1, "a"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(4))
    addNormal(DoublePDSTest.node(2, "u"), DoublePDSTest.node(3, "c"))
    addFieldPush(DoublePDSTest.node(3, "c"), DoublePDSTest.f("h"), DoublePDSTest.node(4, "h"))
    addCallFlow(DoublePDSTest.node(4, "h"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(5))
    addNormal(DoublePDSTest.node(4, "h"), DoublePDSTest.node(5, "h"))
    addFieldPop(DoublePDSTest.node(5, "h"), DoublePDSTest.f("h"), DoublePDSTest.node(6, "g"))
    addFieldPop(DoublePDSTest.node(6, "g"), DoublePDSTest.f("h"), DoublePDSTest.node(7, "g"))
    addReturnFlow(DoublePDSTest.node(7, "g"), `var`("g"))
    calleeToCallerMapping(DoublePDSTest.node(7, "g"), DoublePDSTest.node(4, "a"))
    solve(DoublePDSTest.node(1, "a"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(4, "a")))
  }

  @Test def recursion2(): Unit = {
    addCallFlow(DoublePDSTest.node(1, "a"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(4))
    addNormal(DoublePDSTest.node(2, "u"), DoublePDSTest.node(3, "c"))
    addFieldPush(DoublePDSTest.node(3, "c"), DoublePDSTest.f("h"), DoublePDSTest.node(4, "h"))
    addCallFlow(DoublePDSTest.node(4, "h"), DoublePDSTest.node(2, "u"), DoublePDSTest.returnSite(5))
    addNormal(DoublePDSTest.node(4, "h"), DoublePDSTest.node(5, "h"))
    addFieldPop(DoublePDSTest.node(5, "h"), DoublePDSTest.f("h"), DoublePDSTest.node(6, "g"))
    addFieldPop(DoublePDSTest.node(6, "g"), DoublePDSTest.f("h"), DoublePDSTest.node(7, "g"))
    addReturnFlow(DoublePDSTest.node(7, "g"), `var`("g"))
    calleeToCallerMapping(DoublePDSTest.node(7, "g"), DoublePDSTest.node(4, "a"))
    solve(DoublePDSTest.node(1, "a"))
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(4, "a")))
  }

  @Test def negativeTestFieldPushAndPop(): Unit = {
    addFieldPush(DoublePDSTest.node(1, "u"), DoublePDSTest.f("h"), DoublePDSTest.node(2, "v"))
    addFieldPop(DoublePDSTest.node(2, "v"), DoublePDSTest.f("f"), DoublePDSTest.node(3, "w"))
    solve(DoublePDSTest.node(1, "u"))
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(3, "w")))
  }

  @Test def negativeTestCallSitePushAndPop(): Unit = {
    addCallFlow(DoublePDSTest.node(1, "u"), DoublePDSTest.node(2, "v"), DoublePDSTest.returnSite(4))
    addReturnFlow(DoublePDSTest.node(2, "v"), `var`("w"))
    addNormal(DoublePDSTest.node(3, "w"), DoublePDSTest.node(4, "w"))
    solve(DoublePDSTest.node(1, "u"))
    System.out.println(solver.getReachedStates)
    assertFalse(solver.getReachedStates.contains(DoublePDSTest.node(3, "w")))
  }

  @Test def positiveTestCallSitePushAndPop(): Unit = {
    addCallFlow(DoublePDSTest.node(1, "u"), DoublePDSTest.node(4, "v"), DoublePDSTest.returnSite(2))
    addReturnFlow(DoublePDSTest.node(4, "v"), `var`("v"))
    addNormal(DoublePDSTest.node(2, "w"), DoublePDSTest.node(3, "w"))
    calleeToCallerMapping(DoublePDSTest.node(4, "v"), DoublePDSTest.node(2, "w"))
    solve(DoublePDSTest.node(1, "u"))
    //   verify(solver,times(1)).applyCallSummary(any(),any(),any(),any(),any());
    assertTrue(solver.getReachedStates.contains(DoublePDSTest.node(3, "w")))
  }

  private def `var`(v: Nothing) = new DoublePDSTest.Variable(v)
}