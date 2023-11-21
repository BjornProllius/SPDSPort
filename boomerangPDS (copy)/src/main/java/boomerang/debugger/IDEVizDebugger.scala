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
package boomerang.debugger

import boomerang.BackwardQuery
import boomerang.ForwardQuery
import boomerang.Query
import boomerang.callgraph.CalleeListener
import boomerang.callgraph.CallerListener
import boomerang.callgraph.ObservableICFG
import boomerang.controlflowgraph.ObservableControlFlowGraph
import boomerang.controlflowgraph.SuccessorListener
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.ForwardBoomerangSolver
import boomerang.util.RegExAccessPath
import com.google.common.base.Stopwatch
import com.google.common.collect.HashBasedTable
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import com.google.common.collect.Table
import com.google.common.collect.Table.Cell
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.util
import java.util.Map.Entry
import java.util.Objects
import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.NormalRule
import wpds.impl.Rule
import wpds.impl.Weight

object IDEVizDebugger {
  private val ONLY_CFG = false
  private val logger = LoggerFactory.getLogger(classOf[IDEVizDebugger[_ <: Nothing]])
}

class IDEVizDebugger[W <: Weight](private var ideVizFile: Nothing) extends Nothing {
  private var icfg: Nothing = null
  private val rules = HashBasedTable.create
  private val objectToInteger = new Nothing
  private val charSize = 0
  private var cfg: Nothing = null

  private def callRules(q: Nothing, allRules: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (e <- allRules) {
      val stmt = e.getL1
      if (stmt.getMethod == null) continue //todo: continue is not supported
      val transInMethod = getOrCreateRuleSet(q, stmt.getMethod)
      transInMethod.add(e)
    }
  }

  private def getOrCreateRuleSet(q: Nothing, method: Nothing): Nothing = {
    val map = rules.get(q, method)
    if (map != null) return map
    rules.put(q, method, Sets.newHashSet)
    rules.get(q, method)
  }

  @Override def done(icfg: Nothing, confg: Nothing, visitedMethods: Nothing, solvers: Nothing): Unit = {
    this.icfg = icfg
    this.cfg = confg
    IDEVizDebugger.logger.warn("Starting to compute visualization, this requires a large amount of memory, please ensure the VM has enough memory.")
    val watch = Stopwatch.createStarted
    val eventualData = new Nothing
    if (!IDEVizDebugger.ONLY_CFG) {
      import scala.collection.JavaConversions._
      for (q <- solvers.keySet) {
        callRules(q, solvers.get(q).getCallPDS.getAllRules)
      }
    }
    import scala.collection.JavaConversions._
    for (e <- Lists.newArrayList(solvers.entrySet)) {
      IDEVizDebugger.logger.debug("Computing results for {}", e.getKey)
      val query = e.getKey
      val queryJSON = new IDEVizDebugger[W]#JSONQuery(query)
      val data = new Nothing
      import scala.collection.JavaConversions._
      for (m <- Lists.newArrayList(visitedMethods)) {
        val results = e.getValue.getResults(m)
        if (results.isEmpty) continue //todo: continue is not supported
        val labelYOffset = if (IDEVizDebugger.ONLY_CFG) 0
        else computeLabelYOffset(results.columnKeySet)
        val jsonMethod = new IDEVizDebugger[W]#JSONMethod(m)
        IDEVizDebugger.logger.debug("Creating control-flow graph for {}", m)
        val cfg = createControlFlowGraph(m, labelYOffset)
        jsonMethod.put("cfg", cfg)
        if (!IDEVizDebugger.ONLY_CFG) {
          val rulesInMethod = getOrCreateRuleSet(query, m)
          IDEVizDebugger.logger.debug("Creating data-flow graph for {}", m)
          val dfg = createDataFlowGraph(query, results, rulesInMethod, cfg, m, labelYOffset)
          jsonMethod.put("dfg", dfg)
        }
        data.add(jsonMethod)
      }
      queryJSON.put("methods", data)
      eventualData.add(queryJSON)
    }
    IDEVizDebugger.logger.info("Computing visualization took: {}", watch.elapsed)
    try {
      val file = new Nothing(ideVizFile)
      try {
        IDEVizDebugger.logger.info("Writing visualization to file {}", ideVizFile.getAbsolutePath)
        file.write(eventualData.toJSONString)
        IDEVizDebugger.logger.info("Visualization available in file {}", ideVizFile.getAbsolutePath)
      } catch {
        case e: Nothing =>
          e.printStackTrace
          IDEVizDebugger.logger.info("Exception in writing to visualization file {}", ideVizFile.getAbsolutePath)
      } finally if (file != null) file.close()
    }
  }

  private def computeLabelYOffset(facts: Nothing) = {
    var labelYOffset = 0
    import scala.collection.JavaConversions._
    for (g <- facts) {
      labelYOffset = Math.max(labelYOffset, charSize * g.toString.length)
    }
    labelYOffset
  }

  private def createDataFlowGraph(q: Nothing, table: Nothing, rulesInMethod: Nothing, cfg: IDEVizDebugger[W]#JSONControlFlowGraph, m: Nothing, labelYOffset: Int) = {
    val factsList = new Nothing
    val dataFlowGraph = new IDEVizDebugger[W]#DataFlowGraph
    val facts = table.columnKeySet
    val data = new Nothing
    val offset = 0
    val charSize = 8
    import scala.collection.JavaConversions._
    for (u <- facts) {
      val nodeObj = new Nothing
      val pos = new Nothing
      factsList.add(u)
      pos.put("x", factsList.size * 30 + offset * charSize)
      pos.put("y", labelYOffset)
      nodeObj.put("position", pos)
      val label = new Nothing
      label.put("label", u.toString)
      label.put("factId", id(u))
      nodeObj.put("classes", "fact label method" + id(m))
      nodeObj.put("data", label)
      data.add(nodeObj)
    }
    val esgNodes = HashMultimap.create
    // System.out.println("Number of nodes:\t" + esg.getNodes().size());
    import scala.collection.JavaConversions._
    for (trans <- table.cellSet) {
      val stmt = trans.getRowKey
      val `val` = trans.getColumnKey
      if (!trans.getRowKey.getMethod.equals(`val`.getVal.m)) continue //todo: continue is not supported
      val nodeObj = new Nothing
      val pos = new Nothing
      pos.put("x", (factsList.indexOf(`val`) + 1) * 30 /* + offset * charSize */)
      pos.put("y", cfg.stmtsList.indexOf(stmt) * 30 + (if (q.isInstanceOf[Nothing]) 30
      else 0) /* + labelYOffset */)
      nodeObj.put("position", pos)
      val classes = "esgNode method" + id(m) + " "
      val additionalData = new Nothing
      additionalData.put("id", "q" + id(q) + "n" + id(new Nothing(stmt, `val`)))
      additionalData.put("stmtId", id(stmt))
      additionalData.put("factId", id(`val`))
      if (trans.getValue != null) additionalData.put("ideValue", trans.getValue.toString)
      nodeObj.put("classes", classes)
      nodeObj.put("group", "nodes")
      nodeObj.put("data", additionalData)
      data.add(nodeObj)
      esgNodes.put(new Nothing(stmt, `val`.getVal), `val`)
    }
    import scala.collection.JavaConversions._
    for (rule <- rulesInMethod) {
      if (!rule.isInstanceOf[Nothing]) continue //todo: continue is not supported
      val nodeObj = new Nothing
      val dataEntry = new Nothing
      dataEntry.put("id", "e" + id(rule))
      val start = getStartNode(rule)
      val target = getTargetNode(rule)
      import scala.collection.JavaConversions._
      for (startField <- esgNodes.get(start)) {
        import scala.collection.JavaConversions._
        for (targetField <- esgNodes.get(target)) {
          dataEntry.put("source", "q" + id(q) + "n" + id(new Nothing(start.stmt, startField)))
          dataEntry.put("target", "q" + id(q) + "n" + id(new Nothing(target.stmt, targetField)))
          dataEntry.put("directed", "true")
          dataEntry.put("direction", if (q.isInstanceOf[Nothing]) "Backward"
          else "Forward")
          nodeObj.put("data", dataEntry)
          nodeObj.put("classes", "esgEdge  method" + id(m))
          nodeObj.put("group", "edges")
          data.add(nodeObj)
        }
      }
    }
    dataFlowGraph.put("dataFlowNode", data)
    dataFlowGraph
  }

  private def getTargetNode(rule: Nothing) = new Nothing(rule.getL2, rule.getS2.fact)

  private def getStartNode(rule: Nothing) = new Nothing(rule.getL1, rule.getS1.fact)

  private def createControlFlowGraph(m: Nothing, labelYOffset: Int) = {
    val cfg = new IDEVizDebugger[W]#JSONControlFlowGraph
    var index = 0
    var offset = 0
    val data = new Nothing
    import scala.collection.JavaConversions._
    for (u <- m.getStatements) {
      if (u.getMethod == null) continue //todo: continue is not supported
      val nodeObj = new Nothing
      val pos = new Nothing
      cfg.stmtsList.add(u)
      pos.put("x", 10)
      pos.put("y", cfg.stmtsList.size * 30 + labelYOffset)
      nodeObj.put("position", pos)
      val label = new Nothing
      label.put("label", u.toString)
      label.put("shortLabel", u.toString)
      if (icfg.isCallStmt(u)) {
        label.put("callSite", icfg.isCallStmt(u))
        val callees = new Nothing
        icfg.addCalleeListener(new IDEVizDebugger[W]#JsonCalleeListener(u, callees))
        label.put("callees", callees)
      }
      if (icfg.isExitStmt(u)) {
        label.put("returnSite", icfg.isExitStmt(u))
        val callees = new Nothing
        val callers = new Nothing
        icfg.addCallerListener(new IDEVizDebugger[W]#JsonCallerListener(u, callers))
        import scala.collection.JavaConversions._
        for (caller <- callers) {
          callees.add(new IDEVizDebugger[W]#JSONMethod(caller))
        }
        label.put("callers", callees)
      }
      label.put("stmtId", id(u))
      label.put("id", "stmt" + id(u))
      label.put("stmtIndex", index)
      index += 1
      nodeObj.put("data", label)
      nodeObj.put("classes", "stmt label " + (if (icfg.isExitStmt(u)) " returnSite "
      else " ") + (if (icfg.isCallStmt(u)) " callSite "
      else " ") + " method" + id(m))
      data.add(nodeObj)
      offset = Math.max(offset, u.toString.length)
      this.cfg.addSuccsOfListener(new Nothing(u) {
        @Override def getSuccessor(succ: Nothing): Unit = {
          val cfgEdgeObj = new Nothing
          val dataEntry = new Nothing
          dataEntry.put("source", "stmt" + id(u))
          dataEntry.put("target", "stmt" + id(succ))
          dataEntry.put("directed", "true")
          cfgEdgeObj.put("data", dataEntry)
          cfgEdgeObj.put("classes", "cfgEdge label method" + id(m))
          data.add(cfgEdgeObj)
        }
      })
    }
    cfg.put("controlFlowNode", data)
    cfg
  }

  private class JsonCalleeListener private[debugger](private[debugger] var u: Nothing, private[debugger] var callees: Nothing) extends Nothing {
    @Override def getObservedCaller: Nothing = u

    @Override def onCalleeAdded(unit: Nothing, sootMethod: Nothing): Unit = {
      if (sootMethod != null && sootMethod.toString != null) callees.add(new IDEVizDebugger[W]#JSONMethod(sootMethod))
    }

    @Override def equals(o: Nothing): Boolean = {
      if (this eq o) return true
      if (o == null || (getClass ne o.getClass)) return false
      val that = o.asInstanceOf[IDEVizDebugger[W]#JsonCalleeListener]
      Objects.equals(u, that.u) && Objects.equals(callees, that.callees)
    }

    @Override def hashCode: Int = Objects.hash(u, callees)

    @Override def onNoCalleeFound(): Unit = {
    }
  }

  private class JsonCallerListener private[debugger](private[debugger] var u: Nothing, private[debugger] var callers: Nothing) extends Nothing {
    @Override def getObservedCallee: Nothing = u.getMethod

    @Override def onCallerAdded(unit: Nothing, sootMethod: Nothing): Unit = {
      callers.add(unit.getMethod)
    }

    @Override def equals(o: Nothing): Boolean = {
      if (this eq o) return true
      if (o == null || (getClass ne o.getClass)) return false
      val that = o.asInstanceOf[IDEVizDebugger[W]#JsonCallerListener]
      Objects.equals(u, that.u) && Objects.equals(callers, that.callers)
    }

    @Override def hashCode: Int = Objects.hash(u, callers)
  }

  private class JSONMethod private[debugger](m: Nothing) extends Nothing {
    this.put("methodName", method.toString)
    this.put("id", id(method))
  }

  private class JSONQuery private[debugger](m: Nothing) extends Nothing {
    this.put("query", prettyPrintQuery(method))
    this.put("id", id(method))

    private def prettyPrintQuery(m: Nothing) = (if (m.isInstanceOf[Nothing]) "B "
    else "F ") + m.asNode.fact + " @ " + m.asNode.stmt.getMethod
  }

  private class JSONControlFlowGraph extends Nothing {
    var stmtsList: Nothing = Lists.newLinkedList
  }

  private class DataFlowGraph extends Nothing {}

  def id(u: Nothing): Nothing = {
    if (objectToInteger.get(u) != null) return objectToInteger.get(u)
    val size = objectToInteger.size + 1
    objectToInteger.put(u, size)
    size
  }
}