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
import java.util.HashMap
import java.util.HashSet
import java.util.LinkedList
import java.util.List
import java.util.Map
import java.util.Map.Entry
import java.util.Objects
import java.util.Set
import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.NormalRule
import wpds.impl.Rule
import wpds.impl.Weight

class IDEVizDebugger[W <: Weight](ideVizFile: File) extends Debugger[W] {

  private val ONLY_CFG = false
  private val logger = LoggerFactory.getLogger(classOf[IDEVizDebugger[_]])
  private var icfg: ObservableICFG[Statement, Method] = _
  private val rules = HashBasedTable.create[Query, Method, Set[Rule[Edge, INode[Val], W]]]()
  private val objectToInteger = new HashMap[Object, Integer]()
  private var charSize: Int = _
  private var cfg: ObservableControlFlowGraph = _

  private def callRules(q: Query, allRules: Set[Rule[Edge, INode[Val], W]]): Unit = {
    for (e <- allRules) {
      val stmt = e.getL1()
      if (stmt.getMethod() == null) return
      val transInMethod = getOrCreateRuleSet(q, stmt.getMethod())
      transInMethod.add(e)
    }
  }

  private def getOrCreateRuleSet(q: Query, method: Method): Set[Rule[Edge, INode[Val], W]] = {
    var map = rules.get(q, method)
    if (map != null) return map
    rules.put(q, method, Sets.newHashSet())
    rules.get(q, method)
  }

  override def done(
      icfg: ObservableICFG[Statement, Method],
      confg: ObservableControlFlowGraph,
      visitedMethods: Set[Method],
      solvers: Map[ForwardQuery, ForwardBoomerangSolver[W]]): Unit = {

    this.icfg = icfg
    this.cfg = confg
    logger.warn(
      "Starting to compute visualization, this requires a large amount of memory, please ensure the VM has enough memory.")
    val watch = Stopwatch.createStarted()
    val eventualData = new JSONArray()
    if (!ONLY_CFG) {
      for (q <- solvers.keySet) {
        callRules(q, solvers(q).getCallPDS().getAllRules())
      }
    }
    for (e <- solvers) {
      logger.debug("Computing results for {}", e._1)
      val query = e._1
      val queryJSON = new JSONQuery(query)
      val data = new JSONArray()
      for (m <- visitedMethods) {
        val results = e._2.getResults(m)
        if (results.isEmpty) continue
        val labelYOffset = if (ONLY_CFG) 0 else computeLabelYOffset(results.columnKeySet())
        val jsonMethod = new JSONMethod(m)
        logger.debug("Creating control-flow graph for {}", m)
        val cfg = createControlFlowGraph(m, labelYOffset)

        jsonMethod.put("cfg", cfg)
        if (!ONLY_CFG) {
          val rulesInMethod = getOrCreateRuleSet(query, m)
          logger.debug("Creating data-flow graph for {}", m)
          val dfg = createDataFlowGraph(query, results, rulesInMethod, cfg, m, labelYOffset)
          jsonMethod.put("dfg", dfg)
        }
        data.add(jsonMethod)
      }
      queryJSON.put("methods", data)
      eventualData.add(queryJSON)
    }
    logger.info("Computing visualization took: {}", watch.elapsed())
    try {
      val file = new FileWriter(ideVizFile)
      logger.info("Writing visualization to file {}", ideVizFile.getAbsolutePath())
      file.write(eventualData.toJSONString())
      logger.info("Visualization available in file {}", ideVizFile.getAbsolutePath())
      file.close()
    } catch {
      case e: IOException =>
        e.printStackTrace()
        logger.info("Exception in writing to visualization file {}", ideVizFile.getAbsolutePath())
    }
  }
  
  private def computeLabelYOffset(facts: Set[RegExAccessPath]): Int = {
    var labelYOffset = 0
    for (g <- facts) {
      labelYOffset = Math.max(labelYOffset, charSize * g.toString.length)
    }
    labelYOffset
  }

  private def createDataFlowGraph(
      q: Query,
      table: Table[Edge, RegExAccessPath, W],
      rulesInMethod: Set[Rule[Edge, INode[Val], W]],
      cfg: JSONControlFlowGraph,
      m: Method,
      labelYOffset: Int): DataFlowGraph = {

    val factsList = new LinkedList[RegExAccessPath]()
    val dataFlowGraph = new DataFlowGraph()
    val facts = table.columnKeySet()
    val data = new JSONArray()

    var offset = 0
    val charSize = 8
    for (u <- facts) {
      val nodeObj = new JSONObject()
      val pos = new JSONObject()
      factsList.add(u)
      pos.put("x", factsList.size * 30 + offset * charSize)
      pos.put("y", labelYOffset)
      nodeObj.put("position", pos)
      val label = new JSONObject()
      label.put("label", u.toString)
      label.put("factId", id(u))
      nodeObj.put("classes", "fact label method" + id(m))
      nodeObj.put("data", label)
      data.add(nodeObj)
    }

    val esgNodes = HashMultimap.create[Node[Edge, Val], RegExAccessPath]()
    // System.out.println("Number of nodes:\t" + esg.getNodes().size())
  
    for (trans <- table.cellSet()) {
      val stmt = trans.getRowKey()
      val `val` = trans.getColumnKey()
      if (!trans.getRowKey().getMethod().equals(`val`.getVal().m())) continue
      val nodeObj = new JSONObject()
      val pos = new JSONObject()
      pos.put("x", (factsList.indexOf(`val`) + 1) * 30 /* + offset * charSize */)
      pos.put(
          "y",
          (cfg.stmtsList.indexOf(stmt)) * 30
              + (if (q.isInstanceOf[BackwardQuery]) 30 else 0) /* + labelYOffset */)

      nodeObj.put("position", pos)
      val classes = "esgNode method" + id(m) + " "

      val additionalData = new JSONObject()
      additionalData.put("id", "q" + id(q) + "n" + id(new Node(stmt, `val`)))
      additionalData.put("stmtId", id(stmt))
      additionalData.put("factId", id(`val`))
      if (trans.getValue() != null) additionalData.put("ideValue", trans.getValue().toString())
      nodeObj.put("classes", classes)
      nodeObj.put("group", "nodes")
      nodeObj.put("data", additionalData)

      data.add(nodeObj)

      esgNodes.put(new Node(stmt, `val`.getVal()), `val`)
    }

    for (rule <- rulesInMethod) {
      if (!rule.isInstanceOf[NormalRule]) {
        continue
      }
      val nodeObj = new JSONObject()
      val dataEntry = new JSONObject()
      dataEntry.put("id", "e" + id(rule))
      val start = getStartNode(rule)
      val target = getTargetNode(rule)
      for (startField <- esgNodes.get(start)) {
        for (targetField <- esgNodes.get(target)) {
          dataEntry.put("source", "q" + id(q) + "n" + id(new Node(start.stmt(), startField)))
          dataEntry.put("target", "q" + id(q) + "n" + id(new Node(target.stmt(), targetField)))
          dataEntry.put("directed", "true")
          dataEntry.put("direction", if (q.isInstanceOf[BackwardQuery]) "Backward" else "Forward")
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
  
  private def getTargetNode(rule: Rule[Edge, INode[Val], W]): Node[Edge, Val] = {
    new Node(rule.getL2, rule.getS2.fact)
  }

  private def getStartNode(rule: Rule[Edge, INode[Val], W]): Node[Edge, Val] = {
    new Node(rule.getL1, rule.getS1.fact)
  }

  private def createControlFlowGraph(m: Method, labelYOffset: Int): JSONControlFlowGraph = {
    val cfg = new JSONControlFlowGraph()
    var index = 0
    var offset = 0
    val data = new JSONArray()
    for (u <- m.getStatements) {
      if (u.getMethod == null) {
        continue
      }
      val nodeObj = new JSONObject()
      val pos = new JSONObject()
      cfg.stmtsList.add(u)
      pos.put("x", 10)
      pos.put("y", cfg.stmtsList.size * 30 + labelYOffset)
      nodeObj.put("position", pos)
      val label = new JSONObject()
      label.put("label", u.toString)
      label.put("shortLabel", u.toString)
      if (icfg.isCallStmt(u)) {
        label.put("callSite", icfg.isCallStmt(u))
        val callees = new JSONArray()
        icfg.addCalleeListener(new JsonCalleeListener(u, callees))
        label.put("callees", callees)
      }
      if (icfg.isExitStmt(u)) {
        label.put("returnSite", icfg.isExitStmt(u))
        val callees = new JSONArray()
        val callers = new HashSet[Method]()
        icfg.addCallerListener(new JsonCallerListener(u, callers))
        for (caller <- callers) callees.add(new JSONMethod(caller))
        label.put("callers", callees)
      }
      label.put("stmtId", id(u))
      label.put("id", "stmt" + id(u))

      label.put("stmtIndex", index)
      index += 1

      nodeObj.put("data", label)
      nodeObj.put(
        "classes",
        "stmt label "
          + (if (icfg.isExitStmt(u)) " returnSite " else " ")
          + (if (icfg.isCallStmt(u)) " callSite " else " ")
          + " method"
          + id(m))
      data.add(nodeObj)
      offset = Math.max(offset, u.toString.length)

      this.cfg.addSuccsOfListener(
        new SuccessorListener(u) {

          override def getSuccessor(succ: Statement): Unit = {
            val cfgEdgeObj = new JSONObject()
            val dataEntry = new JSONObject()
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

  private class JsonCalleeListener(u: Statement, callees: JSONArray) extends CalleeListener[Statement, Method] {

      override def getObservedCaller(): Statement = u

      override def onCalleeAdded(unit: Statement, sootMethod: Method): Unit = {
        if (sootMethod != null && sootMethod.toString != null) {
          callees.add(new JSONMethod(sootMethod))
        }
      }

      override def equals(o: Any): Boolean = o match {
        case that: JsonCalleeListener => u == that.u && callees == that.callees
        case _ => false
      }

      override def hashCode(): Int = Objects.hash(u, callees)

      override def onNoCalleeFound(): Unit = {}
  }

  private class JsonCallerListener(u: Statement, callers: Set[Method]) extends CallerListener[Statement, Method] {

      override def getObservedCallee(): Method = u.getMethod()

      override def onCallerAdded(unit: Statement, sootMethod: Method): Unit = {
        callers.add(unit.getMethod())
      }

      override def equals(o: Any): Boolean = o match {
        case that: JsonCallerListener => u == that.u && callers == that.callers
        case _ => false
      }

      override def hashCode(): Int = Objects.hash(u, callers)
    }

  private class JSONMethod(m: Method) extends JSONObject {
      this.put("methodName", m.toString())
      this.put("id", id(m))
  }

  private class JSONQuery(m: Query) extends JSONObject {
      this.put("query", prettyPrintQuery(m))
      this.put("id", id(m))

      private def prettyPrintQuery(m: Query): String = {
        (if (m.isInstanceOf[BackwardQuery]) "B " else "F ") +
          m.asNode().fact() +
          " @ " +
          m.asNode().stmt().getMethod()
      }
  }

  private class JSONControlFlowGraph extends JSONObject {
      val stmtsList: List[Statement] = Lists.newLinkedList()
  }

  private class DataFlowGraph extends JSONObject {}

  def id(u: Object): Integer = {
      if (objectToInteger.get(u) != null) objectToInteger.get(u)
      else {
        val size = objectToInteger.size() + 1
        objectToInteger.put(u, size)
        size
      }
  }


}