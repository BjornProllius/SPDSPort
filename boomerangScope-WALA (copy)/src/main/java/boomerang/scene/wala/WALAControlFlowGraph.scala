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
import com.google.common.base.Joiner
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Maps
import com.google.common.collect.Multimap
import com.google.common.collect.Sets
import com.ibm.wala.classLoader.IClass
import com.ibm.wala.classLoader.IField
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.ssa.ISSABasicBlock
import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.ssa.SSACFG.BasicBlock
import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.types.FieldReference
import com.ibm.wala.types.TypeReference
import java.util

object WALAControlFlowGraph {
  private class Graph[N] {
    private[wala] val outEdges = HashMultimap.create
    private[wala] val inEdges = HashMultimap.create
    private[wala] val entries = Sets.newHashSet

    def removeNode(bb: N): Unit = {
      val out = outEdges.get(bb)
      val in = inEdges.get(bb)
      import scala.collection.JavaConversions._
      for (o <- out) {
        import scala.collection.JavaConversions._
        for (i <- in) {
          addEdge(i, o)
        }
      }
      if (entries.contains(bb)) {
        entries.remove(bb)
        import scala.collection.JavaConversions._
        for (newEntries <- out) {
          entries.add(newEntries)
        }
      }
      import scala.collection.JavaConversions._
      for (tgt <- out) {
        inEdges.remove(tgt, bb)
      }
      import scala.collection.JavaConversions._
      for (src <- in) {
        outEdges.remove(src, bb)
      }
      outEdges.removeAll(bb)
      inEdges.removeAll(bb)
    }

    def addEntry(entry: N): Unit = {
      entries.add(entry)
    }

    private def addEdge(src: N, tgt: N): Unit = {
      outEdges.put(src, tgt)
      inEdges.put(tgt, src)
    }
  }
}

class WALAControlFlowGraph(private var method: Nothing, private var cha: Nothing) extends Nothing {
  this.cfg = method.getIR.getControlFlowGraph
  buildCache()
  private var cfg: Nothing = null
  private val basicBlockToFirstStmt = Maps.newHashMap
  private var cacheBuild = false
  private val startPointCache = Lists.newArrayList
  private val endPointCache = Lists.newArrayList
  private val succsOfCache = HashMultimap.create
  private val predsOfCache = HashMultimap.create
  private val statements = Lists.newArrayList

  def getStartPoints: Nothing = {
    buildCache()
    startPointCache
  }

  private def buildCache(): Unit = {
    if (cacheBuild) return
    cacheBuild = true
    val emptyBasicBlocks = Sets.newHashSet
    val basicBlockToLastStmt = Maps.newHashMap
    val bbIt = cfg.iterator
    // Convert each basic block.
    while (bbIt.hasNext) {
      val next = bbIt.next
      val n = next.asInstanceOf[Nothing]
      val allInstructions = n.getAllInstructions
      val allStatements = convert(allInstructions)
      var last: Nothing = null
      var first: Nothing = null
      import scala.collection.JavaConversions._
      for (curr <- allStatements) {
        statements.add(curr)
        if (last == null) {
          last = curr
          first = curr
        }
        else {
          succsOfCache.put(last, curr)
          predsOfCache.put(curr, last)
          last = curr
        }
      }
      if (last == null) emptyBasicBlocks.add(next)
      else {
        if (first == null) throw new Nothing("Unexpected behaviour!")
        basicBlockToFirstStmt.put(next, first)
        basicBlockToLastStmt.put(next, last)
      }
    }
    val bbGraph = buildDirectedGraph
    import scala.collection.JavaConversions._
    for (eBB <- emptyBasicBlocks) {
      bbGraph.removeNode(eBB)
    }
    val visited = Sets.newHashSet
    val worklist = Lists.newLinkedList
    worklist.addAll(bbGraph.entries)
    while (!worklist.isEmpty) {
      val curr = worklist.poll
      if (emptyBasicBlocks.contains(curr)) throw new Nothing("Unexpected behaviour!")
      else if (!basicBlockToFirstStmt.containsKey(curr)) throw new Nothing("Unexpected behaviour!")
      else if (!basicBlockToLastStmt.containsKey(curr)) throw new Nothing("Unexpected behaviour!")
      if (!visited.add(curr)) continue //todo: continue is not supported
      val succNodes = bbGraph.outEdges.get(curr)
      import scala.collection.JavaConversions._
      for (next <- succNodes) {
        if (emptyBasicBlocks.contains(next)) throw new Nothing("Unexpected behaviour!")
        val firstOfNextBB = basicBlockToFirstStmt.get(next)
        val lastOfPrevBB = basicBlockToLastStmt.get(curr)
        succsOfCache.put(lastOfPrevBB, firstOfNextBB)
        predsOfCache.put(firstOfNextBB, lastOfPrevBB)
        worklist.add(next)
      }
    }
    val entryStatement = new Nothing(("NOP", method)) {
      @Override def hashCode: Int = System.identityHashCode(this)

      @Override def equals(obj: Nothing): Boolean = this eq obj
    }
    val entries = Lists.newArrayList
    import scala.collection.JavaConversions._
    for (s <- statements) {
      if (predsOfCache.get(s).isEmpty) entries.add(s)
    }
    statements.add(0, entryStatement)
    val entryStatements = addUnitializedFields
    if (!entryStatements.isEmpty) {
      predsOfCache.put(entryStatements.getFirst, entryStatement)
      succsOfCache.put(entryStatement, entryStatements.getLast)
      import scala.collection.JavaConversions._
      for (s <- entries) {
        predsOfCache.put(s, entryStatements.getLast)
        succsOfCache.put(entryStatements.getLast, s)
      }
    }
    else {
      import scala.collection.JavaConversions._
      for (s <- entries) {
        predsOfCache.put(s, entryStatement)
        succsOfCache.put(entryStatement, s)
      }
    }
    import scala.collection.JavaConversions._
    for (s <- statements) {
      if (succsOfCache.get(s).isEmpty) endPointCache.add(s)
      if (predsOfCache.get(s).isEmpty) startPointCache.add(s)
    }
    import scala.collection.JavaConversions._
    for (s <- statements) {
      if (s.containsInvokeExpr) if (s.isInstanceOf[Nothing]) throw new Nothing("The statement must be split into CallSite and ReturnSiteStatement")
    }
    import scala.collection.JavaConversions._
    for (s <- succsOfCache.values) {
      if (s.containsInvokeExpr) if (s.isInstanceOf[Nothing]) throw new Nothing("The statement must be split into CallSite and ReturnSiteStatement")
    }
  }

  private def convert(allInstructions: Nothing) = {
    val res = Lists.newArrayList
    import scala.collection.JavaConversions._
    for (ins <- allInstructions) {
      res.addAll(convert(ins))
    }
    // TODO Auto-generated method stub
    res
  }

  private def convert(ins: Nothing) = {
    val res = Lists.newArrayList
    val curr = new Nothing(ins, method)
    // TODO for PhiNodes as well.
    if (curr.containsInvokeExpr) res.addAll(handleCallSite(curr))
    else if (curr.isFieldStore && curr.getRightOp.isNull) {
      res.add(new Nothing(curr.getRightOp, method))
      res.add(curr)
    }
    else res.add(curr)
    res
  }

  private def addUnitializedFields: Nothing = {
    val uninitializedStatements = Lists.newLinkedList
    if (!method.isConstructor) return uninitializedStatements
    val delegate = method.getDeclaringClass.getDelegate.asInstanceOf[Nothing]
    val c = cha.lookupClass(delegate)
    val allFields = c.getAllFields
    val allDeclaredFields = computeInitializedFields
    import scala.collection.JavaConversions._
    for (f <- allFields) {
      allDeclaredFields.add(f.getReference)
    }
    val definedFields = computeInitializedFields
    allDeclaredFields.removeAll(definedFields)
    import scala.collection.JavaConversions._
    for (ref <- allDeclaredFields) {
      val dummyVal = new Nothing(method.asInstanceOf[Nothing])
      uninitializedStatements.add(new Nothing(dummyVal, method))
      uninitializedStatements.add(new Nothing(new Nothing(ref), method.asInstanceOf[Nothing], method.getThisLocal, dummyVal))
    }
    addListOfStmts(uninitializedStatements)
    uninitializedStatements
  }

  private def computeInitializedFields = {
    val refs = Lists.newArrayList
    import scala.collection.JavaConversions._
    for (s <- statements) {
      if (s.isFieldStore) {
        val del = s.asInstanceOf[Nothing]
        val ssaInstruction = del.getSSAInstruction.asInstanceOf[Nothing]
        val declaredFieldType = ssaInstruction.getDeclaredField
        refs.add(declaredFieldType)
      }
    }
    refs
  }

  private def addListOfStmts(stmts: Nothing): Unit = {
    var last: Nothing = null
    import scala.collection.JavaConversions._
    for (curr <- stmts) {
      statements.add(curr)
      if (last == null) last = curr
      else {
        succsOfCache.put(last, curr)
        predsOfCache.put(curr, last)
        last = curr
      }
    }
  }

  private def handleCallSite(call: Nothing) = {
    val res = Lists.newLinkedList
    containsNullVariables(call, res)
    //      addListOfStmts(res);
    res
  }

  private def containsNullVariables(call: Nothing, res: Nothing): Unit = {
    val args = call.getInvokeExpr.getArgs
    import scala.collection.JavaConversions._
    for (a <- args) {
      if (a.isNull) res.add(new Nothing(a, call.getMethod))
    }
    res.add(call)
  }

  private def buildDirectedGraph = {
    val graph = new WALAControlFlowGraph.Graph[Nothing]
    val visited = Sets.newHashSet
    val worklist = Lists.newLinkedList
    worklist.add(cfg.entry)
    graph.addEntry(cfg.entry)
    while (!worklist.isEmpty) {
      val curr = worklist.poll
      if (!visited.add(curr)) continue //todo: continue is not supported
      val succNodes = cfg.getSuccNodes(curr)
      while (succNodes.hasNext) {
        val next = succNodes.next
        graph.addEdge(curr, next)
        worklist.add(next)
      }
    }
    graph
  }

  def getEndPoints: Nothing = {
    buildCache()
    endPointCache
  }

  def getSuccsOf(curr: Nothing): Nothing = {
    buildCache()
    succsOfCache.get(curr)
  }

  def getPredsOf(curr: Nothing): Nothing = {
    buildCache()
    predsOfCache.get(curr)
  }

  def getStatements: Nothing = {
    buildCache()
    statements
  }

  @Override def toString: Nothing = {
    var s = "============================="
    s += "Control Flow Graph for " + method
    s += "Entries:\n " + Joiner.on("\n").join(startPointCache)
    s += "\nExit:\n " + Joiner.on("\n").join(endPointCache)
    s += "\nSucc:\n "
    import scala.collection.JavaConversions._
    for (e <- succsOfCache.keySet) {
      s += " from " + e + " \n "
      import scala.collection.JavaConversions._
      for (v <- succsOfCache.get(e)) {
        s += "\t \t to : " + v + "\n"
      }
    }
    s += "============================="
    s
  }

  def getBranchTarget(targetIndex: Int): Nothing = {
    val blockForInstruction = cfg.getBlockForInstruction(targetIndex)
    basicBlockToFirstStmt.get(blockForInstruction)
  }
}