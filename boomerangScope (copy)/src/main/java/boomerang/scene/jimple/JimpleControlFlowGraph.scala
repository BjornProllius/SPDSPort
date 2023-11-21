package boomerang.scene.jimple

import boomerang.scene.ControlFlowGraph
import boomerang.scene.Statement
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import java.util
import soot.Unit
import soot.UnitPatchingChain
import soot.jimple.IdentityStmt
import soot.jimple.Stmt
import soot.toolkits.graph.BriefUnitGraph
import soot.toolkits.graph.UnitGraph

class JimpleControlFlowGraph(private var method: Nothing) extends Nothing {
  this.graph = new Nothing(method.getDelegate.getActiveBody)
  private var graph: Nothing = null
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
    val heads = graph.getHeads
    import scala.collection.JavaConversions._
    for (u <- heads) {
      // We add a nop statement to the body and ignore IdentityStmt ($stack14 := @caughtexception)
      if (u.isInstanceOf[Nothing]) continue //todo: continue is not supported
      val stmt = JimpleStatement.create(u.asInstanceOf[Nothing], method)
      startPointCache.add(stmt)
    }
    val tails = graph.getTails
    import scala.collection.JavaConversions._
    for (u <- tails) {
      val stmt = JimpleStatement.create(u.asInstanceOf[Nothing], method)
      endPointCache.add(stmt)
    }
    val units = method.getDelegate.getActiveBody.getUnits
    import scala.collection.JavaConversions._
    for (u <- units) {
      val first = JimpleStatement.create(u.asInstanceOf[Nothing], method)
      statements.add(first)
      import scala.collection.JavaConversions._
      for (succ <- graph.getSuccsOf(u)) {
        val succStmt = JimpleStatement.create(succ.asInstanceOf[Nothing], method)
        succsOfCache.put(first, succStmt)
      }
      import scala.collection.JavaConversions._
      for (pred <- graph.getPredsOf(u)) {
        val predStmt = JimpleStatement.create(pred.asInstanceOf[Nothing], method)
        predsOfCache.put(first, predStmt)
      }
    }
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
}