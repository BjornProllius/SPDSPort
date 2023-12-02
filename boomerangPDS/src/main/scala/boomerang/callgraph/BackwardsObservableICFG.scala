package boomerang.callgraph

import boomerang.scene.{CallGraph, Method, Statement}
import java.util.Collection

class BackwardsObservableICFG(delegate: ObservableICFG[Statement, Method]) extends ObservableICFG[Statement, Method] {

  override def getStartPointsOf(m: Method): Collection[Statement] = delegate.getEndPointsOf(m)

  override def isExitStmt(stmt: Statement): Boolean = delegate.isStartPoint(stmt)

  override def isStartPoint(stmt: Statement): Boolean = delegate.isExitStmt(stmt)

  override def getEndPointsOf(m: Method): Collection[Statement] = delegate.getStartPointsOf(m)

  override def isCallStmt(stmt: Statement): Boolean = delegate.isCallStmt(stmt)

  override def addCalleeListener(listener: CalleeListener): Unit = delegate.addCalleeListener(listener)

  override def addCallerListener(listener: CallerListener): Unit = delegate.addCallerListener(listener)

  override def getNumberOfEdgesTakenFromPrecomputedGraph(): Int = delegate.getNumberOfEdgesTakenFromPrecomputedGraph()

  override def resetCallGraph(): Unit = delegate.resetCallGraph()

  override def computeFallback(): Unit = delegate.computeFallback()

  override def addEdges(e: CallGraph.Edge): Unit = delegate.addEdges(e)
}