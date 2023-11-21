package boomerang.callgraph

import boomerang.scene.CallGraph.Edge
import boomerang.scene.Method
import boomerang.scene.Statement
import java.util

class BackwardsObservableICFG(protected val delegate: Nothing) extends Nothing {
  @Override def getStartPointsOf(m: Nothing): Nothing = this.delegate.getEndPointsOf(m)

  @Override def isExitStmt(stmt: Nothing): Boolean = this.delegate.isStartPoint(stmt)

  @Override def isStartPoint(stmt: Nothing): Boolean = this.delegate.isExitStmt(stmt)

  @Override def getEndPointsOf(m: Nothing): Nothing = this.delegate.getStartPointsOf(m)

  @Override def isCallStmt(stmt: Nothing): Boolean = this.delegate.isCallStmt(stmt)

  @Override def addCalleeListener(listener: Nothing): Unit = {
    delegate.addCalleeListener(listener)
  }

  @Override def addCallerListener(listener: Nothing): Unit = {
    delegate.addCallerListener(listener)
  }

  @Override def getNumberOfEdgesTakenFromPrecomputedGraph: Int = delegate.getNumberOfEdgesTakenFromPrecomputedGraph

  @Override def resetCallGraph(): Unit = {
    delegate.resetCallGraph
  }

  @Override def computeFallback(): Unit = {
    delegate.computeFallback
  }

  @Override def addEdges(e: Nothing): Unit = {
    this.delegate.addEdges(e)
  }
}