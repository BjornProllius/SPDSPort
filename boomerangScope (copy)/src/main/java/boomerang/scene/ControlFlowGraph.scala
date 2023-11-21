package boomerang.scene

import java.util
import wpds.interfaces.Location

object ControlFlowGraph {
  class Edge(start: Nothing, target: Nothing) extends Nothing(start, target) with Nothing {
    if (!start.equals(Statement.epsilon) && !start.getMethod.equals(target.getMethod)) throw new Nothing("Illegal Control Flow Graph Edge constructed")

    @Override def toString: Nothing = getStart + " -> " + getTarget

    def getStart: Nothing = getX

    def getTarget: Nothing = getY

    def getMethod: Nothing = getStart.getMethod

    @Override def accepts(other: Nothing): Boolean = this.equals(other)
  }
}

trait ControlFlowGraph {
  def getStartPoints: Nothing

  def getEndPoints: Nothing

  def getSuccsOf(curr: Nothing): Nothing

  def getPredsOf(curr: Nothing): Nothing

  def getStatements: Nothing
}