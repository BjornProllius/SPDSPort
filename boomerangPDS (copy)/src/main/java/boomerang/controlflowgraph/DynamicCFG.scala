package boomerang.controlflowgraph

import boomerang.scene.Statement
import com.google.common.collect.HashMultimap
import com.google.common.collect.Lists
import com.google.common.collect.Multimap
import java.util

class DynamicCFG extends Nothing {
  private[controlflowgraph] val statementToPredecessorListener = HashMultimap.create
  private[controlflowgraph] val statementToSuccessorListener = HashMultimap.create
  private[controlflowgraph] val succToPred = HashMultimap.create
  private[controlflowgraph] val predToSucc = HashMultimap.create

  @Override def addPredsOfListener(l: Nothing): Unit = {
    if (statementToPredecessorListener.put(l.getCurr, l)) {
      val preds = Lists.newArrayList(succToPred.get(l.getCurr))
      import scala.collection.JavaConversions._
      for (pred <- preds) {
        l.getPredecessor(pred)
      }
    }
  }

  @Override def addSuccsOfListener(l: Nothing): Unit = {
    if (statementToSuccessorListener.put(l.getCurr, l)) {
      val succs = Lists.newArrayList(predToSucc.get(l.getCurr))
      import scala.collection.JavaConversions._
      for (succ <- succs) {
        l.getSuccessor(succ)
      }
    }
  }

  def step(curr: Nothing, succ: Nothing): Unit = {
    predToSucc.put(curr, succ)
    succToPred.put(succ, curr)
    val lsnr = Lists.newArrayList(statementToSuccessorListener.get(curr))
    import scala.collection.JavaConversions._
    for (l <- lsnr) {
      l.getSuccessor(succ)
    }
    val lisnr = Lists.newArrayList(statementToPredecessorListener.get(succ))
    import scala.collection.JavaConversions._
    for (l <- lisnr) {
      l.getPredecessor(curr)
    }
  }

  @Override def unregisterAllListeners(): Unit = {
    statementToPredecessorListener.clear
    statementToSuccessorListener.clear
  }
}