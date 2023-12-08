package boomerang.controlflowgraph

import boomerang.scene.Statement
import com.google.common.collect.{HashMultimap, Lists, Multimap}

import scala.collection.JavaConverters._

class DynamicCFG extends ObservableControlFlowGraph {

  private val statementToPredecessorListener: Multimap[Statement, PredecessorListener] = HashMultimap.create()
  private val statementToSuccessorListener: Multimap[Statement, SuccessorListener] = HashMultimap.create()

  private val succToPred: Multimap[Statement, Statement] = HashMultimap.create()
  private val predToSucc: Multimap[Statement, Statement] = HashMultimap.create()

  override def addPredsOfListener(l: PredecessorListener): Unit = {
    if (statementToPredecessorListener.put(l.getCurr, l)) {
      val preds: Collection[Statement] = Lists.newArrayList(succToPred.get(l.getCurr))
      for (pred <- preds.asScala) {
        l.getPredecessor(pred)
      }
    }
  }

  override def addSuccsOfListener(l: SuccessorListener): Unit = {
    if (statementToSuccessorListener.put(l.getCurr, l)) {
      val succs: Collection[Statement] = Lists.newArrayList(predToSucc.get(l.getCurr))
      for (succ <- succs.asScala) {
        l.getSuccessor(succ)
      }
    }
  }

  def step(curr: Statement, succ: Statement): Unit = {
    predToSucc.put(curr, succ)
    succToPred.put(succ, curr)
    val lsnr: Collection[SuccessorListener] = Lists.newArrayList(statementToSuccessorListener.get(curr))
    for (l <- lsnr.asScala) {
      l.getSuccessor(succ)
    }
    val lisnr: Collection[PredecessorListener] = Lists.newArrayList(statementToPredecessorListener.get(succ))
    for (l <- lisnr.asScala) {
      l.getPredecessor(curr)
    }
  }

  override def unregisterAllListeners(): Unit = {
    statementToPredecessorListener.clear()
    statementToSuccessorListener.clear()
  }
}