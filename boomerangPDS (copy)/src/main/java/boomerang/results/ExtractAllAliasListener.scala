package boomerang.results

import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Field
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.util.AccessPath
import com.google.common.collect.Lists
import com.google.common.collect.Sets
import java.util
import sync.pds.solver.SyncPDSUpdateListener
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.Empty
import wpds.interfaces.WPAStateListener
import wpds.interfaces.WPAUpdateListener

class ExtractAllAliasListener[W <: Weight](private var fwSolver: Nothing, private val results: Nothing, private val stmt: Nothing) extends Nothing {
  @Override def onReachableNodeAdded(reachableNode: Nothing): Unit = {
    if (reachableNode.stmt.equals(stmt)) {
      val base = reachableNode.fact
      import scala.collection.JavaConversions._
      for (allocNode <- fwSolver.getFieldAutomaton.getInitialStates) {
        fwSolver.getFieldAutomaton.registerListener(new Nothing() {
          @Override def onWeightAdded(t: Nothing, w: W, aut: Nothing): Unit = {
            if (t.getStart.fact.stmt.equals(stmt) && !t.getStart.isInstanceOf[Nothing] && t.getStart.fact.fact.equals(base)) {
              if (t.getLabel.equals(Field.empty)) if (t.getTarget.equals(allocNode)) results.add(new Nothing(base))
              val fields = Lists.newArrayList
              if (!t.getLabel.isInstanceOf[Nothing]) fields.add(t)
              fwSolver.getFieldAutomaton.registerListener(new ExtractAllAliasListener[W]#ExtractAccessPathStateListener(t.getTarget, allocNode, base, fields, results))
            }
          }
        })
      }
    }
  }

  private[results] class ExtractAccessPathStateListener(state: Nothing, private var allocNode: Nothing, private var base: Nothing, private var fields: Nothing, private var results: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getLabel.equals(Field.epsilon)) return
      var copiedFields = if (fields.isInstanceOf[Nothing]) Sets.newHashSet(fields)
      else Lists.newArrayList(fields)
      if (!t.getLabel.equals(Field.empty)) {
        if (copiedFields.contains(t)) copiedFields = Sets.newHashSet(fields)
        if (!t.getLabel.isInstanceOf[Nothing]) copiedFields.add(t)
      }
      if (t.getTarget.equals(allocNode)) results.add(new Nothing(base, convert(copiedFields)))
      weightedPAutomaton.registerListener(new ExtractAllAliasListener[W]#ExtractAccessPathStateListener(t.getTarget, allocNode, base, copiedFields, results))
    }

    private def convert(fields: Nothing) = {
      var res: Nothing = null
      if (fields.isInstanceOf[Nothing]) res = Lists.newArrayList
      else res = Sets.newHashSet
      import scala.collection.JavaConversions._
      for (f <- fields) {
        res.add(f.getLabel)
      }
      res
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (allocNode == null) 0
      else allocNode.hashCode)
      result = prime * result + (if (base == null) 0
      else base.hashCode)
      // result = prime * result + ((fields == null) ? 0 : fields.hashCode());
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[ExtractAllAliasListener[W]#ExtractAccessPathStateListener]
      if (!(getOuterType == other.getOuterType)) return false
      if (allocNode == null) if (other.allocNode != null) return false
      else if (!allocNode.equals(other.allocNode)) return false
      if (base == null) if (other.base != null) return false
      else if (!base.equals(other.base)) return false
      // if (fields == null) {
      // if (other.fields != null)
      // return false;
      // } else if (!fields.equals(other.fields))
      // return false;
      true
    }

    private def getOuterType = thisExtractAllAliasListener
  }

  @Override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (fwSolver == null) 0
    else fwSolver.hashCode)
    result = prime * result + (if (stmt == null) 0
    else stmt.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[ExtractAllAliasListener[_ <: Nothing]]
    if (fwSolver == null) if (other.fwSolver != null) return false
    else if (!fwSolver.equals(other.fwSolver)) return false
    if (stmt == null) if (other.stmt != null) return false
    else if (!stmt.equals(other.stmt)) return false
    true
  }
}