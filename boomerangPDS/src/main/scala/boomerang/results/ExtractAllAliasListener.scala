package boomerang.results

import boomerang.scene.{ControlFlowGraph, Field, Val}
import boomerang.solver.AbstractBoomerangSolver
import boomerang.util.AccessPath
import com.google.common.collect.{Lists, Sets}
import sync.pds.solver.{SyncPDSUpdateListener, nodes}
import wpds.impl.{Transition, Weight, WeightedPAutomaton}
import wpds.interfaces.{Empty, WPAStateListener, WPAUpdateListener}

import scala.collection.{Set, List}

class ExtractAllAliasListener[W <: Weight] extends SyncPDSUpdateListener[Edge, Val] {
  class ExtractAllAliasListener[W <: Weight](fwSolver: AbstractBoomerangSolver[W], results: Set[AccessPath], stmt: Edge) extends SyncPDSUpdateListener[Edge, Val] {
    override def onReachableNodeAdded(reachableNode: Node[Edge, Val]): Unit = {
      if (reachableNode.stmt().equals(stmt)) {
        val base = reachableNode.fact()
        for (allocNode <- fwSolver.getFieldAutomaton().getInitialStates()) {
          fwSolver.getFieldAutomaton().registerListener(
            new WPAUpdateListener[Field, INode[Node[Edge, Val]], W] {
              override def onWeightAdded(t: Transition[Field, INode[Node[Edge, Val]]], w: W, aut: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]): Unit = {
                if (t.getStart().fact().stmt().equals(stmt) && !t.getStart().isInstanceOf[GeneratedState] && t.getStart().fact().fact().equals(base)) {
                  if (t.getLabel().equals(Field.empty())) {
                    if (t.getTarget().equals(allocNode)) {
                      results.add(new AccessPath(base))
                    }
                  }
                  val fields = Lists.newArrayList[Transition[Field, INode[Node[Edge, Val]]]]()
                  if (!t.getLabel().isInstanceOf[Empty]) {
                    fields.add(t)
                  }
                  fwSolver.getFieldAutomaton().registerListener(
                    new ExtractAccessPathStateListener(t.getTarget(), allocNode, base, fields, results)
                  )
                }
              }
            }
          )
        }
      }
    }
  }

  class ExtractAccessPathStateListener extends WPAStateListener[Field, INode[Node[Edge, Val]], W]{

    class ExtractAccessPathStateListener(
        state: INode[Node[Edge, Val]],
        var allocNode: INode[Node[Edge, Val]],
        var base: Val,
        var fields: Collection[Transition[Field, INode[Node[Edge, Val]]]],
        var results: Set[AccessPath]
    ) extends WPAStateListener[Field, INode[Node[Edge, Val]], W](state) {

      override def onOutTransitionAdded(
          t: Transition[Field, INode[Node[Edge, Val]]],
          w: W,
          weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
      ): Unit = {
        if (t.getLabel().equals(Field.epsilon())) return
        var copiedFields =
          if (fields.isInstanceOf[Set[_]]) Sets.newHashSet(fields)
          else Lists.newArrayList(fields)
        if (!t.getLabel().equals(Field.empty())) {
          if (copiedFields.contains(t)) {
            copiedFields = Sets.newHashSet(fields)
          }
          if (!t.getLabel().isInstanceOf[Empty]) copiedFields.add(t)
        }
        if (t.getTarget().equals(allocNode)) {
          results.add(new AccessPath(base, convert(copiedFields)))
        }
        weightedPAutomaton.registerListener(
          new ExtractAccessPathStateListener(
            t.getTarget(), allocNode, base, copiedFields, results
          )
        )
      }

      private def convert(
          fields: Collection[Transition[Field, INode[Node[Edge, Val]]]]
      ): Collection[Field] = {
        val res =
          if (fields.isInstanceOf[List[_]]) Lists.newArrayList()
          else Sets.newHashSet()
        for (f <- fields) {
          res.add(f.getLabel())
        }
        res
      }

      override def onInTransitionAdded(
          t: Transition[Field, INode[Node[Edge, Val]]],
          w: W,
          weightedPAutomaton: WeightedPAutomaton[Field, INode[Node[Edge, Val]], W]
      ): Unit = {}

      override def hashCode(): Int = {
        val prime = 31
        var result = super.hashCode()
        result = prime * result + getOuterType().hashCode()
        result = prime * result + (if (allocNode == null) 0 else allocNode.hashCode())
        result = prime * result + (if (base == null) 0 else base.hashCode())
        result
      }
    

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ExtractAccessPathStateListener =>
          (this eq other) ||
          (super.equals(obj) &&
          (this.getClass == obj.getClass) &&
          (getOuterType() == other.getOuterType()) &&
          (allocNode == null && other.allocNode == null || allocNode != null && allocNode.equals(other.allocNode)) &&
          (base == null && other.base == null || base != null && base.equals(other.base)))
        case _ => false
      }
    }

    private def getOuterType(): ExtractAllAliasListener = this

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (fwSolver == null) 0 else fwSolver.hashCode())
      result = prime * result + (if (stmt == null) 0 else stmt.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: ExtractAllAliasListener =>
          (this eq other) ||
          (obj != null &&
          (this.getClass == obj.getClass) &&
          (fwSolver == null && other.fwSolver == null || fwSolver != null && fwSolver.equals(other.fwSolver)) &&
          (stmt == null && other.stmt == null || stmt != null && stmt.equals(other.stmt)))
        case _ => false
      }
    }
  }
}