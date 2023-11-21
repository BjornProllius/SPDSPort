package boomerang.results

import boomerang.ForwardQuery
import boomerang.scene.ControlFlowGraph.Edge
import boomerang.scene.Statement
import boomerang.scene.Val
import boomerang.solver.AbstractBoomerangSolver
import boomerang.solver.ForwardBoomerangSolver
import boomerang.util.DefaultValueMap
import com.google.common.collect.Table
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import sync.pds.solver.nodes.GeneratedState
import sync.pds.solver.nodes.INode
import sync.pds.solver.nodes.Node
import sync.pds.solver.nodes.SingleNode
import wpds.impl.PAutomaton
import wpds.impl.Transition
import wpds.impl.Weight
import wpds.impl.WeightedPAutomaton
import wpds.interfaces.WPAStateListener

object AbstractBoomerangResults {
  private class OpeningCallStackExtracter[W <: Weight](state: Nothing, private var source: Nothing, private var context: AbstractBoomerangResults.Context, private var solver: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (weightedPAutomaton.getInitialStates.contains(t.getTarget)) return
      // TODO Doesn't work anymore!
      if (t.getLabel.getMethod != null) if (t.getStart.isInstanceOf[Nothing]) context.getOpeningContext.addTransition(new Nothing(source, t.getLabel, t.getTarget))
      else {
        weightedPAutomaton.registerListener(new AbstractBoomerangResults.OpeningCallStackExtracter[W](t.getTarget, source, context, solver))
        return
      }
      weightedPAutomaton.registerListener(new AbstractBoomerangResults.OpeningCallStackExtracter[W](t.getTarget, t.getTarget, context, solver))
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (context == null) 0
      else context.hashCode)
      result = prime * result + (if (solver == null) 0
      else solver.hashCode)
      result = prime * result + (if (source == null) 0
      else source.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[AbstractBoomerangResults.OpeningCallStackExtracter[_ <: Nothing]]
      if (context == null) if (other.context != null) return false
      else if (!(context == other.context)) return false
      if (solver == null) if (other.solver != null) return false
      else if (!solver.equals(other.solver)) return false
      if (source == null) if (other.source != null) return false
      else if (!source.equals(other.source)) return false
      true
    }
  }

  private class ClosingCallStackExtracter[W <: Weight](state: Nothing, private var source: Nothing, private var context: AbstractBoomerangResults.Context, private var solver: Nothing) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (weightedPAutomaton.isUnbalancedState(t.getStart)) if (!t.getStart.fact.isStatic) context.getClosingContext.addTransition(t)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (context == null) 0
      else context.hashCode)
      result = prime * result + (if (solver == null) 0
      else solver.hashCode)
      result = prime * result + (if (source == null) 0
      else source.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[AbstractBoomerangResults.ClosingCallStackExtracter[_ <: Nothing]]
      if (context == null) if (other.context != null) return false
      else if (!(context == other.context)) return false
      if (solver == null) if (other.solver != null) return false
      else if (!solver.equals(other.solver)) return false
      if (source == null) if (other.source != null) return false
      else if (!source.equals(other.source)) return false
      true
    }
  }

  class Context(private[results] val node: Nothing, forwardQuery: Nothing) {
    this.openingContext = new Nothing() {
      @Override def createState(d: Nothing, loc: Nothing) = throw new Nothing("Not implemented")

      @Override def isGeneratedState(d: Nothing) = throw new Nothing("Not implemented")

      @Override def epsilon = new Nothing(Statement.epsilon, Statement.epsilon)
    }
    this.closingContext = new Nothing() {
      @Override def createState(d: Nothing, loc: Nothing) = throw new Nothing("Not implemented")

      @Override def isGeneratedState(d: Nothing) = throw new Nothing("Not implemented")

      @Override def epsilon = new Nothing(Statement.epsilon, Statement.epsilon)
    }
    final private var openingContext: Nothing = null
    final private var closingContext: Nothing = null

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (closingContext == null) 0
      else closingContext.hashCode)
      result = prime * result + (if (node == null) 0
      else node.hashCode)
      result = prime * result + (if (openingContext == null) 0
      else openingContext.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[AbstractBoomerangResults.Context]
      if (node == null) if (other.node != null) return false
      else if (!node.equals(other.node)) return false
      true
    }

    def getOpeningContext: Nothing = openingContext

    def getClosingContext: Nothing = closingContext
  }
}

class AbstractBoomerangResults[W <: Weight](protected val queryToSolvers: Nothing) {
  final protected val LOGGER = LoggerFactory.getLogger(classOf[AbstractBoomerangResults[_ <: Nothing]])

  protected def constructContextGraph(forwardQuery: Nothing, targetFact: Nothing): AbstractBoomerangResults.Context = {
    val context = new AbstractBoomerangResults.Context(targetFact, forwardQuery)
    val forwardSolver = queryToSolvers.get(forwardQuery)
    computeUnmatchedOpeningContext(context, forwardSolver, targetFact)
    computeUnmatchedClosingContext(context, forwardSolver)
    context
  }

  def computeUnmatchedClosingContext(context: AbstractBoomerangResults.Context, forwardSolver: Nothing): Unit = {
    import scala.collection.JavaConversions._
    for (t <- forwardSolver.getCallAutomaton.getTransitions) {
      if (t.getTarget.fact.isUnbalanced) {
        val v = t.getTarget
        forwardSolver.getCallAutomaton.registerListener(new AbstractBoomerangResults.ClosingCallStackExtracter[W](v, v, context, forwardSolver))
      }
    }
  }

  def computeUnmatchedOpeningContext(context: AbstractBoomerangResults.Context, forwardSolver: Nothing, node: Nothing): Unit = {
    val initialState = new Nothing(node.fact)
    forwardSolver.getCallAutomaton.registerListener(new AbstractBoomerangResults.OpeningCallStackExtracter[W](initialState, initialState, context, forwardSolver))
  }

  def asStatementValWeightTable(query: Nothing): Nothing = queryToSolvers.getOrCreate(query).asStatementValWeightTable
}