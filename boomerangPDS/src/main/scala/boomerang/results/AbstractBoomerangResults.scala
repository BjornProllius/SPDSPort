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

class AbstractBoomerangResults[W <: Weight] (solverMap: DefaultValueMap[ForwardQuery, ForwardBoomerangSolver[W]]) {
  protected val queryToSolvers: DefaultValueMap[ForwardQuery, ForwardBoomerangSolver[W]] = solverMap
  protected val LOGGER: Logger = LoggerFactory.getLogger(classOf[AbstractBoomerangResults[_]])

  protected def constructContextGraph(forwardQuery: ForwardQuery, targetFact: Node[Edge, Val]): Context = {
    val context = new Context(targetFact, forwardQuery)
    val forwardSolver = queryToSolvers.get(forwardQuery)
    computeUnmatchedOpeningContext(context, forwardSolver, targetFact)
    computeUnmatchedClosingContext(context, forwardSolver)
    context
  }

  def computeUnmatchedClosingContext(context: Context, forwardSolver: AbstractBoomerangSolver[W]): Unit = {
    for (t <- forwardSolver.getCallAutomaton.getTransitions) {
      if (t.getTarget.fact.isUnbalanced) {
        val v = t.getTarget
        forwardSolver.getCallAutomaton.registerListener(new ClosingCallStackExtracter(v, v, context, forwardSolver))
      }
    }
  }

  def computeUnmatchedOpeningContext(context: Context, forwardSolver: AbstractBoomerangSolver[W], node: Node[Edge, Val]): Unit = {
    val initialState = new SingleNode(node.fact)
    forwardSolver.getCallAutomaton.registerListener(new OpeningCallStackExtracter(initialState, initialState, context, forwardSolver))
  }

  def asStatementValWeightTable(query: ForwardQuery): Table[Edge, Val, W] = {
    queryToSolvers.getOrCreate(query).asStatementValWeightTable()
  }

  private final class OpeningCallStackExtracter[W <: Weight](state: INode[Val], source: INode[Val], context: Context, solver: AbstractBoomerangSolver[W]) extends WPAStateListener[Edge, INode[Val], W](state) {
    private var source: INode[Val] = source
    private var context: Context = context
    private var solver: AbstractBoomerangSolver[W] = solver

    override def onOutTransitionAdded(t: Transition[Edge, INode[Val]], w: W, weightedPAutomaton: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {
      if (weightedPAutomaton.getInitialStates.contains(t.getTarget)) {
        return
      }

      // TODO Doesn't work anymore!
      if (t.getLabel.getMethod != null) {
        if (t.getStart.isInstanceOf[GeneratedState]) {
          context
            .getOpeningContext()
            .addTransition(new Transition(source, t.getLabel, t.getTarget))
        } else {
          weightedPAutomaton.registerListener(
            new OpeningCallStackExtracter(t.getTarget, source, context, solver))
          return
        }
      }
      weightedPAutomaton.registerListener(
        new OpeningCallStackExtracter(t.getTarget, t.getTarget, context, solver))
    }

    override def onInTransitionAdded(t: Transition[Edge, INode[Val]], w: W, weightedPAutomaton: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {}

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (context == null) 0 else context.hashCode())
      result = prime * result + (if (solver == null) 0 else solver.hashCode())
      result = prime * result + (if (source == null) 0 else source.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case that: OpeningCallStackExtracter[_] =>
          (that canEqual this) &&
            super.equals(that) &&
            context == that.context &&
            solver == that.solver &&
            source == that.source
        case _ => false
      }
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[OpeningCallStackExtracter[_]]
  }

  private final class ClosingCallStackExtracter[W <: Weight](state: INode[Val], source: INode[Val], context: Context, solver: AbstractBoomerangSolver[W]) extends WPAStateListener[Edge, INode[Val], W](state) {
    private var source: INode[Val] = source
    private var context: Context = context
    private var solver: AbstractBoomerangSolver[W] = solver

    override def onOutTransitionAdded(t: Transition[Edge, INode[Val]], w: W, weightedPAutomaton: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {}

    override def onInTransitionAdded(t: Transition[Edge, INode[Val]], w: W, weightedPAutomaton: WeightedPAutomaton[Edge, INode[Val], W]): Unit = {
      if (weightedPAutomaton.isUnbalancedState(t.getStart)) {
        if (!t.getStart.fact.isStatic) {
          context.getClosingContext.addTransition(t)
        }
      }
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = super.hashCode()
      result = prime * result + (if (context == null) 0 else context.hashCode())
      result = prime * result + (if (solver == null) 0 else solver.hashCode())
      result = prime * result + (if (source == null) 0 else source.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case that: ClosingCallStackExtracter[_] =>
          (that canEqual this) &&
            super.equals(that) &&
            context == that.context &&
            solver == that.solver &&
            source == that.source
        case _ => false
      }
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[ClosingCallStackExtracter[_]]
  }

  class Context(node: Node[Edge, Val], forwardQuery: ForwardQuery) {
    private val openingContext: PAutomaton[Edge, INode[Val]] = new PAutomaton[Edge, INode[Val]] {
      override def createState(d: INode[Val], loc: Edge): INode[Val] = throw new RuntimeException("Not implemented")

      override def isGeneratedState(d: INode[Val]): Boolean = throw new RuntimeException("Not implemented")

      override def epsilon(): Edge = new Edge(Statement.epsilon(), Statement.epsilon())
    }

    private val closingContext: PAutomaton[Edge, INode[Val]] = new PAutomaton[Edge, INode[Val]] {
      override def createState(d: INode[Val], loc: Edge): INode[Val] = throw new RuntimeException("Not implemented")

      override def isGeneratedState(d: INode[Val]): Boolean = throw new RuntimeException("Not implemented")

      override def epsilon(): Edge = new Edge(Statement.epsilon(), Statement.epsilon())
    }

    override def hashCode(): Int = {
      val prime = 31
      var result = 1
      result = prime * result + (if (closingContext == null) 0 else closingContext.hashCode())
      result = prime * result + (if (node == null) 0 else node.hashCode())
      result = prime * result + (if (openingContext == null) 0 else openingContext.hashCode())
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case that: Context =>
          (that canEqual this) &&
            node == that.node
        case _ => false
      }
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Context]

    def getOpeningContext(): PAutomaton[Edge, INode[Val]] = openingContext

    def getClosingContext(): PAutomaton[Edge, INode[Val]] = closingContext
  }
}