package wpds.impl

import com.google.common.collect.Sets
import java.util
import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.interfaces.WPAStateListener

abstract class StackListener[N <: Location, D <: State, W <: Weight](
                                                                      /** */
                                                                      private val aut: Nothing, state: D, private var source: N) extends Nothing(state) {
  private val notifiedStacks = Sets.newHashSet

  @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    if (t.getLabel.equals(aut.epsilon)) return
    if (this.aut.getInitialStates.contains(t.getTarget)) {
      if (t.getLabel.equals(source)) anyContext(source)
      return
    }
    if (this.aut.isGeneratedState(t.getTarget)) aut.registerListener(new StackListener[N, D, W]#SubStackListener(t.getTarget, this))
  }

  @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
  }

  def stackElement(callSite: N): Unit

  def anyContext(end: N): Unit

  @Override def hashCode: Int = {
    val prime = 31
    var result = super.hashCode
    result = prime * result + (if (source == null) 0
    else source.hashCode)
    result
  }

  @Override def equals(obj: Nothing): Boolean = {
    if (this eq obj) return true
    if (!super.equals(obj)) return false
    if (getClass ne obj.getClass) return false
    val other = obj.asInstanceOf[StackListener[N, D, W]]
    if (source == null) if (other.source != null) return false
    else if (!source.equals(other.source)) return false
    true
  }

  private class SubStackListener(state: D, private var parent: StackListener[_ <: Nothing, _ <: Nothing, _ <: Nothing]) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (t.getLabel.equals(aut.epsilon)) return
      stackElement(t.getLabel)
      if (aut.isGeneratedState(t.getTarget) && !t.getTarget.equals(t.getStart)) aut.registerListener(new StackListener[N, D, W]#SubStackListener(t.getTarget, parent))
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    def stackElement(parent: N): Unit = {
      if (notifiedStacks.add(parent)) thisStackListener.stackElement(parent)
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + (if (parent == null) 0
      else parent.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[StackListener[N, D, W]#SubStackListener]
      if (parent == null) if (other.parent != null) return false
      else if (!(parent == other.parent)) return false
      true
    }
  }
}