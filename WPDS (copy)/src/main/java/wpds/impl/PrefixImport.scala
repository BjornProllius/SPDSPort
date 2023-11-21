package wpds.impl

import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.interfaces.WPAStateListener

class PrefixImport[N <: Location, D <: State, W <: Weight](private val baseAutomaton: Nothing, private val flowAutomaton: Nothing, t: Nothing) {
  baseAutomaton.registerListener(new PrefixImport[N, D, W]#IntersectionListener(t.getStart, t.getStart, t.getLabel, new PrefixImport[N, D, W]#IntersectionCallback() {
    @Override override def trigger(baseT: Nothing, flowT: Nothing): Unit = {
      // 3.
      baseAutomaton.registerListener(new PrefixImport[N, D, W]#Import(t.getTarget, flowT.getTarget))
      baseAutomaton.registerListener(new PrefixImport[N, D, W]#IntersectionListenerNoLabel(t.getTarget, flowT.getTarget, new PrefixImport[N, D, W]#IntersectionCallback() {
        @Override override def trigger(baseT: Nothing, flowT: Nothing): Unit = {
          // 3.
          baseAutomaton.registerListener(new PrefixImport[N, D, W]#Import(baseT.getTarget, flowT.getTarget))
        }
      }))
    }
  }))

  abstract private class IntersectionCallback {
    private[impl] def trigger(baseT: Nothing, flowT: Nothing): Unit
  }

  private class Import(state: D, private var flowTarget: D) extends Nothing(state) {
    @Override def onOutTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      flowAutomaton.addTransition(new Nothing(t.getStart, t.getLabel, flowTarget))
      baseAutomaton.registerListener(new PrefixImport[N, D, W]#Import(t.getStart, t.getStart))
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (flowTarget == null) 0
      else flowTarget.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PrefixImport[N, D, W]#Import]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (flowTarget == null) if (other.flowTarget != null) return false
      else if (!flowTarget.equals(other.flowTarget)) return false
      true
    }

    private def getOuterType = thisPrefixImport
  }

  private class IntersectionListener(baseState: D, private var flowState: D, private var label: N, private var callback: PrefixImport[N, D, W]#IntersectionCallback) extends Nothing(baseState) {
    @Override def onOutTransitionAdded(baseT: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (!baseT.getLabel.equals(label)) return
      flowAutomaton.registerListener(new PrefixImport[N, D, W]#HasOutTransWithSameLabel(flowState, baseT, callback))
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (flowState == null) 0
      else flowState.hashCode)
      result = prime * result + (if (label == null) 0
      else label.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PrefixImport[N, D, W]#IntersectionListener]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (flowState == null) if (other.flowState != null) return false
      else if (!flowState.equals(other.flowState)) return false
      if (label == null) if (other.label != null) return false
      else if (!label.equals(other.label)) return false
      true
    }

    private def getOuterType = thisPrefixImport
  }

  final protected class HasOutTransWithSameLabel private(state: D, private val baseT: Nothing, private val callback: PrefixImport[N, D, W]#IntersectionCallback) extends Nothing(state) {
    @Override def onOutTransitionAdded(flowT: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      if (flowT.getLabel.equals(baseT.getLabel)) callback.trigger(baseT, flowT)
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (baseT == null) 0
      else baseT.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PrefixImport[N, D, W]#HasOutTransWithSameLabel]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (baseT == null) if (other.baseT != null) return false
      else if (!baseT.equals(other.baseT)) return false
      true
    }

    private def getOuterType = thisPrefixImport
  }

  private class IntersectionListenerNoLabel(baseState: D, private var flowState: D, private var callback: PrefixImport[N, D, W]#IntersectionCallback) extends Nothing(baseState) {
    @Override def onOutTransitionAdded(baseT: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
      flowAutomaton.registerListener(new PrefixImport[N, D, W]#HasOutTransWithSameLabel(flowState, baseT, callback))
    }

    @Override def onInTransitionAdded(t: Nothing, w: W, weightedPAutomaton: Nothing): Unit = {
    }

    @Override def hashCode: Int = {
      val prime = 31
      var result = super.hashCode
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (flowState == null) 0
      else flowState.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (!super.equals(obj)) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PrefixImport[N, D, W]#IntersectionListenerNoLabel]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (flowState == null) if (other.flowState != null) return false
      else if (!flowState.equals(other.flowState)) return false
      true
    }

    private def getOuterType = thisPrefixImport
  }
}