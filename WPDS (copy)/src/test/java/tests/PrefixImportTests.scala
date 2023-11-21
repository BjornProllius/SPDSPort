package tests

import org.junit.Assert.assertTrue
import org.junit.Test
import wpds.impl.PAutomaton
import wpds.impl.PrefixImport
import wpds.impl.Transition
import wpds.interfaces.Location
import wpds.interfaces.State

class PrefixImportTests {
  private[tests] val autA = new Nothing() {
    @Override def createState(d: PrefixImportTests#StringState, loc: PrefixImportTests#StringLoc): PrefixImportTests#StringState = new PrefixImportTests#StringState(d + "_" + loc) {
      @Override override def generated: Boolean = return true
    }

    @Override def isGeneratedState(d: PrefixImportTests#StringState): Boolean = d.generated

    @Override def epsilon = new PrefixImportTests#StringLoc("EPS")
  }
  private[tests] val autB = new Nothing() {
    @Override def createState(d: PrefixImportTests#StringState, loc: PrefixImportTests#StringLoc): PrefixImportTests#StringState = new PrefixImportTests#StringState(d + "_" + loc) {
      @Override override def generated: Boolean = return true
    }

    @Override def isGeneratedState(d: PrefixImportTests#StringState): Boolean = d.generated

    @Override def epsilon = new PrefixImportTests#StringLoc("EPS")
  }

  @Test def prefixCombine(): Unit = {
    autA.addTransition(t("A1", "f", "INITA"))
    autA.addTransition(t("A3", "g", "INITA"))
    autA.addTransition(t("A4", "h", "A3"))
    autB.addTransition(t("A1", "f", "B2"))
    autB.addTransition(t("B2", "i", "INITB"))
    new Nothing(autA, autB, t("A1", "f", "INITA"))
    assertTrue(autB.getTransitions.contains(t("A3", "g", "B2")))
    assertTrue(autB.getTransitions.contains(t("A4", "h", "A3")))
  }

  @Test def prefixCombineDouble(): Unit = {
    autA.addTransition(t("A1", "f", "A2"))
    autA.addTransition(t("A2", "i", "INITA"))
    autA.addTransition(t("A3", "g", "INITA"))
    autA.addTransition(t("A4", "k", "A3"))
    autB.addTransition(t("A1", "f", "B2"))
    autB.addTransition(t("B2", "i", "INITB"))
    new Nothing(autA, autB, t("A1", "f", "A2"))
    assertTrue(autB.getTransitions.contains(t("A3", "g", "INITB")))
    assertTrue(autB.getTransitions.contains(t("A4", "k", "A3")))
  }

  private def t(start: Nothing, label: Nothing, target: Nothing) = new Nothing(new PrefixImportTests#StringState(start), new PrefixImportTests#StringLoc(label), new PrefixImportTests#StringState(target))

  private[tests] class StringState(private var rep: Nothing) extends Nothing {
    def generated = false

    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (rep == null) 0
      else rep.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PrefixImportTests#StringState]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (rep == null) if (other.rep != null) return false
      else if (!rep.equals(other.rep)) return false
      true
    }

    @Override def toString: Nothing = rep

    private def getOuterType = thisPrefixImportTests
  }

  private[tests] class StringLoc(private var rep: Nothing) extends Nothing {
    @Override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + getOuterType.hashCode
      result = prime * result + (if (rep == null) 0
      else rep.hashCode)
      result
    }

    @Override def equals(obj: Nothing): Boolean = {
      if (this eq obj) return true
      if (obj == null) return false
      if (getClass ne obj.getClass) return false
      val other = obj.asInstanceOf[PrefixImportTests#StringLoc]
      if (!getOuterType.equals(other.getOuterType)) return false
      if (rep == null) if (other.rep != null) return false
      else if (!rep.equals(other.rep)) return false
      true
    }

    @Override def toString: Nothing = rep

    private def getOuterType = thisPrefixImportTests

    @Override def accepts(other: Nothing): Boolean = this == other
  }
}