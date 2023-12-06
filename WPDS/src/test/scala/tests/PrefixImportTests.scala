package tests

import org.junit.Test
import org.junit.Assert._
import wpds.impl._

class PrefixImportTests {

    val autA: PAutomaton[StringLoc, StringState] = new PAutomaton[StringLoc, StringState] {
        override def createState(d: StringState, loc: StringLoc): StringState = new StringState(d + "_" + loc) {
            override def generated(): Boolean = true
        }

        override def isGeneratedState(d: StringState): Boolean = d.generated()

        override def epsilon(): StringLoc = new StringLoc("EPS")
    }

    val autB: PAutomaton[StringLoc, StringState] = new PAutomaton[StringLoc, StringState] {
        override def createState(d: StringState, loc: StringLoc): StringState = new StringState(d + "_" + loc) {
            override def generated(): Boolean = true
        }

        override def isGeneratedState(d: StringState): Boolean = d.generated()

        override def epsilon(): StringLoc = new StringLoc("EPS")
    }

    @Test
    def prefixCombine(): Unit = {
        autA.addTransition(t("A1", "f", "INITA"))
        autA.addTransition(t("A3", "g", "INITA"))
        autA.addTransition(t("A4", "h", "A3"))

        autB.addTransition(t("A1", "f", "B2"))
        autB.addTransition(t("B2", "i", "INITB"))

        new PrefixImport(autA, autB, t("A1", "f", "INITA"))
        assertTrue(autB.getTransitions.contains(t("A3", "g", "B2")))
        assertTrue(autB.getTransitions.contains(t("A4", "h", "A3")))
    }

    @Test
    def prefixCombineDouble(): Unit = {
        autA.addTransition(t("A1", "f", "A2"))
        autA.addTransition(t("A2", "i", "INITA"))
        autA.addTransition(t("A3", "g", "INITA"))
        autA.addTransition(t("A4", "k", "A3"))

        autB.addTransition(t("A1", "f", "B2"))
        autB.addTransition(t("B2", "i", "INITB"))

        new PrefixImport(autA, autB, t("A1", "f", "A2"))
        assertTrue(autB.getTransitions.contains(t("A3", "g", "INITB")))
        assertTrue(autB.getTransitions.contains(t("A4", "k", "A3")))
    }

    private def t(start: String, label: String, target: String): Transition[StringLoc, StringState] = 
        new Transition[StringLoc, StringState](new StringState(start), new StringLoc(label), new StringState(target))

    case class StringState(rep: String) extends State {
        def generated(): Boolean = false
        override def toString: String = rep
    }

    case class StringLoc(rep: String) extends Location {
        override def toString: String = rep
        override def accepts(other: Location): Boolean = this == other
    }

}