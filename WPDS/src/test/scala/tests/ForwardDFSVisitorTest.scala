package tests

import org.junit.{Before, Test}
import org.junit.Assert._
import wpds.impl._
import scala.collection.mutable.HashSet

class ForwardDFSVisitorTest {
    import TestHelper._

    val fa: PAutomaton[StackSymbol, Abstraction] = new PAutomaton[StackSymbol, Abstraction]() {
        override def createState(d: Abstraction, loc: StackSymbol): Abstraction = new Abstraction(d, loc)
        override def epsilon(): StackSymbol = s("EPS")
        override def isGeneratedState(d: Abstraction): Boolean = d.s != null
    }

    val reachables: HashSet[Transition[StackSymbol, Abstraction]] = HashSet()

    @Test
    def delayedAdd(): Unit = {
        fa.registerDFSListener(
            a(0),
            new ReachabilityListener[StackSymbol, Abstraction]() {
                override def reachable(t: Transition[StackSymbol, Abstraction]): Unit = {
                    reachables += t
                }
            }
        )
        fa.addTransition(t(0, "n1", 1))
        assertFalse(reachables.isEmpty)
        assertTrue(reachableMinusTrans().isEmpty)
        fa.addTransition(t(1, "n1", 2))
        assertTrue(reachableMinusTrans().isEmpty)
    }

    @Test
    def delayedAddListener(): Unit = {
        fa.addTransition(t(0, "n1", 1))
        fa.addTransition(t(1, "n1", 2))
        assertFalse(fa.getTransitions.isEmpty)
        fa.registerDFSListener(
            a(0),
            new ReachabilityListener[StackSymbol, Abstraction]() {
                override def reachable(t: Transition[StackSymbol, Abstraction]): Unit = {
                    reachables += t
                }
            }
        )
        assertFalse(reachables.isEmpty)
        assertTrue(reachableMinusTrans().isEmpty)

        fa.addTransition(t(4, "n1", 5))
        assertTrue(fa.getTransitions.size > reachables.size)

        fa.addTransition(t(2, "n1", 5))
        assertTrue(fa.getTransitions.size > reachables.size)
        assertFalse(reachableMinusTrans().isEmpty)

        fa.addTransition(t(2, "n1", 4))

        assertTrue(fa.getTransitions.size == reachables.size)
        assertTrue(reachableMinusTrans().isEmpty)

        fa.addTransition(t(3, "n1", 8))
        fa.addTransition(t(8, "n1", 9))
        fa.addTransition(t(3, "n1", 7))
        fa.addTransition(t(3, "n1", 6))
        fa.addTransition(t(6, "n1", 3))
        assertTrue(fa.getTransitions.size > reachables.size)
        assertFalse(reachableMinusTrans().isEmpty)

        fa.addTransition(t(1, "n1", 3))
        assertTrue(reachableMinusTrans().isEmpty)
    }

    private var pds: PushdownSystem[StackSymbol, Abstraction] = _

    @Before
    def init(): Unit = {
        pds = new PushdownSystem[StackSymbol, Abstraction]() {}
    }

    @Test
    def summaryReachabilityTest(): Unit = {
        pds.addRule(normal(1, "a", 1, "b"))
        pds.addRule(normal(1, "b", 1, "c"))
        pds.addRule(push(1, "c", 1, "d", "e"))
        pds.addRule(push(1, "d", 1, "h", "i"))
        pds.addRule(normal(1, "h", 2, "g"))
        pds.addRule(pop(2, "g", 1))
        pds.addRule(pop(1, "d", 1))
        pds.addRule(normal(1, "e", 1, "k"))
        val fa = accepts(1, "a")
        pds.poststar(fa)
        val faSummaries = accepts(1, "a")
        pds.poststar(
            faSummaries, new SummaryNestedWeightedPAutomatons[StackSymbol, Abstraction, NoWeight]())
        assertSetEquals(reachableFrom(fa, a(2)), reachableFrom(faSummaries, a(2)))
        assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
    }

    @Test
    def simpleSummaryReachabilityTest(): Unit = {
        pds.addRule(normal(1, "a", 2, "b"))
        pds.addRule(push(2, "b", 3, "d", "e"))
        pds.addRule(normal(3, "d", 3, "f"))
        pds.addRule(pop(3, "f", 2))
        pds.addRule(normal(2, "e", 3, "k"))
        val fa = accepts(1, "a")
        pds.poststar(fa)
        val faSummaries = accepts(1, "a")
        pds.poststar(faSummaries, new SummaryNestedWeightedPAutomatons[StackSymbol, Abstraction, NoWeight]())
        assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
    }

    @Test
    def reapplySummaryReachabilityTest(): Unit = {
        pds.addRule(normal(1, "a", 2, "b"))
        pds.addRule(push(2, "b", 3, "d", "e"))
        pds.addRule(normal(3, "d", 3, "f"))
        pds.addRule(normal(3, "f", 4, "j"))
        pds.addRule(pop(4, "j", 2))
        pds.addRule(normal(2, "e", 4, "k"))
        pds.addRule(push(4, "k", 3, "d", "i"))
        pds.addRule(normal(2, "i", 5, "m"))
        val fa = accepts(1, "a")
        pds.poststar(fa)
        val faSummaries = accepts(1, "a")
        pds.poststar(faSummaries, new SummaryNestedWeightedPAutomatons[StackSymbol, Abstraction, NoWeight]())
        assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
    }

    @Test
    def doublePushSummaryReachabilityTest(): Unit = {
        pds.addRule(normal(1, "a", 2, "b"))
        pds.addRule(push(2, "b", 3, "d", "e"))
        pds.addRule(normal(3, "d", 3, "f"))
        pds.addRule(push(3, "f", 4, "l", "k"))
        pds.addRule(normal(4, "l", 5, "m"))
        pds.addRule(pop(5, "m", 4))
        pds.addRule(normal(4, "k", 3, "z"))
        pds.addRule(pop(3, "z", 2))
        pds.addRule(normal(2, "e", 6, "i"))
        val fa = accepts(1, "a")
        pds.poststar(fa)
        val faSummaries = accepts(1, "a")
        pds.poststar(faSummaries, new SummaryNestedWeightedPAutomatons[StackSymbol, Abstraction, NoWeight]())
        assertSetEquals(reachableFrom(fa, a(1)), reachableFrom(faSummaries, a(1)))
    }

    private def assertSetEquals(s1: Set[Transition[StackSymbol, Abstraction]], s2: Set[Transition[StackSymbol, Abstraction]]): Unit = {
        if (s1 == s2) return
        val s1MinusS2 = s1 -- s2
        val s2MinusS1 = s2 -- s1
        throw new AssertionError(
            "The sets are not equal: \n S1\\S2 = \n"
                + s1MinusS2.mkString("\n\t")
                + " \n S2\\S1 = \n"
                + s2MinusS1.mkString("\n\t"))
    }

    private def reachableFrom(aut: PAutomaton[StackSymbol, Abstraction], a: Abstraction): Set[Transition[StackSymbol, Abstraction]] = {
        val reachable = mutable.HashSet[Transition[StackSymbol, Abstraction]]()
        aut.registerDFSListener(
            a,
            new ReachabilityListener[StackSymbol, Abstraction]() {
                override def reachable(t: Transition[StackSymbol, Abstraction]): Unit = {
                    reachable += t
                }
            })
        reachable.toSet
    }

    private def reachableMinusTrans(): Set[Transition[StackSymbol, Abstraction]] = {
        val res = fa.getTransitions.toSet -- reachables
        res
    }


}