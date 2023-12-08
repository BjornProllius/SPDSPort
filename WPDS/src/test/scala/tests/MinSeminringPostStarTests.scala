package tests

import org.junit.{Before, Test}
import org.junit.Assert._
import wpds.impl._

class MinSeminringPostStarTests {
    import TestHelper._

    private var pds: WeightedPushdownSystem[StackSymbol, Abstraction, MinSemiring] = _

    @Before
    def init(): Unit = {
        pds = new WeightedPushdownSystem[StackSymbol, Abstraction, MinSemiring]()
    }

    @Test
    def simple(): Unit = {
        pds.addRule(wnormal(1, "a", 2, "b", w(1)))
        pds.addRule(wnormal(2, "b", 3, "c", w(1)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(3, fa.getTransitions.size)
        assertEquals(4, fa.getStates.size)
        assertEquals(w(2), fa.getWeightFor(t(3, "c", ACC)))
    }

    @Test
    def branch(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(1)))
        pds.addRule(wnormal(1, "b", 1, "c", w(1)))
        pds.addRule(wnormal(1, "a", 1, "d", w(1)))
        pds.addRule(wnormal(1, "d", 1, "c", w(1)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(w(2), fa.getWeightFor(t(1, "c", ACC)))
        assertEquals(w(1), fa.getWeightFor(t(1, "b", ACC)))
        assertEquals(w(1), fa.getWeightFor(t(1, "d", ACC)))
    }

    @Test
    def push1(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(1)))
        pds.addRule(wpush(1, "b", 1, "c", "d", w(1)))
        pds.addRule(wnormal(1, "c", 1, "e", w(1)))
        pds.addRule(wpop(1, "e", 1, w(1)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(w(1), fa.getWeightFor(t(1, "b", ACC)))
        assertEquals(w(4), fa.getWeightFor(t(1, "d", ACC)))
        // assertEquals(w(2), fa.getWeightFor(t(1, "e", a(1, "c"))))
    }

    @Test
    def push2(): Unit = {
        pds.addRule(wnormal(1, "a", 2, "b", w(1)))
        pds.addRule(wpush(2, "b", 3, "c", "d", w(2)))
        pds.addRule(wnormal(3, "c", 4, "e", w(1)))
        pds.addRule(wpop(4, "e", 5, w(1)))
        pds.addRule(wnormal(5, "d", 2, "f", w(10)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(w(5), fa.getWeightFor(t(5, "d", ACC)))
        assertEquals(w(15), fa.getWeightFor(t(2, "f", ACC)))
    }

    @Test
    def twoCall(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(1)))
        pds.addRule(wpush(1, "b", 1, "call", "d", w(1)))
        pds.addRule(wnormal(1, "call", 1, "e", w(1)))
        pds.addRule(wpop(1, "e", 1, w(1)))
        pds.addRule(wnormal(1, "d", 1, "f", w(1)))
        pds.addRule(wpush(1, "f", 1, "call", "g", w(1)))
        pds.addRule(wnormal(1, "g", 1, "h", w(1)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(w(9), fa.getWeightFor(t(1, "h", ACC)))
    }

    private def w(i: Int): MinSemiring = new MinSemiring(i)

    private def waccepts(a: Int, c: String, weight: MinSemiring): WeightedPAutomaton[StackSymbol, Abstraction, MinSemiring] = {
        val aut = new WeightedPAutomaton[StackSymbol, Abstraction, MinSemiring]() {
            override def createState(d: Abstraction, loc: StackSymbol): Abstraction = new Abstraction(d, loc)
            override def epsilon(): StackSymbol = s("EPS")
            override def getOne(): MinSemiring = MinSemiring.one()
            override def isGeneratedState(d: Abstraction): Boolean = d.s != null
        }
        aut.addFinalState(ACC)
        aut.addTransition(t(a, c, ACC))
        aut.addWeightForTransition(t(a, c, ACC), weight)
        aut
    }

    private def wnormal(a: Int, n: String, b: Int, m: String, w: MinSemiring): NormalRule[StackSymbol, Abstraction, MinSemiring] =
        new NormalRule[StackSymbol, Abstraction, MinSemiring](a(a), s(n), a(b), s(m), w)

    private def wpush(a: Int, n: String, b: Int, m: String, l: String, w: MinSemiring): PushRule[StackSymbol, Abstraction, MinSemiring] =
        new PushRule[StackSymbol, Abstraction, MinSemiring](a(a), s(n), a(b), s(m), s(l), w)

    private def wpop(a: Int, n: String, b: Int, w: MinSemiring): PopRule[StackSymbol, Abstraction, MinSemiring] =
        new PopRule[StackSymbol, Abstraction, MinSemiring](a(a), s(n), a(b), w)
}