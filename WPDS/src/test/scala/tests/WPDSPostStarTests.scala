package tests

import org.junit.{Before, Test}
import org.junit.Assert._
import tests.TestHelper._

class WPDSPostStarTests {
    private var pds: WeightedPushdownSystem[StackSymbol, Abstraction, NumWeight] = _

    @Before
    def init(): Unit = {
        pds = new WeightedPushdownSystem[StackSymbol, Abstraction, NumWeight]()
    }

    @Test
    def simple(): Unit = {
        pds.addRule(wnormal(1, "a", 2, "b", w(2)))
        pds.addRule(wnormal(2, "b", 3, "c", w(3)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(fa.getTransitions.size, 3)
        assertEquals(fa.getStates.size, 4)
        assertEquals(fa.getWeightFor(t(3, "c", ACC)), w(5))
    }

    @Test
    def branch(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(2)))
        pds.addRule(wnormal(1, "b", 1, "c", w(3)))
        pds.addRule(wnormal(1, "a", 1, "d", w(3)))
        pds.addRule(wnormal(1, "d", 1, "c", w(3)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(fa.getWeightFor(t(1, "c", ACC)), NumWeight.zero())
        assertEquals(fa.getWeightFor(t(1, "b", ACC)), w(2))
        assertEquals(fa.getWeightFor(t(1, "d", ACC)), w(3))
    }

    @Test
    def push1(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(2)))
        pds.addRule(wpush(1, "b", 1, "c", "d", w(3)))
        pds.addRule(wnormal(1, "c", 1, "e", w(1)))
        pds.addRule(wpop(1, "e", 1, w(5)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(fa.getWeightFor(t(1, "b", ACC)), w(2))
        assertEquals(fa.getWeightFor(t(1, "d", ACC)), w(11))
        assertEquals(fa.getWeightFor(t(1, "e", a(1, "c"))), w(1))
        val weights = fa.getTransitionsToFinalWeights
    }


    @Test
    def push2(): Unit = {
        pds.addRule(wnormal(1, "a", 2, "b", w(2)))
        pds.addRule(wpush(2, "b", 3, "c", "d", w(3)))
        pds.addRule(wnormal(3, "c", 4, "e", w(1)))
        pds.addRule(wpop(4, "e", 5, w(5)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(fa.getWeightFor(t(5, "d", ACC)), w(11))
    }

    @Test
    def twoCall(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(1)))
        pds.addRule(wpush(1, "b", 2, "call", "d", w(2)))
        pds.addRule(wnormal(2, "call", 2, "e", w(3)))
        pds.addRule(wpop(2, "e", 3, w(4)))
        pds.addRule(wnormal(3, "d", 1, "f", w(5)))
        pds.addRule(wpush(1, "f", 2, "call", "g", w(6)))
        pds.addRule(wnormal(3, "g", 4, "h", w(7)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(w(15), fa.getWeightFor(t(1, "f", ACC)))
        assertEquals(w(10), fa.getWeightFor(t(3, "d", ACC)))
        assertEquals(w(28), fa.getWeightFor(t(3, "g", ACC)))
        assertEquals(w(35), fa.getWeightFor(t(4, "h", ACC)))
    }

    @Test
    def oneCall(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(1)))
        pds.addRule(wpush(1, "b", 2, "call", "d", w(2)))
        pds.addRule(wnormal(2, "call", 2, "e", w(3)))
        pds.addRule(wpop(2, "e", 3, w(4)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(w(10), fa.getWeightFor(t(3, "d", ACC)))
    }

    @Test
    def twoCallOnlyReturnWeight(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(0)))
        pds.addRule(wpush(1, "b", 2, "call", "d", w(0)))
        pds.addRule(wnormal(2, "call", 2, "e", w(0)))
        pds.addRule(wpop(2, "e", 3, w(4)))
        pds.addRule(wnormal(3, "d", 3, "f", w(0)))
        pds.addRule(wpush(3, "f", 2, "call", "g", w(0)))
        pds.addRule(wnormal(3, "g", 4, "h", w(0)))
        val fa = waccepts(1, "a", w(0))
        pds.poststar(fa)
        assertEquals(w(4), fa.getWeightFor(t(3, "f", ACC)))
        assertEquals(w(8), fa.getWeightFor(t(3, "g", ACC)))
        assertEquals(w(8), fa.getWeightFor(t(4, "h", ACC)))
    }

    private def w(i: Int): NumWeight = new NumWeight(i)
}