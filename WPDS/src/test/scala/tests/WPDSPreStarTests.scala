package tests

import org.junit.{Before, Ignore, Test}
import org.junit.Assert._
import tests.TestHelper._

@Ignore
class WPDSPreStarTests {
    private var pds: WeightedPushdownSystem[StackSymbol, Abstraction, NumWeight] = _

    @Before
    def init(): Unit = {
        pds = new WeightedPushdownSystem[StackSymbol, Abstraction, NumWeight]()
    }

    @Test
    def simple(): Unit = {
        pds.addRule(wnormal(1, "a", 2, "b", w(2)))
        pds.addRule(wnormal(2, "b", 3, "c", w(3)))
        val fa = waccepts(3, "c", w(0))
        pds.prestar(fa)
        assertEquals(fa.getTransitions.size, 3)
        assertEquals(fa.getStates.size, 4)
        assertEquals(fa.getWeightFor(t(1, "a", ACC)), w(5))
    }

    @Test
    def branch(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(2)))
        pds.addRule(wnormal(1, "b", 1, "c", w(4)))
        pds.addRule(wnormal(1, "a", 1, "d", w(3)))
        pds.addRule(wnormal(1, "d", 1, "c", w(3)))
        val fa = waccepts(1, "c", w(0))
        pds.prestar(fa)
        assertEquals(fa.getWeightFor(t(1, "a", ACC)), w(6))
        assertEquals(fa.getWeightFor(t(1, "b", ACC)), w(4))
        assertEquals(fa.getWeightFor(t(1, "d", ACC)), w(3))
    }

    @Test
    def push1(): Unit = {
        pds.addRule(wnormal(1, "a", 1, "b", w(2)))
        pds.addRule(wpush(1, "b", 1, "c", "d", w(3)))
        pds.addRule(wnormal(1, "c", 1, "e", w(1)))
        pds.addRule(wpop(1, "e", 1, w(5)))
        pds.addRule(wnormal(1, "d", 1, "f", w(6)))
        val fa = waccepts(1, "f", w(0))
        pds.prestar(fa)
        assertEquals(fa.getWeightFor(t(1, "a", ACC)), w(17))
        assertEquals(fa.getWeightFor(t(1, "b", ACC)), w(15))
        assertEquals(fa.getWeightFor(t(1, "c", 1)), w(6))
    }

    private def w(i: Int): NumWeight = new NumWeight(i)
}