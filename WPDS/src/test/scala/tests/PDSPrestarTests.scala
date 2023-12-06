package tests

import org.junit.{Before, Ignore, Test}
import org.junit.Assert._
import wpds.impl._

@Ignore
class PDSPrestarTests {
    import TestHelper._

    private var pds: PushdownSystem[StackSymbol, Abstraction] = _

    @Before
    def init(): Unit = {
        pds = new PushdownSystem[StackSymbol, Abstraction] {}
    }

    @Test
    def simple(): Unit = {
        pds.addRule(normal(1, "1", 1, "2"))
        pds.addRule(normal(1, "2", 1, "3"))
        val fa = accepts(1, "3")
        pds.prestar(fa)
        assertEquals(3, fa.getTransitions.size)
        assertEquals(2, fa.getStates.size)
        assertTrue(fa.getStates.contains(a(1)))
    }

    @Test
    def simple2(): Unit = {
        pds.addRule(normal(1, "a", 2, "b"))
        pds.addRule(normal(2, "b", 2, "c"))
        val fa = accepts(2, "c")
        pds.prestar(fa)
        assertEquals(3, fa.getTransitions.size)
        assertEquals(3, fa.getStates.size)
        assertTrue(fa.getStates.contains(a(1)))
        assertTrue(fa.getStates.contains(a(2)))
    }

    @Test
    def pushTest(): Unit = {
        pds.addRule(normal(1, "a", 1, "b"))
        pds.addRule(push(1, "b", 1, "c", "d"))
        pds.addRule(pop(1, "c", 1))
        pds.addRule(normal(1, "d", 1, "e"))
        val fa = accepts(1, "e")
        pds.prestar(fa)
        assertTrue(fa.getTransitions.contains(t(1, "c", 1)))
        assertTrue(fa.getTransitions.contains(t(1, "a", ACC)))
    }

    @Test
    def doublePushTest(): Unit = {
        pds.addRule(normal(1, "a", 1, "b"))
        pds.addRule(normal(1, "b", 1, "c"))
        pds.addRule(push(1, "c", 1, "d", "e"))
        pds.addRule(push(1, "d", 1, "h", "i"))
        pds.addRule(pop(1, "h", 1))
        pds.addRule(pop(1, "d", 1))
        pds.addRule(normal(1, "e", 1, "k"))
        var fa = accepts(1, "k")
        pds.prestar(fa)
        println(fa)
        assertTrue(fa.getTransitions.contains(t(1, "k", ACC)))
        fa = accepts(1, "k")
        pds.prestar(fa)
        assertTrue(fa.getTransitions.contains(t(1, "a", ACC)))
    }


    @Test
    def recPushTest(): Unit = {
        pds.addRule(normal(1, "a", 1, "b"))
        pds.addRule(normal(1, "b", 1, "c"))
        pds.addRule(push(1, "c", 1, "d", "e"))
        pds.addRule(normal(1, "d", 1, "f"))
        pds.addRule(push(1, "f", 1, "d", "h"))
        pds.addRule(pop(1, "d", 1))
        pds.addRule(normal(1, "e", 1, "k"))
        val fa = accepts(1, "k")
        pds.prestar(fa)
        println(fa)
        assertTrue(fa.getTransitions.contains(t(1, "c", ACC)))
        assertTrue(fa.getTransitions.contains(t(1, "a", ACC)))
    }

    @Test
    def recPushTestSimple(): Unit = {
        pds.addRule(push(1, "a", 1, "d", "e"))
        pds.addRule(push(1, "d", 1, "d", "h"))
        pds.addRule(pop(1, "d", 1))
        pds.addRule(normal(1, "e", 1, "k"))
        val fa = accepts(1, "k")
        pds.prestar(fa)
        assertTrue(fa.getTransitions.contains(t(1, "a", ACC)))
    }

    @Test
    def paperEx(): Unit = {
        pds.addRule(normal(1, "n1", 1, "n2"))
        pds.addRule(normal(1, "n1", 1, "n3"))
        pds.addRule(push(1, "n2", 1, "n7", "n4"))
        pds.addRule(push(1, "n3", 1, "n7", "n5"))
        pds.addRule(normal(1, "n4", 1, "n6"))
        pds.addRule(normal(1, "n5", 1, "n6"))
        pds.addRule(normal(1, "n7", 1, "n8"))
        pds.addRule(pop(1, "n8", 1))
        val fa = accepts(1, "n6")
        pds.prestar(fa)
        println(fa)
        val transitions = fa.getTransitions
        transitions.remove(t(1, "n1", ACC))
        transitions.remove(t(1, "n2", ACC))
        transitions.remove(t(1, "n3", ACC))
        transitions.remove(t(1, "n4", ACC))
        transitions.remove(t(1, "n5", ACC))
        transitions.remove(t(1, "n6", ACC))
        transitions.remove(t(1, "n7", 1))
        transitions.remove(t(1, "n8", 1))
        assertTrue(transitions.isEmpty)
    }









}