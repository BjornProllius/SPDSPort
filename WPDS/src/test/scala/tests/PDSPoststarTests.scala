package tests

import org.junit.{Before, Test}
import org.junit.Assert._
import wpds.impl._

class PDSPoststarTests {
    import TestHelper._

    private var pds: PushdownSystem[StackSymbol, Abstraction] = _

    @Before
    def init(): Unit = {
        pds = new PushdownSystem[StackSymbol, Abstraction] {}
    }

    @Test
    def popEpsilonTest1(): Unit = {
        pds.addRule(push(2, "b", 2, "c", "d"))
        pds.addRule(pop(3, "c", 3))

        val fa = accepts(2, "b")
        fa.addTransition(new Transition[StackSymbol, Abstraction](a(3), fa.epsilon(), a(2)))
        pds.poststar(fa)
        assertTrue(fa.getTransitions.contains(t(3, "EPS", 2)))
        assertTrue(fa.getTransitions.contains(t(2, "b", ACC)))
    }

    @Test
    def popEpsilonTest(): Unit = {
        pds.addRule(push(1, "b", 1, "c", "d"))
        pds.addRule(pop(1, "c", 1))
        val fa = accepts(1, "b")
        fa.addTransition(new Transition[StackSymbol, Abstraction](a(0), fa.epsilon(), a(1)))
        pds.poststar(fa)
        assertTrue(fa.getTransitions.contains(t(1, "d", ACC)))
        assertTrue(fa.getTransitions.contains(t(0, "EPS", 1)))
    }

    @Test
    def pushTest(): Unit = {
        pds.addRule(normal(1, "a", 1, "b"))
        pds.addRule(push(1, "b", 1, "c", "d"))
        pds.addRule(pop(1, "c", 1))
        val fa = accepts(1, "a")
        pds.poststar(fa)
        assertTrue(fa.getTransitions.contains(t(1, "d", ACC)))
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
        val fa = accepts(1, "a")
        pds.poststar(fa)
        assertTrue(fa.getTransitions.contains(t(1, "k", ACC)))
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
        val fa = accepts(1, "a")
        pds.poststar(fa)
        assertTrue(fa.getTransitions.contains(t(1, "k", ACC)))
        assertTrue(fa.getTransitions.contains(t(1, "k", ACC)))
    }

    @Test
    def recPushTestSimple(): Unit = {
        pds.addRule(push(1, "a", 1, "d", "e"))
        pds.addRule(push(1, "d", 1, "d", "h"))
        pds.addRule(pop(1, "d", 1))
        pds.addRule(normal(1, "e", 1, "k"))
        val fa = accepts(1, "a")
        pds.poststar(fa)
        val transitions = fa.getTransitions
        transitions.remove(t(1, "e", ACC))
        transitions.remove(t(1, "a", ACC))
        transitions.remove(t(1, "k", ACC))
        transitions.remove(t(a(1, "d"), "e", ACC))
        transitions.remove(t(a(1, "d"), s("h"), a(1, "d")))
        transitions.remove(t(1, s("d"), a(1, "d")))
        transitions.remove(t(1, s("h"), a(1, "d")))
        transitions.remove(t(1, fa.epsilon(), a(1, "d")))
        assertTrue(transitions.isEmpty)
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
        val fa = accepts(1, "n1")
        pds.poststar(fa)
        val transitions = fa.getTransitions
        transitions.remove(t(1, "n1", ACC))
        transitions.remove(t(1, "n2", ACC))
        transitions.remove(t(1, "n3", ACC))
        transitions.remove(t(1, "n4", ACC))
        transitions.remove(t(1, "n5", ACC))
        transitions.remove(t(1, "n6", ACC))
        transitions.remove(t(1, fa.epsilon(), a(1, "n7")))
        transitions.remove(t(1, "n7", a(1, "n7")))
        transitions.remove(t(1, "n8", a(1, "n7")))
        transitions.remove(t(a(1, "n7"), "n4", ACC))
        transitions.remove(t(a(1, "n7"), "n5", ACC))
        assertTrue(transitions.isEmpty)
    }
    
}