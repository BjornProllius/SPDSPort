package analysis.test

import org.junit.Assert._
import com.google.common.collect.{HashMultimap, Multimap}
import org.junit.Test
import org.slf4j.{Logger, LoggerFactory}
import sync.pds.solver.{OneWeightFunctions, SyncPDSSolver, WeightFunctions}
import sync.pds.solver.SyncPDSSolver.PDSSystem
import sync.pds.solver.nodes.{ExclusionNode, Node, NodeWithLocation, PopNode, PushNode, SingleNode}
import wpds.impl.{SummaryNestedWeightedPAutomatons, Weight}
import wpds.interfaces.{Location, State}
import wpds.wildcard.{ExclusionWildcard, Wildcard}


class DoublePDSTest {
    private val LOGGER: Logger = LoggerFactory.getLogger(classOf[DoublePDSTest])
    private val successorMap: Multimap[Node[Statement, Variable], State] = HashMultimap.create()
    private val summaryMap: Multimap[Node[Statement, Variable], Node[Statement, Variable]] = HashMultimap.create()

    private def addFieldPop(curr: Node[Statement, Variable], ref: FieldRef, succ: Node[Statement, Variable]): Unit = {
        addSucc(curr, new PopNode(new NodeWithLocation(succ.stmt, succ.fact, ref), PDSSystem.FIELDS))
    }

    private def addFieldPush(curr: Node[Statement, Variable], push: FieldRef, succ: Node[Statement, Variable]): Unit = {
        addSucc(curr, new PushNode(succ.stmt, succ.fact, push, PDSSystem.FIELDS))
    }

    private def addNormal(curr: Node[Statement, Variable], succ: Node[Statement, Variable]): Unit = {
        addSucc(curr, succ)
    }

    private def addReturnFlow(curr: Node[Statement, Variable], returns: Variable): Unit = {
        addSucc(curr, new PopNode(returns, PDSSystem.CALLS))
    }

    private def addCallFlow(curr: Node[Statement, Variable], succ: Node[Statement, Variable], returnSite: Statement): Unit = {
        addSucc(curr, new PushNode(succ.stmt, succ.fact, returnSite, PDSSystem.CALLS))
    }

    private def calleeToCallerMapping(ret: Node[Statement, Variable], succOfRet: Node[Statement, Variable]): Unit = {
        summaryMap.put(ret, succOfRet)
    }

    private def addSucc(curr: Node[Statement, Variable], succ: State): Unit = {
        successorMap.put(curr, succ)
    }

    private def addExcludeField(curr: Node[Statement, Variable], push: FieldRef, succ: Node[Statement, Variable]): Unit = {
        addSucc(curr, new ExclusionNode(succ.stmt, succ.fact, push))
    }

    private val epsilonField: FieldRef = new FieldRef("EMPTY")
    private val epsilonCallSite: Statement = new Statement(-1)

    private val solver: SyncPDSSolver[Statement, Variable, FieldRef, NoWeight] = new TestSyncPDSSolver()

    private class TestSyncPDSSolver extends SyncPDSSolver[Statement, Variable, FieldRef, NoWeight](
        false,
        new SummaryNestedWeightedPAutomatons(),
        false,
        new SummaryNestedWeightedPAutomatons(),
        -1,
        -1,
        -1
    )   {

        override def computeSuccessor(node: Node[Statement, Variable]): Unit = {
            val states = successorMap.get(node)
            for (s <- states) {
                propagate(node, s)
            }
        }

        override def epsilonField(): FieldRef = new FieldRef("eps_f")

        override def epsilonStmt(): Statement = epsilonCallSite

        override def emptyField(): FieldRef = epsilonField

        override def fieldWildCard(): FieldRef = new FieldWildCard()

        override def exclusionFieldWildCard(exclusion: FieldRef): FieldRef = new ExclusionWildcardField(exclusion)

     override def applyCallSummary(
            callSite: Statement,
           factInCallee: Variable,
           sPInCallee: Statement,
           exitStmt: Statement,
           exitingFact: Variable
      ): Unit = {
         val exitingNode = new Node(exitStmt, exitingFact)
          for (n <- summaryMap.get(exitingNode)) {
               addNormalFieldFlow(exitingNode, n)
                addNormalCallFlow(new Node(callSite, exitingFact), n)
            }
        }

        override def getFieldWeights(): WeightFunctions[Statement, Variable, FieldRef, NoWeight] = 
            new OneWeightFunctions(NoWeight.NO_WEIGHT_ONE)

        override def getCallWeights(): WeightFunctions[Statement, Variable, Statement, NoWeight] = 
            new OneWeightFunctions(NoWeight.NO_WEIGHT_ONE)
    }

    private def solve(node: Node[Statement, Variable]): Unit = {
        solver.solve(
            node,
            epsilonField,
            new SingleNode(node(1, "u")),
            epsilonCallSite,
            new SingleNode(new Variable("u"))
        )

        LOGGER.info("All reachable states of SPDS: {}", solver.getReachedStates)
    }

    @Test
    def test1(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addCallFlow(node(2, "v"), node(3, "p"), returnSite(5))
        addFieldPush(node(3, "p"), f("g"), node(4, "q"))
        addReturnFlow(node(4, "q"), var("q"))
        addFieldPop(node(5, "w"), f("g"), node(6, "x"))
        addFieldPop(node(6, "x"), f("f"), node(7, "y"))

        // second branch
        addFieldPush(node(8, "r"), f("f"), node(9, "s"))
        addCallFlow(node(9, "s"), node(3, "p"), returnSite(10))
        addReturnFlow(node(4, "q"), var("q"))
        addFieldPush(node(10, "t"), f("f"), node(11, "s"))

        calleeToCallerMapping(node(4, "q"), node(5, "w"))
        calleeToCallerMapping(node(4, "q"), node(10, "t"))

        solve(node(1, "u"))
        // TODO needs inspection
        // assertFalse(solver.getReachedStates.contains(node(11, "s")))
        assertTrue(solver.getReachedStates.contains(node(5, "w")))
        assertTrue(solver.getReachedStates.contains(node(6, "x")))
        assertFalse(solver.getReachedStates.contains(node(7, "y")))
    }

    @Test
    def branching(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPush(node(1, "u"), f("g"), node(3, "x"))

        // can pop
        addFieldPop(node(2, "v"), f("h"), node(5, "y"))
        addFieldPop(node(3, "x"), f("g"), node(4, "y"))

        // but cannot pop
        addFieldPop(node(5, "y"), f("g"), node(6, "y"))
        addFieldPop(node(4, "y"), f("g"), node(7, "y"))
        solve(node(1, "u"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(5, "y")))
        assertTrue(solver.getReachedStates.contains(node(4, "y")))
        assertFalse(solver.getReachedStates.contains(node(6, "y")))
        assertFalse(solver.getReachedStates.contains(node(7, "y")))
    }

    @Test
    def tooMuchPopping(): Unit = {
        addFieldPush(node(1, "u"), f("g"), node(3, "x"))
        addFieldPop(node(3, "x"), f("g"), node(3, "y"))
        addFieldPop(node(3, "y"), f("g"), node(3, "z"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(3, "y")))
        assertFalse(solver.getReachedStates.contains(node(3, "z")))
    }

    @Test
    def test1Simple(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addCallFlow(node(2, "v"), node(3, "p"), returnSite(5))
        addFieldPush(node(3, "p"), f("g"), node(4, "q"))
        addReturnFlow(node(4, "q"), var("q"))
        addFieldPop(node(5, "w"), f("g"), node(6, "x"))
        addFieldPop(node(6, "x"), f("f"), node(7, "y"))

        calleeToCallerMapping(node(4, "q"), node(5, "w"))
        solve(node(1, "u"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(6, "x")))
    }

    @Test
    def callOnlyIntraprocedural(): Unit = {
        addNormal(node(1, "u"), node(5, "q"))
        addNormal(node(5, "q"), node(6, "x"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(6, "x")))
    }

    @Test
    def fieldPushAndPop(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPop(node(2, "v"), f("h"), node(6, "x"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(6, "x")))
    }

    @Test
    def simpleNonFieldFlow(): Unit = {
        addNormal(node(1, "v"), node(2, "w"))
        addCallFlow(node(2, "w"), node(3, "p"), returnSite(4))
        addNormal(node(3, "p"), node(5, "q"))
        addNormal(node(5, "q"), node(7, "z"))
        addNormal(node(7, "z"), node(6, "x"))
        addReturnFlow(node(6, "x"), var("x"))
        addNormal(node(4, "k"), node(6, "y"))

        calleeToCallerMapping(node(6, "x"), node(4, "k"))
        solve(node(1, "v"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(6, "y")))
    }

    @Test
    def simpleExclusionFieldFlow(): Unit = {
        addFieldPush(node(1, "v"), f("g"), node(4, "w"))
        addExcludeField(node(4, "w"), f("g"), node(5, "w"))
        addFieldPop(node(5, "w"), f("g"), node(7, "w"))

        solve(node(1, "v"))
        solver.debugOutput()
        assertFalse(solver.getReachedStates.contains(node(7, "w")))
    }

    @Test
    def simpleNegativeExclusionFieldFlow(): Unit = {
        addFieldPush(node(1, "v"), f("g"), node(4, "w"))
        addExcludeField(node(4, "w"), f("h"), node(5, "w")) // overwrite of h should not affect the subsequent pop operation
        addFieldPop(node(5, "w"), f("g"), node(7, "w"))

        solve(node(1, "v"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(7, "w")))
    }

    @Test
    def doubleNegativeExclusionFieldFlow(): Unit = {
        addFieldPush(node(1, "v"), f("g"), node(4, "w"))
        addExcludeField(node(4, "w"), f("h"), node(5, "w")) // overwrite of h should not affect the subsequent pop operation
        addExcludeField(node(5, "w"), f("i"), node(6, "w")) // overwrite of h should not affect the subsequent pop operation
        addFieldPop(node(6, "w"), f("g"), node(7, "w"))

        solve(node(1, "v"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(7, "w")))
    }

    @Test
    def doubleExclusionFieldFlow(): Unit = {
        addFieldPush(node(1, "v"), f("g"), node(4, "w"))
        addExcludeField(node(4, "w"), f("i"), node(5, "w")) // overwrite of i should not affect the subsequent pop operation
        addExcludeField(node(5, "w"), f("g"), node(6, "w"))
        addFieldPop(node(6, "w"), f("g"), node(7, "w"))

        solve(node(1, "v"))
        solver.debugOutput()
        assertFalse(solver.getReachedStates.contains(node(7, "w")))
    }

    @Test
    def simpleTransitiveExclusionFieldFlow(): Unit = {
        addFieldPush(node(1, "v"), f("g"), node(4, "w"))
        addExcludeField(node(4, "w"), f("g"), node(5, "w"))
        addNormal(node(5, "w"), node(6, "w"))
        addFieldPop(node(6, "w"), f("g"), node(7, "w"))

        solve(node(1, "v"))
        solver.debugOutput()
        assertFalse(solver.getReachedStates.contains(node(7, "w")))
    }


    @Test
    def simpleNegativeTransitiveExclusionFieldFlow(): Unit = {
        addFieldPush(node(1, "v"), f("g"), node(4, "w"))
        addExcludeField(node(4, "w"), f("h"), node(5, "w")) // overwrite of h should not affect the subsequent pop operation
        addNormal(node(5, "w"), node(6, "w"))
        addFieldPop(node(6, "w"), f("g"), node(7, "w"))

        solve(node(1, "v"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(7, "w")))
    }

    @Test
    def testWithTwoStacks(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addCallFlow(node(2, "v"), node(3, "p"), returnSite(4))
        addFieldPush(node(3, "p"), f("g"), node(5, "q"))
        addReturnFlow(node(5, "q"), var("q"))
        addNormal(node(4, "w"), node(7, "t"))

        calleeToCallerMapping(node(5, "q"), node(4, "w"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(7, "t")))
    }
    @Test
    def testWithTwoStacksAndTwoField(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addCallFlow(node(2, "v"), node(3, "p"), returnSite(4))
        addFieldPush(node(3, "p"), f("g"), node(5, "q"))
        addFieldPush(node(5, "q"), f("f"), node(6, "q"))
        addReturnFlow(node(6, "q"), var("q"))
        addNormal(node(4, "w"), node(7, "t"))
        addFieldPop(node(7, "t"), f("f"), node(8, "s"))
        addFieldPop(node(8, "s"), f("g"), node(9, "x"))
        addFieldPop(node(9, "x"), f("h"), node(10, "y"))
        addFieldPop(node(9, "x"), f("impossibleRead"), node(11, "z"))

        calleeToCallerMapping(node(6, "q"), node(4, "w"))

        solve(node(1, "u"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(7, "t")))
        assertTrue(solver.getReachedStates.contains(node(8, "s")))
        assertTrue(solver.getReachedStates.contains(node(9, "x")))
        assertTrue(solver.getReachedStates.contains(node(10, "y")))
        assertFalse(solver.getReachedStates.contains(node(11, "z")))
    }

    @Test
    def positiveTestFieldDoublePushAndPop(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPush(node(2, "v"), f("g"), node(3, "w"))
        addFieldPop(node(3, "w"), f("g"), node(4, "x"))
        addNormal(node(3, "w"), node(4, "kkk"))
        addFieldPop(node(4, "x"), f("h"), node(5, "y"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(4, "x")))
        assertTrue(solver.getReachedStates.contains(node(5, "y")))
    }

    @Test
    def negativeTestFieldDoublePushAndPop(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPush(node(2, "v"), f("h"), node(3, "w"))
        addFieldPop(node(3, "w"), f("h"), node(4, "x"))
        addFieldPop(node(4, "x"), f("g"), node(5, "y"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(4, "x")))
        assertFalse(solver.getReachedStates.contains(node(5, "y")))
    }

    @Test
    def positiveTestFieldPushPushAndPop(): Unit = {
        addFieldPush(node(0, "u"), f("h"), node(1, "u"))
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPop(node(2, "v"), f("h"), node(2, "x"))
        solve(node(0, "u"))
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(2, "x")))
    }

    @Test
    def negativeTestFieldPushAndPopPop(): Unit = {
        addFieldPush(node(0, "u"), f("h"), node(1, "u"))
        addFieldPop(node(1, "u"), f("h"), node(2, "v"))
        addFieldPop(node(2, "v"), f("h"), node(2, "x"))
        solve(node(0, "u"))
        assertFalse(solver.getReachedStates.contains(node(2, "x")))
    }

    @Test
    def negativeSinglePop(): Unit = {
        addNormal(node(0, "u"), node(1, "u"))
        addFieldPop(node(1, "u"), f("h"), node(2, "v"))
        solve(node(0, "u"))
        assertFalse(solver.getReachedStates.contains(node(2, "v")))
    }

    @Test
    def negativeJustPop(): Unit = {
        addFieldPop(node(0, "u"), f("h"), node(2, "v"))
        solve(node(0, "u"))
        println(solver.getReachedStates)
        assertFalse(solver.getReachedStates.contains(node(2, "v")))
    }

    @Test
    def positiveTestFieldPushAndPop(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPop(node(2, "v"), f("h"), node(2, "x"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(2, "x")))
    }

    @Test
    def positiveTestFieldIntermediatePushAndPop(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addNormal(node(2, "v"), node(3, "w"))
        addNormal(node(3, "w"), node(4, "w"))
        addFieldPop(node(4, "w"), f("h"), node(5, "w"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(5, "w")))
    }

    @Test
    def positiveTestFieldLoop(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPush(node(2, "v"), f("h"), node(2, "v"))
        addFieldPop(node(2, "v"), f("h"), node(3, "w"))
        addFieldPop(node(3, "w"), f("h"), node(4, "x"))
        addFieldPop(node(4, "x"), f("h"), node(5, "y"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(4, "x")))
        assertTrue(solver.getReachedStates.contains(node(5, "y")))
    }

    @Test
    def positiveTestFieldLoop2(): Unit = {
        addFieldPush(node(0, "a"), f("g"), node(1, "u"))
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPush(node(2, "v"), f("h"), node(2, "v"))
        addFieldPop(node(2, "v"), f("h"), node(3, "w"))
        addFieldPop(node(3, "w"), f("h"), node(4, "x"))
        addFieldPop(node(4, "x"), f("g"), node(5, "y"))
        solve(node(0, "a"))
        assertTrue(solver.getReachedStates.contains(node(5, "y")))
    }

    @Test
    def positiveSummaryTest(): Unit = {
        addFieldPush(node(0, "c"), f("g"), node(1, "a"))
        addCallFlow(node(1, "a"), node(2, "u"), returnSite(4))
        addFieldPush(node(2, "u"), f("f"), node(3, "u"))
        addReturnFlow(node(3, "u"), var("u"))

        addNormal(node(4, "a"), node(5, "e"))
        addCallFlow(node(5, "e"), node(2, "u"), returnSite(6))
        addReturnFlow(node(3, "u"), var("u"))
        addFieldPop(node(6, "a"), f("f"), node(7, "h"))
        addFieldPop(node(7, "h"), f("f"), node(8, "g"))
        addFieldPop(node(8, "g"), f("g"), node(9, "z"))
        addFieldPop(node(8, "g"), f("f"), node(9, "y"))

        calleeToCallerMapping(node(3, "u"), node(4, "a"))
        calleeToCallerMapping(node(3, "u"), node(6, "a"))
        solve(node(0, "c"))
        println(solver.getReachedStates)
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(7, "h")))
        assertTrue(solver.getReachedStates.contains(node(8, "g")))
        assertTrue(solver.getReachedStates.contains(node(9, "z")))
        // assertFalse(solver.getReachedStates.contains(node(9,"y")))//False Positive
    }

    @Test
    def positiveSummaryWithFieldTest(): Unit = {
        addFieldPush(node(0, "c"), f("g"), node(1, "a"))
        addCallFlow(node(1, "a"), node(2, "u"), returnSite(4))
        addFieldPush(node(2, "u"), f("f"), node(3, "u"))
        addReturnFlow(node(3, "u"), var("u"))
        addFieldPop(node(4, "a"), f("f"), node(5, "e"))
        addCallFlow(node(5, "e"), node(2, "u"), returnSite(6))
        addFieldPop(node(6, "a"), f("f"), node(7, "h")) // Due to the summary, we should be able to read f again.
        addFieldPop(node(7, "h"), f("g"), node(8, "l")) // Due to the summary, we should be able to read f again.
        calleeToCallerMapping(node(3, "u"), node(4, "a"))
        calleeToCallerMapping(node(3, "u"), node(6, "a"))
        solve(node(0, "c"))
        println(solver.getReachedStates)
        solver.debugOutput()
        assertTrue(solver.getReachedStates.contains(node(7, "h")))
        assertTrue(solver.getReachedStates.contains(node(8, "l")))
    }

    @Test
    def simpleFieldPushAndPopAndContext(): Unit = {
        addFieldPush(node(0, "c"), f("g"), node(1, "a"))
        addCallFlow(node(1, "a"), node(2, "u"), returnSite(4))
        addFieldPush(node(2, "u"), f("f"), node(3, "u"))
        addReturnFlow(node(3, "u"), var("u"))
        addFieldPop(node(4, "a"), f("f"), node(5, "e"))
        addFieldPop(node(5, "e"), f("g"), node(6, "f")) // Should be possible
        calleeToCallerMapping(node(3, "u"), node(4, "a"))
        solve(node(0, "c"))
        assertTrue(solver.getReachedStates.contains(node(6, "f")))
    }

    @Test
    def positiveNoFieldsSummaryTest(): Unit = {
        addNormal(node(0, "c"), node(1, "a"))
        addCallFlow(node(1, "a"), node(2, "u"), returnSite(4))
        addNormal(node(2, "u"), node(3, "u"))
        addReturnFlow(node(3, "u"), var("u"))
        addNormal(node(4, "a"), node(5, "e"))
        addCallFlow(node(5, "e"), node(2, "u"), returnSite(6))
        addNormal(node(6, "e"), node(7, "h"))
        calleeToCallerMapping(node(3, "u"), node(4, "a"))
        calleeToCallerMapping(node(3, "u"), node(6, "e"))
        solve(node(0, "c"))
        assertTrue(solver.getReachedStates.contains(node(7, "h")))
    }

    @Test
    def positiveSummaryFlowTest(): Unit = {
        addCallFlow(node(1, "a"), node(2, "u"), returnSite(4))
        addReturnFlow(node(2, "u"), var("e"))
        addCallFlow(node(4, "e"), node(2, "u"), returnSite(6))
        calleeToCallerMapping(node(2, "u"), node(4, "e"))
        calleeToCallerMapping(node(2, "u"), node(6, "e"))
        solve(node(1, "a"))
        assertTrue(solver.getReachedStates.contains(node(6, "e")))
    }

    @Test
    def recursion(): Unit = {
        addCallFlow(node(1, "a"), node(2, "u"), returnSite(4))
        addNormal(node(2, "u"), node(3, "c"))
        addFieldPush(node(3, "c"), f("h"), node(4, "h"))
        addCallFlow(node(4, "h"), node(2, "u"), returnSite(5))
        addNormal(node(4, "h"), node(5, "h"))
        addFieldPop(node(5, "h"), f("h"), node(6, "g"))
        addFieldPop(node(6, "g"), f("h"), node(7, "g"))
        addReturnFlow(node(7, "g"), var("g"))
        calleeToCallerMapping(node(7, "g"), node(4, "a"))
        solve(node(1, "a"))
        assertTrue(solver.getReachedStates.contains(node(4, "a")))
    }

    @Test
    def recursion2(): Unit = {
        addCallFlow(node(1, "a"), node(2, "u"), returnSite(4))
        addNormal(node(2, "u"), node(3, "c"))
        addFieldPush(node(3, "c"), f("h"), node(4, "h"))
        addCallFlow(node(4, "h"), node(2, "u"), returnSite(5))
        addNormal(node(4, "h"), node(5, "h"))
        addFieldPop(node(5, "h"), f("h"), node(6, "g"))
        addFieldPop(node(6, "g"), f("h"), node(7, "g"))
        addReturnFlow(node(7, "g"), var("g"))
        calleeToCallerMapping(node(7, "g"), node(4, "a"))
        solve(node(1, "a"))
        assertTrue(solver.getReachedStates.contains(node(4, "a")))
    }

    @Test
    def negativeTestFieldPushAndPop(): Unit = {
        addFieldPush(node(1, "u"), f("h"), node(2, "v"))
        addFieldPop(node(2, "v"), f("f"), node(3, "w"))
        solve(node(1, "u"))
        assertFalse(solver.getReachedStates.contains(node(3, "w")))
    }

    @Test
    def negativeTestCallSitePushAndPop(): Unit = {
        addCallFlow(node(1, "u"), node(2, "v"), returnSite(4))
        addReturnFlow(node(2, "v"), var("w"))
        addNormal(node(3, "w"), node(4, "w"))
        solve(node(1, "u"))
        println(solver.getReachedStates)
        assertFalse(solver.getReachedStates.contains(node(3, "w")))
    }

    @Test
    def positiveTestCallSitePushAndPop(): Unit = {
        addCallFlow(node(1, "u"), node(4, "v"), returnSite(2))
        addReturnFlow(node(4, "v"), var("v"))
        addNormal(node(2, "w"), node(3, "w"))
        calleeToCallerMapping(node(4, "v"), node(2, "w"))
        solve(node(1, "u"))
        assertTrue(solver.getReachedStates.contains(node(3, "w")))
    }

    def var(v: String): Variable = new Variable(v)

    def returnSite(call: Int): Statement = new Statement(call)

    def f(f: String): FieldRef = new FieldRef(f)

    def node(stmt: Int, var: String): Node[Statement, Variable] = new Node[Statement, Variable](new Statement(stmt), new Variable(var))

    class Statement(name: Int) extends StringBasedObj(Integer.toString(name)) with Location {
        override def accepts(other: Location): Boolean = this.equals(other)
    }

    class Variable(name: String) extends StringBasedObj(name)

    class FieldWildCard extends FieldRef("*") with Wildcard

    class ExclusionWildcardField(excl: FieldRef) extends FieldRef(excl.name) with ExclusionWildcard[FieldRef] {
        private val excludes: FieldRef = excl

        override def excludes(): FieldRef = excludes

        override def toString: String = "not " + super.toString

        override def hashCode: Int = {
            val prime = 31
            var result = super.hashCode
            result = prime * result + (if (excludes == null) 0 else excludes.hashCode)
            result
        }
    }


    override def equals(obj: Any): Boolean = {
        obj match {
            case other: ExclusionWildcardField =>
                if (this == other) return true
                if (!super.equals(other)) return false
                if (getClass != other.getClass) return false
                if (excludes == null) {
                    if (other.excludes != null) return false
                } else if (!excludes.equals(other.excludes)) return false
                true
            case _ => false
        }
    }

    class FieldRef(name: String) extends StringBasedObj(name) with Location {
        override def accepts(other: Location): Boolean = this.equals(other)
    }

    class StringBasedObj(val name: String) {
        override def hashCode(): Int = {
            val prime = 31
            var result = 1
            result = prime * result + (if (name == null) 0 else name.hashCode)
            result
        }

        override def equals(obj: Any): Boolean = {
            obj match {
                case other: StringBasedObj =>
                    if (this == other) return true
                    if (other == null) return false
                    if (getClass != other.getClass) return false
                    if (name == null) {
                        if (other.name != null) return false
                    } else if (!name.equals(other.name)) return false
                    true
                case _ => false
            }
        }

        override def toString: String = name
    }
    
}