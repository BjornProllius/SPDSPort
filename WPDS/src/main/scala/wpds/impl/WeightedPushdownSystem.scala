package wpds.impl

import scala.collection.mutable.{HashSet, ListBuffer}
import wpds.interfaces.{IPushdownSystem, Location, State, WPDSUpdateListener}
import wpds.wildcard.Wildcard




class WeightedPushdownSystem[N <: Location, D <: State, W <: Weight] extends IPushdownSystem[N, D, W] {
    protected val pushRules: HashSet[PushRule[N, D, W]] = HashSet()
    protected val popRules: HashSet[PopRule[N, D, W]] = HashSet()
    protected val normalRules: HashSet[NormalRule[N, D, W]] = HashSet()
    protected val listeners: HashSet[WPDSUpdateListener[N, D, W]] = HashSet()

    override def addRule(rule: Rule[N, D, W]): Boolean = {
        if (addRuleInternal(rule)) {
            listeners.foreach(_.onRuleAdded(rule))
            true
        } else false
    }

    private def addRuleInternal(rule: Rule[N, D, W]): Boolean = rule match {
        case r: PushRule[N, D, W] => pushRules.add(r)
        case r: PopRule[N, D, W] => popRules.add(r)
        case r: NormalRule[N, D, W] => normalRules.add(r)
        case _ => throw new RuntimeException("Try to add a rule of wrong type")
    }

    def registerUpdateListener(listener: WPDSUpdateListener[N, D, W]): Unit = {
        if (listeners.add(listener)) {
            getAllRules.foreach(listener.onRuleAdded)
        }
    }

    override def getNormalRules: Set[NormalRule[N, D, W]] = normalRules.toSet

    override def getPopRules: Set[PopRule[N, D, W]] = popRules.toSet

    override def getPushRules: Set[PushRule[N, D, W]] = pushRules.toSet

    override def getAllRules: Set[Rule[N, D, W]] = {
        val rules = HashSet[Rule[N, D, W]]()
        rules ++= normalRules
        rules ++= popRules
        rules ++= pushRules
        rules.toSet
    }

    override def getRulesStarting(start: D, string: N): Set[Rule[N, D, W]] = {
        val result = HashSet[Rule[N, D, W]]()
        getRulesStartingWithinSet(start, string, popRules, result)
        getRulesStartingWithinSet(start, string, normalRules, result)
        getRulesStartingWithinSet(start, string, pushRules, result)
        result.toSet
    }

    private def getRulesStartingWithinSet(start: D, string: N, rules: Set[Rule[N, D, W]], res: HashSet[Rule[N, D, W]]): Unit = {
        for (r <- rules) {
            if (r.getS1 == start && (r.getL1 == string || r.getL1.isInstanceOf[Wildcard]))
                res += r
            if (string.isInstanceOf[Wildcard] && r.getS1 == start) {
                res += r
            }
        }
    }

    override def getNormalRulesEnding(start: D, string: N): Set[NormalRule[N, D, W]] = {
        val allRules = getNormalRules
        val result = HashSet[NormalRule[N, D, W]]()
        for (r <- allRules) {
            if (r.getS2 == start && r.getL2 == string) result += r
        }
        result.toSet
    }

    override def getPushRulesEnding(start: D, string: N): Set[PushRule[N, D, W]] = {
        val allRules = getPushRules
        val result = HashSet[PushRule[N, D, W]]()
        for (r <- allRules) {
            if (r.getS2 == start && r.getL2 == string) result += r
        }
        result.toSet
    }

    override def getStates: Set[D] = {
        val states = HashSet[D]()
        for (r <- getAllRules) {
            states += r.getS1
            states += r.getS2
        }
        states.toSet
    }

    override def poststar(initialAutomaton: WeightedPAutomaton[N, D, W], summaries: NestedWeightedPAutomatons[N, D, W]): Unit = {
        new PostStar[N, D, W] {
            override def putSummaryAutomaton(target: D, aut: WeightedPAutomaton[N, D, W]): Unit = {
                summaries.putSummaryAutomaton(target, aut)
            }

            override def getSummaryAutomaton(target: D): WeightedPAutomaton[N, D, W] = {
                summaries.getSummaryAutomaton(target)
            }
        }.poststar(this, initialAutomaton)
    }


    override def poststar(initialAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
        new PostStar[N, D, W] {
            override def putSummaryAutomaton(target: D, aut: WeightedPAutomaton[N, D, W]): Unit = {}

            override def getSummaryAutomaton(target: D): WeightedPAutomaton[N, D, W] = {
                initialAutomaton
            }
        }.poststar(this, initialAutomaton)
    }

    override def prestar(initialAutomaton: WeightedPAutomaton[N, D, W]): Unit = {
        new PreStar[N, D, W].prestar(this, initialAutomaton)
    }

    override def toString: String = {
        val s = new StringBuilder
        s.append("WPDS (#Rules: ").append(getAllRules.size).append(")\n")
        s.append("\tNormalRules:\n\t\t").append(normalRules.mkString("\n\t\t")).append("\n")
        s.append("\tPopRules:\n\t\t").append(popRules.mkString("\n\t\t")).append("\n")
        s.append("\tPushRules:\n\t\t").append(pushRules.mkString("\n\t\t"))
        s.toString
    }

    override def unregisterAllListeners(): Unit = {
        listeners.clear()
    }

}
