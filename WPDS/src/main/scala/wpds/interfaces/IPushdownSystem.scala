package wpds.interfaces

import wpds.impl._

trait IPushdownSystem[N <: Location, D <: State, W <: Weight] {

    def addRule(rule: Rule[N, D, W]): Boolean

    def getStates: Set[D]

    def getNormalRules: Set[NormalRule[N, D, W]]

    def getPopRules: Set[PopRule[N, D, W]]

    def getPushRules: Set[PushRule[N, D, W]]

    def getAllRules: Set[Rule[N, D, W]]

    def getRulesStarting(start: D, string: N): Set[Rule[N, D, W]]

    def getNormalRulesEnding(start: D, string: N): Set[NormalRule[N, D, W]]

    def getPushRulesEnding(start: D, string: N): Set[PushRule[N, D, W]]

    def prestar(initialAutomaton: WeightedPAutomaton[N, D, W]): Unit

    def poststar(initialAutomaton: WeightedPAutomaton[N, D, W]): Unit

    def poststar(initialAutomaton: WeightedPAutomaton[N, D, W], summaries: NestedWeightedPAutomatons[N, D, W]): Unit

    def registerUpdateListener(listener: WPDSUpdateListener[N, D, W]): Unit

    def unregisterAllListeners(): Unit
}