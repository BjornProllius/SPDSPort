package wpds.interfaces

import wpds.impl.{Rule, Weight}

trait WPDSUpdateListener[N <: Location, D <: State, W <: Weight] {
    def onRuleAdded(rule: Rule[N, D, W]): Unit
}