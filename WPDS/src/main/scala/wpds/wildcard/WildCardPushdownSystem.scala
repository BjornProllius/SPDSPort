import wpds.wildcard._
import wpds.impl._
import wpds.interfaces._
import wpds.impl.Weight.NoWeight

import scala.collection.mutable.HashSet

abstract class WildcardPushdownSystem[N <: Location, D <: State] extends PushdownSystem[N, D] {

    override def getRulesStarting(start: D, string: N): Set[Rule[N, D, NoWeight]] = {
        assert(!string.equals(anyTransition))
        val allRules = getAllRules
        val result = new HashSet[Rule[N, D, NoWeight]]
        for (r <- allRules) {
            if (r.getS1 == start && r.getL1 == string) result += r
            if (anyTransition != null && r.getS1 == start && r.getL1 == anyTransition) {
                r match {
                    case _: NormalRule[N, D, NoWeight] => result += new UNormalRule[N, D](r.getS1, string, r.getS2, string)
                    case _: PopRule[N, D, NoWeight] => result += new UPopRule[N, D](r.getS1, string, r.getS2)
                    case _: PushRule[N, D, NoWeight] => result += new UPushRule[N, D](r.getS1, string, r.getS2, r.getL2, string)
                    case _ =>
                }
            }
        }
        result.toSet
    }

    override def getNormalRulesEnding(start: D, string: N): Set[NormalRule[N, D, NoWeight]] = {
        assert(!string.equals(anyTransition))
        val allRules = getNormalRules
        val result = new HashSet[NormalRule[N, D, NoWeight]]
        for (r <- allRules) {
            if (r.getS2 == start && r.getL2 == string) result += r
            if (r.getS2 == start && r.getL2 == anyTransition) {
                result += new UNormalRule[N, D](r.getS1, string, r.getS2, string)
            }
        }
        result.toSet
    }

    override def getPushRulesEnding(start: D, string: N): Set[PushRule[N, D, NoWeight]] = {
        assert(!string.equals(anyTransition))
        val allRules = getPushRules
        val result = new HashSet[PushRule[N, D, NoWeight]]
        for (r <- allRules) {
            if (r.getS2 == start && r.getL2 == string) result += r
        }
        result.toSet
    }

    def anyTransition: Wildcard
}