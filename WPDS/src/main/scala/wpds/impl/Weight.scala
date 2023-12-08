package wpds.impl

abstract class Weight {
    def extendWith(other: Weight): Weight

    def combineWith(other: Weight): Weight
}

object Weight {
    val NO_WEIGHT_ONE: NoWeight = new NoWeight()

    class NoWeight extends Weight {
        override def extendWith(other: Weight): Weight = other

        override def combineWith(other: Weight): Weight = other

        override def toString: String = ""
    }
}