package wpds.impl

import wpds.interfaces.{Location, State}
import wpds.impl.Weight

class NormalRule[N <: Location, D <: State, W <: Weight](s1: D, l1: N, s2: D, l2: N, w: W) extends Rule[N, D, W](s1, l1, s2, l2, w) {

  override def toString: String = {
    "<" + s1 + ";" + l1 + ">-><" + s2 + ";" + l2 + ">" + (if (w.isInstanceOf[Weight.NoWeight]) "" else "(" + w + ")")
  }

  def canBeApplied(t: Transition[N, D], weight: W): Boolean = {
    true
  }
}