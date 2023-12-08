package wpds.impl

import wpds.interfaces.Location
import wpds.interfaces.State
import wpds.interfaces.Weight

class PopRule[N <: Location, D <: State, W <: Weight](s1: D, l1: N, s2: D, w: W) 
  extends Rule[N, D, W](s1, l1, s2, null.asInstanceOf[N], w) {

  override def toString: String = s"<$s1;$l1>-><$s2>($w)"
}