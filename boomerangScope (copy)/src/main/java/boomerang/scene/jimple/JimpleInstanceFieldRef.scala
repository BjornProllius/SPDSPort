package boomerang.scene.jimple

import boomerang.scene.Field
import boomerang.scene.InstanceFieldRef
import boomerang.scene.Val

class JimpleInstanceFieldRef(private var delegate: Nothing, private var m: Nothing) extends Nothing {
  def getBase = new Nothing(delegate.getBase, m)

  def getField = new Nothing(delegate.getField)
}