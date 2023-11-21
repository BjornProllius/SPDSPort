package test

import boomerang.scene.Statement

class ShouldNotBeAnalyzed private[test](var unit: Nothing) extends Nothing {
  private var isSatisfied = true
  private var isImprecise = false

  // TODO Melanie: Get calling method, atm this is always shouldNotBeAnalyzed
  def toString: Nothing = "Method should not be included in analysis: " + unit.toString

  @Override def isSatisfied: Boolean = isSatisfied

  @Override def isImprecise: Boolean = isImprecise

  def hasBeenAnalyzed(): Unit = {
    isSatisfied = false
    isImprecise = true
  }
}