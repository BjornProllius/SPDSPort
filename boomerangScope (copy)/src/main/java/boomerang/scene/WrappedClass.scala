package boomerang.scene

import java.util

trait WrappedClass {
  def getMethods: Nothing

  def hasSuperclass: Boolean

  def getSuperclass: WrappedClass

  def getType: Nothing

  def isApplicationClass: Boolean

  def getFullyQualifiedName: Nothing

  def getName: Nothing

  def getDelegate: Nothing
}