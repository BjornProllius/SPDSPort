package boomerang.scene

import boomerang.scene.jimple.JimpleDeclaredMethod
import boomerang.scene.jimple.JimpleMethod
import com.google.common.base.Predicate
import com.google.common.collect.Sets
import java.util
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import soot.MethodOrMethodContext
import soot.Scene
import soot.SootClass
import soot.SootMethod
import soot.util.queue.QueueReader

object SootDataFlowScope {
  private val LOGGER = LoggerFactory.getLogger(classOf[SootDataFlowScope])
  private val HASH_CODE_SUB_SIG = "int hashCode()"
  private val TO_STRING_SUB_SIG = "java.lang.String toString()"
  private val EQUALS_SUB_SIG = "boolean equals(java.lang.Object)"
  private val CLONE_SIG = "java.lang.Object clone()"
  var classFilters: Array[Nothing] = null
  var methodFilters: Array[Nothing] = null

  /**
   * Default data-flow scope that only excludes phantom and native methods.
   *
   * @param scene
   * @return
   */
  def make(scene: Nothing): Nothing = {
    reset()
    new Nothing() {
      @Override def isExcluded(method: Nothing): Boolean = {
        val m = method.asInstanceOf[Nothing]
        m.getDeclaringClass.getDelegate.asInstanceOf[Nothing].isPhantom || m.isNative
      }

      def isExcluded(method: Nothing): Boolean = {
        val m = method.asInstanceOf[Nothing]
        m.getDeclaringClass.getDelegate.asInstanceOf[Nothing].isPhantom || m.isNative
      }
    }
  }

  /**
   * Excludes hashCode, toString, equals methods and the implementors of java.util.Collection,
   * java.util.Maps and com.google.common.collect.Multimap
   */
  def excludeComplex(scene: Nothing): Nothing = {
    reset()
    new Nothing() {
      @Override def isExcluded(method: Nothing): Boolean = {
        val m = method.asInstanceOf[Nothing]
        for (f <- classFilters) {
          if (f.apply(m.getDeclaringClass.getDelegate.asInstanceOf[Nothing])) return true
        }
        for (f <- methodFilters) {
          if (f.apply(m.getDelegate.asInstanceOf[Nothing])) return true
        }
        m.getDeclaringClass.getDelegate.asInstanceOf[Nothing].isPhantom || m.isNative
      }

      def isExcluded(method: Nothing): Boolean = {
        val m = method.asInstanceOf[Nothing]
        for (f <- classFilters) {
          if (f.apply(m.getDeclaringClass.getDelegate.asInstanceOf[Nothing])) return true
        }
        for (f <- methodFilters) {
          if (f.apply(m.getDelegate)) return true
        }
        m.getDeclaringClass.getDelegate.asInstanceOf[Nothing].isPhantom || m.isNative
      }
    }
  }

  private object MapFilter {
    private val MAP = "java.util.Map"
    private val GUAVA_MAP = "com.google.common.collect.Multimap"
  }

  private class MapFilter extends Nothing {
    val mapSubClasses: Nothing = Scene.v.getActiveHierarchy.getImplementersOf(Scene.v.getSootClass(MapFilter.MAP))
    excludes.add(Scene.v.getSootClass(MapFilter.MAP))
    excludes.addAll(mapSubClasses)
    if (Scene.v.containsClass(MapFilter.GUAVA_MAP)) {
      val c = Scene.v.getSootClass(MapFilter.GUAVA_MAP)
      if (c.isInterface) excludes.addAll(Scene.v.getActiveHierarchy.getImplementersOf(c))
    }

    import scala.collection.JavaConversions._

    for (c <- Scene.v.getClasses) {
      if (c.hasOuterClass && excludes.contains(c.getOuterClass)) excludes.add(c)
    }
    if (excludes.isEmpty) LOGGER.debug("Excludes empty for {}", MapFilter.MAP)
    private val excludes = Sets.newHashSet

    @Override def apply(c: Nothing): Boolean = excludes.contains(c)
  }

  private object IterableFilter {
    private val ITERABLE = "java.lang.Iterable"
  }

  private class IterableFilter extends Nothing {
    val iterableSubClasses: Nothing = Scene.v.getActiveHierarchy.getImplementersOf(Scene.v.getSootClass(IterableFilter.ITERABLE))
    excludes.addAll(iterableSubClasses)

    import scala.collection.JavaConversions._

    for (c <- Scene.v.getClasses) {
      if (c.hasOuterClass && excludes.contains(c.getOuterClass)) excludes.add(c)
    }
    if (excludes.isEmpty) LOGGER.debug("Excludes empty for {}", IterableFilter.ITERABLE)
    private val excludes = Sets.newHashSet

    @Override def apply(c: Nothing): Boolean = excludes.contains(c)
  }

  private class SubSignatureFilter(subSig: Nothing) extends Nothing {
    val l: Nothing = Scene.v.getReachableMethods.listener
    while (listener.hasNext) {
      val m = listener.next.method
      if (m.getSubSignature.equals(subSig)) excludes.add(m)
    }
    if (excludes.isEmpty) LOGGER.debug("Excludes empty for {}", subSig)
    private val excludes = Sets.newHashSet

    @Override def apply(m: Nothing): Boolean = excludes.contains(m)
  }

  private def reset(): Unit = {
    classFilters = Array[Nothing](new SootDataFlowScope.MapFilter, new SootDataFlowScope.IterableFilter)
    methodFilters = Array[Nothing](new SootDataFlowScope.SubSignatureFilter(HASH_CODE_SUB_SIG), new SootDataFlowScope.SubSignatureFilter(TO_STRING_SUB_SIG), new SootDataFlowScope.SubSignatureFilter(EQUALS_SUB_SIG), new SootDataFlowScope.SubSignatureFilter(CLONE_SIG))
  }
}