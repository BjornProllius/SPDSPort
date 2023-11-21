/**
 * ***************************************************************************** Copyright (c) 2018
 * Fraunhofer IEM, Paderborn, Germany. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0 which is available at
 * http://www.eclipse.org/legal/epl-2.0.
 *
 * <p>SPDX-License-Identifier: EPL-2.0
 *
 * <p>Contributors: Johannes Spaeth - initial API and implementation
 * *****************************************************************************
 */
package test.cases.hashmap

import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.util

object TreeNode {
  /** Returns x's Class if it is of the form "class C implements Comparable<C>", else null. */
  private[hashmap] def comparableClassFor(x: Nothing): Nothing = {
    if (x.isInstanceOf[Nothing]) {
      var c: Nothing = null
      var ts: Array[Nothing] = null
      var as: Array[Nothing] = null
      var t: Nothing = null
      var p: Nothing = null
      if ((c = x.getClass) eq classOf[Nothing]) return c // bypass checks
      if ((ts = c.getGenericInterfaces) != null) for (i <- 0 until ts.length) {
        if ((t = ts(i)).isInstanceOf[Nothing] && ((p = t.asInstanceOf[Nothing]).getRawType eq classOf[Nothing]) && (as = p.getActualTypeArguments) != null && as.length == 1 && (as(0) eq c)) return c // type arg is
        // c
      }
    }
    null
  }

  /** Returns k.compareTo(x) if x matches kc (k's screened comparable class), else 0. */
  @SuppressWarnings(Array("rawtypes", "unchecked")) // for cast to Comparable private[hashmap]  def compareComparables(kc: Nothing, k: Nothing, x: Nothing): Int =  { return (if (x == null || (x.getClass ne kc))  { 0}
  else
  {
    k.asInstanceOf[Nothing].compareTo(x)
  }
  )
}

/**
 * The bin count threshold for using a tree rather than list for a bin. Bins are converted to
 * trees when adding an element to a bin with at least this many nodes. The value must be greater
 * than 2 and should be at least 8 to mesh with assumptions in tree removal about conversion back
 * to plain bins upon shrinkage.
 */
private[hashmap] val TREEIFY_THRESHOLD = 8
/**
 * The bin count threshold for untreeifying a (split) bin during a resize operation. Should be
 * less than TREEIFY_THRESHOLD, and at most 6 to mesh with shrinkage detection under removal.
 */
private[hashmap] val UNTREEIFY_THRESHOLD = 6
/** Ensures that the given root is the first node of its bin. */
private[hashmap] def moveRootToFront[K, V](tab: Array[Nothing], root: TreeNode[K, V]): Unit = {
  var n = 0
  if (root != null && tab != null && (n = tab.length) > 0) {
    val index = (n - 1) & root.hash
    val first = tab(index).asInstanceOf[TreeNode[K, V]]
    if (root ne first) {
      var rn: Nothing = null
      tab(index) = root
      val rp = root.prev
      if ((rn = root.next) != null) rn.asInstanceOf[TreeNode[K, V]].prev = rp
      if (rp != null) rp.next = rn
      if (first != null) first.prev = root
      root.next = first
      root.prev = null
    }
    assert(checkInvariants(root))
  }
}
/**
 * Tie-breaking utility for ordering insertions when equal hashCodes and non-comparable. We don't
 * require a total order, just a consistent insertion rule to maintain equivalence across
 * rebalancings. Tie-breaking further than necessary simplifies testing a bit.
 */
private[hashmap] def tieBreakOrder(a: Nothing, b: Nothing) = {
  var d = 0
  if (a == null || b == null || (d = a.getClass.getName.compareTo(b.getClass.getName)) == 0) d = if (System.identityHashCode(a) <= System.identityHashCode(b)) -(1)
  else 1
  d
}
/* ------------------------------------------------------------ */
// Red-black tree methods, all adapted from CLR
private[hashmap] def rotateLeft[K, V](root: TreeNode[K, V], p: TreeNode[K, V]) = {
  var r: TreeNode[K, V] = null
  var pp: TreeNode[K, V] = null
  var rl: TreeNode[K, V] = null
  if (p != null && (r = p.right) != null) {
    if ((rl = p.right = r.left) != null) rl.parent = p
    if ((pp = r.parent = p.parent) == null) (root = r).red = false
    else if (pp.left eq p) pp.left = r
    else pp.right = r
    r.left = p
    p.parent = r
  }
  root
}
private[hashmap] def rotateRight[K, V](root: TreeNode[K, V], p: TreeNode[K, V]) = {
  var l: TreeNode[K, V] = null
  var pp: TreeNode[K, V] = null
  var lr: TreeNode[K, V] = null
  if (p != null && (l = p.left) != null) {
    if ((lr = p.left = l.right) != null) lr.parent = p
    if ((pp = l.parent = p.parent) == null) (root = l).red = false
    else if (pp.right eq p) pp.right = l
    else pp.left = l
    l.right = p
    p.parent = l
  }
  root
}
private[hashmap] def balanceInsertion[K, V](root: TreeNode[K, V], x: TreeNode[K, V]): TreeNode[K, V] = {
  x.red = true
  var xp: TreeNode[K, V] = null
  var xpp: TreeNode[K, V] = null
  var xppl: TreeNode[K, V] = null
  var xppr: TreeNode[K, V] = null
  while (true) {
    if ((xp = x.parent) == null) {
      x.red = false
      return x
    }
    else if (!xp.red || (xpp = xp.parent) == null) return root
    if (xp eq (xppl = xpp.left)) if ((xppr = xpp.right) != null && xppr.red) {
      xppr.red = false
      xp.red = false
      xpp.red = true
      x = xpp
    }
    else {
      if (x eq xp.right) {
        root = rotateLeft(root, x = xp)
        xpp = if ((xp = x.parent) == null) null
        else xp.parent
      }
      if (xp != null) {
        xp.red = false
        if (xpp != null) {
          xpp.red = true
          root = rotateRight(root, xpp)
        }
      }
    }
    else if (xppl != null && xppl.red) {
      xppl.red = false
      xp.red = false
      xpp.red = true
      x = xpp
    }
    else {
      if (x eq xp.left) {
        root = rotateRight(root, x = xp)
        xpp = if ((xp = x.parent) == null) null
        else xp.parent
      }
      if (xp != null) {
        xp.red = false
        if (xpp != null) {
          xpp.red = true
          root = rotateLeft(root, xpp)
        }
      }
    }
  }
}
private[hashmap] def balanceDeletion[K, V](root: TreeNode[K, V], x: TreeNode[K, V]): TreeNode[K, V] = {
  var xp: TreeNode[K, V] = null
  var xpl: TreeNode[K, V] = null
  var xpr: TreeNode[K, V] = null
  while (true) if (x == null || (x eq root)) return root
  else if ((xp = x.parent) == null) {
    x.red = false
    return x
  }
  else if (x.red) {
    x.red = false
    return root
  }
  else if ((xpl = xp.left) eq x) {
    if ((xpr = xp.right) != null && xpr.red) {
      xpr.red = false
      xp.red = true
      root = rotateLeft(root, xp)
      xpr = if ((xp = x.parent) == null) null
      else xp.right
    }
    if (xpr == null) x = xp
    else {
      val sl = xpr.left
      var sr = xpr.right
      if ((sr == null || !sr.red) && (sl == null || !sl.red)) {
        xpr.red = true
        x = xp
      }
      else {
        if (sr == null || !sr.red) {
          if (sl != null) sl.red = false
          xpr.red = true
          root = rotateRight(root, xpr)
          xpr = if ((xp = x.parent) == null) null
          else xp.right
        }
        if (xpr != null) {
          xpr.red = if (xp == null) false
          else xp.red
          if ((sr = xpr.right) != null) sr.red = false
        }
        if (xp != null) {
          xp.red = false
          root = rotateLeft(root, xp)
        }
        x = root
      }
    }
  }
  else { // symmetric
    if (xpl != null && xpl.red) {
      xpl.red = false
      xp.red = true
      root = rotateRight(root, xp)
      xpl = if ((xp = x.parent) == null) null
      else xp.left
    }
    if (xpl == null) x = xp
    else {
      var sl = xpl.left
      val sr = xpl.right
      if ((sl == null || !sl.red) && (sr == null || !sr.red)) {
        xpl.red = true
        x = xp
      }
      else {
        if (sl == null || !sl.red) {
          if (sr != null) sr.red = false
          xpl.red = true
          root = rotateLeft(root, xpl)
          xpl = if ((xp = x.parent) == null) null
          else xp.left
        }
        if (xpl != null) {
          xpl.red = if (xp == null) false
          else xp.red
          if ((sl = xpl.left) != null) sl.red = false
        }
        if (xp != null) {
          xp.red = false
          root = rotateRight(root, xp)
        }
        x = root
      }
    }
  }
}
/** Recursive invariant check */
private[hashmap] def checkInvariants[K, V](t: TreeNode[K, V]): Boolean = {
  val tp = t.parent
  val tl = t.left
  val tr = t.right
  val tb = t.prev
  val tn = t.next.asInstanceOf[TreeNode[K, V]]
  if (tb != null && (tb.next ne t)) return false
  if (tn != null && (tn.prev ne t)) return false
  if (tp != null && (t ne tp.left) && (t ne tp.right)) return false
  if (tl != null && ((tl.parent ne t) || tl.hash > t.hash)) return false
  if (tr != null && ((tr.parent ne t) || tr.hash < t.hash)) return false
  if (t.red && tl != null && tl.red && tr != null && tr.red) return false
  if (tl != null && !checkInvariants(tl)) return false
  if (tr != null && !checkInvariants(tr)) return false
  true
}
}

class TreeNode[K, V] private[hashmap](hash: Int, key: K, `val`: V, next: Nothing) extends Nothing(hash, key, `val`, next) {
  private[hashmap] val parent: TreeNode[K, V] = null // red-black tree links
  private[hashmap] val left: TreeNode[K, V] = null
  private[hashmap] val right: TreeNode[K, V] = null
  private[hashmap] val prev: TreeNode[K, V] = null // needed to unlink next upon deletion
  private[hashmap] val red = false

  /** Returns root of tree containing this node. */
  final private[hashmap] def root: TreeNode[K, V] = {
    var r = this
    var p: TreeNode[K, V] = null
    while (true) {
      if ((p = r.parent) == null) return r
      r = p
    }
  }

  /**
   * Finds the node starting at root p with the given hash and key. The kc argument caches
   * comparableClassFor(key) upon first use comparing keys.
   */
  final private[hashmap] def find(h: Int, k: Nothing, kc: Nothing): TreeNode[K, V] = {
    var p = this
    do {
      var ph = 0
      var dir = 0
      var pk: K = null
      val pl = p.left
      val pr = p.right
      var q: TreeNode[K, V] = null
      if ((ph = p.hash) > h) p = pl
      else if (ph < h) p = pr
      else if (((pk = p.key) eq k) || (k != null && k.equals(pk))) return p
      else if (pl == null) p = pr
      else if (pr == null) p = pl
      else if ((kc != null || (kc = TreeNode.comparableClassFor(k)) != null) && (dir = TreeNode.compareComparables(kc, k, pk)) != 0) p = if (dir < 0) pl
      else pr
      else if ((q = pr.find(h, k, kc)) != null) return q
      else p = pl
    } while (p != null)
    null
  }

  /** Calls find for root node. */
  final private[hashmap] def getTreeNode(h: Int, k: Nothing) = (if (parent != null) root
  else this).find(h, k, null)

  /**
   * Forms tree of the nodes linked from this node.
   *
   * @return root of tree
   */
  final private[hashmap] def treeify(tab: Array[Nothing]): Unit = {
    var root: TreeNode[K, V] = null
    var x = this
    var next: TreeNode[K, V] = null
    while (x != null) {
      next = x.next.asInstanceOf[TreeNode[K, V]]
      x.left = x.right = null
      if (root == null) {
        x.parent = null
        x.red = false
        root = x
      }
      else {
        val k = x.key
        val h = x.hash
        val kc: Nothing = null
        var p = root
        while (true) {
          var dir = 0
          var ph = 0
          val pk = p.key
          if ((ph = p.hash) > h) dir = -1
          else if (ph < h) dir = 1
          // else if ((kc == null &&
          // (kc = comparableClassFor(k)) == null) ||
          // (dir = compareComparables(kc, k, pk)) == 0)
          // dir = tieBreakOrder(k, pk);
          val xp = p
          if ((p = if (dir <= 0) p.left
          else p.right) == null) {
            x.parent = xp
            val v = 1
            if (dir <= 0) xp.left = x
            else xp.right = x
            // root = balanceInsertion(root, x);
            break //todo: break is not supported
          }
        }
      }
      x = next
    }
    // moveRootToFront(tab, root);
  }

  /** Returns a list of non-TreeNodes replacing those linked from this node. */
  final private[hashmap] def untreeify(map: Nothing) = {
    val hd: Nothing = null
    val tl: Nothing = null
    var q = this
    while (q != null) q = q.next
    hd
  }

  /** Tree version of putVal. */
  final private[hashmap] def putTreeVal(map: Nothing, tab: Array[Nothing], h: Int, k: K, v: V): TreeNode[K, V] = {
    var kc: Nothing = null
    var searched = false
    val root = if (parent != null) root
    else this
    var p = root
    while (true) {
      var dir = 0
      var ph = 0
      var pk: K = null
      if ((ph = p.hash) > h) dir = -1
      else if (ph < h) dir = 1
      else if (((pk = p.key) eq k) || (k != null && k.equals(pk))) return p
      else if ((kc == null && (kc = TreeNode.comparableClassFor(k)) == null) || (dir = TreeNode.compareComparables(kc, k, pk)) == 0) {
        if (!searched) {
          var q: TreeNode[K, V] = null
          var ch: TreeNode[K, V] = null
          searched = true
          if (((ch = p.left) != null && (q = ch.find(h, k, kc)) != null) || ((ch = p.right) != null && (q = ch.find(h, k, kc)) != null)) return q
        }
        dir = TreeNode.tieBreakOrder(k, pk)
      }
      val xp = p
      if ((p = if (dir <= 0) p.left
      else p.right) == null) {
        val xpn = xp.next
        val x: TreeNode[K, V] = null // ; map.newTreeNode(h, k, v, xpn);
        if (dir <= 0) xp.left = x
        else xp.right = x
        xp.next = x
        x.parent = x.prev = xp
        if (xpn != null) xpn.asInstanceOf[TreeNode[K, V]].prev = x
        TreeNode.moveRootToFront(tab, TreeNode.balanceInsertion(root, x))
        return null
      }
    }
  }

  /**
   * Removes the given node, that must be present before this call. This is messier than typical
   * red-black deletion code because we cannot swap the contents of an interior node with a leaf
   * successor that is pinned by "next" pointers that are accessible independently during traversal.
   * So instead we swap the tree linkages. If the current tree appears to have too few nodes, the
   * bin is converted back to a plain bin. (The test triggers somewhere between 2 and 6 nodes,
   * depending on tree structure).
   */
  final private[hashmap] def removeTreeNode(map: Nothing, tab: Array[Nothing], movable: Boolean): Unit = {
    var n = 0
    if (tab == null || (n = tab.length) == 0) return
    val index = (n - 1) & hash
    var first = tab(index).asInstanceOf[TreeNode[K, V]]
    var root = first
    var rl: TreeNode[K, V] = null
    val succ = next.asInstanceOf[TreeNode[K, V]]
    val pred = prev
    if (pred == null) tab(index) = first = succ
    else pred.next = succ
    if (succ != null) succ.prev = pred
    if (first == null) return
    if (root.parent != null) root = root.root
    if (root == null || root.right == null || (rl = root.left) == null || rl.left == null) {
      tab(index) = first.untreeify(map) // too small
      return
    }
    val p = this
    val pl = left
    val pr = right
    var replacement: TreeNode[K, V] = null
    if (pl != null && pr != null) {
      var s = pr
      var sl: TreeNode[K, V] = null
      while ((sl = s.left) != null) s = sl // find successor
      val c = s.red
      s.red = p.red
      p.red = c // swap colors
      val sr = s.right
      val pp = p.parent
      if (s eq pr) { // p was s's direct parent
        p.parent = s
        s.right = p
      }
      else {
        val sp = s.parent
        if ((p.parent = sp) != null) if (s eq sp.left) sp.left = p
        else sp.right = p
        if ((s.right = pr) != null) pr.parent = s
      }
      p.left = null
      if ((p.right = sr) != null) sr.parent = p
      if ((s.left = pl) != null) pl.parent = s
      if ((s.parent = pp) == null) root = s
      else if (p eq pp.left) pp.left = s
      else pp.right = s
      if (sr != null) replacement = sr
      else replacement = p
    }
    else if (pl != null) replacement = pl
    else if (pr != null) replacement = pr
    else replacement = p
    if (replacement ne p) {
      val pp = replacement.parent = p.parent
      if (pp == null) root = replacement
      else if (p eq pp.left) pp.left = replacement
      else pp.right = replacement
      p.left = p.right = p.parent = null
    }
    val r = if (p.red) root
    else TreeNode.balanceDeletion(root, replacement)
    if (replacement eq p) { // detach
      val pp = p.parent
      p.parent = null
      if (pp != null) if (p eq pp.left) pp.left = null
      else if (p eq pp.right) pp.right = null
    }
    if (movable) TreeNode.moveRootToFront(tab, r)
  }

  /**
   * Splits nodes in a tree bin into lower and upper tree bins, or untreeifies if now too small.
   * Called only from resize; see above discussion about split bits and indices.
   *
   * @param map   the map
   * @param tab   the table for recording bin heads
   * @param index the index of the table being split
   * @param bit   the bit of hash to split on
   */
  final private[hashmap] def split(map: Nothing, tab: Array[Nothing], index: Int, bit: Int): Unit = {
    val b = this
    // Relink into lo and hi lists, preserving order
    var loHead: TreeNode[K, V] = null
    var loTail: TreeNode[K, V] = null
    var hiHead: TreeNode[K, V] = null
    var hiTail: TreeNode[K, V] = null
    var lc = 0
    var hc = 0
    var e = b
    var next: TreeNode[K, V] = null
    while (e != null) {
      next = e.next.asInstanceOf[TreeNode[K, V]]
      e.next = null
      if ((e.hash & bit) eq 0) {
        if ((e.prev = loTail) == null) loHead = e
        else loTail.next = e
        loTail = e
        lc += 1
      }
      else {
        if ((e.prev = hiTail) == null) hiHead = e
        else hiTail.next = e
        hiTail = e
        hc += 1
      }
      e = next
    }
    if (loHead != null) if (lc <= TreeNode.UNTREEIFY_THRESHOLD) tab(index) = loHead.untreeify(map)
    else {
      tab(index) = loHead
      if (hiHead != null) loHead.treeify(tab) // (else is already treeified)
    }
    if (hiHead != null) if (hc <= TreeNode.UNTREEIFY_THRESHOLD) tab(index + bit) = hiHead.untreeify(map)
    else {
      tab(index + bit) = hiHead
      if (loHead != null) hiHead.treeify(tab)
    }
  }
}