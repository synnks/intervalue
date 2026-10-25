package synnks.intervalue

import scala.collection.immutable.TreeMap

/** An immutable piecewise-constant function over an ordered key domain. */
final class IntervalMap[K, V] private (
  val aboveAll: V,
  private val breakpoints: TreeMap[K, IntervalMap.ValueAtAndBelow[V]]
) {

  /** Returns the value at `key`. */
  def apply(key: K): V = breakpoints.minAfter(key) match {
    case Some((boundary, value)) if breakpoints.ordering.equiv(key, boundary) => value.at
    case Some((_, value))                                                     => value.below
    case None                                                                 => aboveAll
  }

  /** Returns the value at `key`. */
  def at(key: K): V = apply(key)

  /** The value on the unbounded lower tail. */
  def belowAll: V = breakpoints.headOption.map(_._2.below).getOrElse(aboveAll)
}

object IntervalMap {

  final private case class ValueAtAndBelow[V](at: V, below: V)

  /** Creates a map that returns `value` for every key. */
  def constant[K: Ordering, V](value: V): IntervalMap[K, V] =
    new IntervalMap(value, TreeMap.empty[K, ValueAtAndBelow[V]])
}
