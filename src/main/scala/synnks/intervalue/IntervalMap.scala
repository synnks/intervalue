package synnks.intervalue

import spire.algebra.Order

/** An immutable piecewise-constant function over an ordered key domain. */
sealed trait IntervalMap[K, V] extends Serializable {

  /** Returns the value at `key`. */
  def apply(key: K): V

  /** Returns the value at `key`. */
  def at(key: K): V = apply(key)

  /** Returns the value on the open region immediately below `key` in the key ordering. */
  def below(key: K): V

  /** Returns the value on the open region immediately above `key` in the key ordering. */
  def above(key: K): V

  /** The value on the unbounded lower tail. */
  def belowAll: V

  /** The value on the unbounded upper tail. */
  def aboveAll: V
}

object IntervalMap {

  private case class Impl[K, V](aboveAll: V, breakpoints: Breakpoints[K, V]) extends IntervalMap[K, V] {

    override def apply(key: K): V = breakpoints.at(key) match {
      case Some(value) => value
      case None        => aboveAll
    }

    override def below(key: K): V = breakpoints.below(key).getOrElse(aboveAll)

    override def above(key: K): V = breakpoints.above(key).getOrElse(aboveAll)

    override def belowAll: V = breakpoints.headOption.map(_._2.below).getOrElse(aboveAll)
  }

  /** Creates a map that returns `value` for every key. */
  def constant[K: Order, V](value: V): IntervalMap[K, V] =
    Impl(value, Breakpoints.empty[K, V])
}
