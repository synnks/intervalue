package synnks.intervalue

import spire.algebra.{ Eq, Order }
import spire.syntax.all.*

import scala.collection.immutable.TreeMap

private[intervalue] case class Breakpoints[K: Eq, V](values: TreeMap[K, ValueAtAndBelow[V]]) {

  def headOption: Option[(K, ValueAtAndBelow[V])] = Option.when(values.nonEmpty)(values.head)

  def at(k: K): Option[V] = values.minAfter(k).map {
    case (key, value) if k === key => value.at
    case (_, value)                => value.below
  }

  def above(k: K): Option[V] = values.rangeFrom(k).removed(k).headOption.map(_._2.below)

  def below(k: K): Option[V] = values.minAfter(k).map(_._2.below)
}

private[intervalue] object Breakpoints {

  def empty[K: Order, V]: Breakpoints[K, V] = {
    given Ordering[K] = Order[K].toOrdering

    Breakpoints(TreeMap.empty[K, ValueAtAndBelow[V]])
  }
}
