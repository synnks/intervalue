package synnks.intervalue

import spire.algebra.Eq
import spire.syntax.all.*

private[intervalue] case class ValueAtAndBelow[V: Eq](at: V, below: V) {
  def isConstant: Boolean = at === below
}

private[intervalue] object ValueAtAndBelow {
  given eq[V: Eq]: Eq[ValueAtAndBelow[V]] = Eq.and(Eq.by(_.at), Eq.by(_.below))
}
