package synnks.intervalue

import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import spire.algebra.{ Eq, Order }
import spire.math.Rational

import scala.collection.immutable.TreeMap

class BreakpointsTestSuite extends ScalaCheckSuite {

  private val valueGenerator = for {
    at    <- arbitrary[Int]
    below <- arbitrary[Int]
  } yield ValueAtAndBelow(at, below)

  private val sampleGenerator = for {
    start  <- arbitrary[Int].map(Rational(_))
    step   <- Gen.choose(1, 100).map(Rational(_))
    before <- valueGenerator
    value  <- valueGenerator
    after  <- valueGenerator
  } yield (start, step, before, value, after)

  property("queries distinguish boundary points, intervening regions, and the upper tail") {
    forAll(sampleGenerator) { case (start, step, before, value, after) =>
      val key    = start + step
      val last   = key + step
      val points = Breakpoints(
        TreeMap(
          start -> before,
          key   -> value,
          last  -> after
        )(using Order[Rational].toOrdering)
      )

      assertEquals(points.at(start), Some(before.at))
      assertEquals(points.at(key), Some(value.at))
      assertEquals(points.at(last), Some(after.at))
      assertEquals(points.at(key - step / Rational(2)), Some(value.below))
      assertEquals(points.at(key + step / Rational(2)), Some(after.below))
      assertEquals(points.at(start - step), Some(before.below))
      assertEquals(points.at(last + step), None)
      assertEquals(points.below(start), Some(before.below))
      assertEquals(points.below(key), Some(value.below))
      assertEquals(points.below(last), Some(after.below))
      assertEquals(points.below(key - step / Rational(2)), Some(value.below))
      assertEquals(points.below(key + step / Rational(2)), Some(after.below))
      assertEquals(points.below(last + step), None)
      assertEquals(points.above(start - step), Some(before.below))
      assertEquals(points.above(start), Some(value.below))
      assertEquals(points.above(key), Some(after.below))
      assertEquals(points.above(key - step / Rational(2)), Some(value.below))
      assertEquals(points.above(key + step / Rational(2)), Some(after.below))
      assertEquals(points.above(last), None)
      assertEquals(points.headOption, Some(start -> before))
    }
  }

  property("empty breakpoints leave every query to the map's tail value") {
    forAll { (key: Int) =>
      val points = Breakpoints.empty[Int, Int]

      assertEquals(points.at(key), None)
      assertEquals(points.below(key), None)
      assertEquals(points.above(key), None)
      assertEquals(points.headOption, None)
    }
  }

  property("boundary queries use the supplied key equality and order") {
    given Order[String] = Order.from((left, right) => left.compareToIgnoreCase(right))

    forAll(Gen.alphaLowerStr, valueGenerator, valueGenerator) { (stem, first, last) =>
      val lower  = stem + "a"
      val upper  = stem + "z"
      val points = Breakpoints(
        TreeMap(
          lower -> first,
          upper -> last
        )(using Order[String].toOrdering)
      )
      val key    = lower.toUpperCase(java.util.Locale.ROOT)

      assertEquals(points.at(key), Some(first.at))
      assertEquals(points.below(key), Some(first.below))
      assertEquals(points.above(key), Some(last.below))
      assertEquals(points.at(upper.toUpperCase(java.util.Locale.ROOT)), Some(last.at))
      assertEquals(points.above(upper.toUpperCase(java.util.Locale.ROOT)), None)
    }
  }

  property("boundary constancy and equality use the supplied value equality") {
    final case class Value(amount: Int, note: String)
    given Eq[Value] = Eq.by(_.amount)

    forAll { (at: Int, below: Int, firstNote: String, secondNote: String) =>
      val first  = ValueAtAndBelow(Value(at, firstNote), Value(below, secondNote))
      val second = ValueAtAndBelow(Value(at, secondNote), Value(below, firstNote))
      val eq     = Eq[ValueAtAndBelow[Value]]

      assertEquals(first.isConstant, at == below)
      assert(eq.eqv(first, second))
      assert(!eq.eqv(first, ValueAtAndBelow(Value(at + 1, firstNote), Value(below, secondNote))))
      assert(!eq.eqv(first, ValueAtAndBelow(Value(at, firstNote), Value(below + 1, secondNote))))
    }
  }
}
