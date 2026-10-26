package synnks.intervalue

import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import spire.algebra.Order

class IntervalMapTestSuite extends ScalaCheckSuite {

  property("constant maps return their value at every key and on both tails") {
    forAll { (key: Int, value: Int) =>
      val map = IntervalMap.constant[Int, Int](value)

      assertEquals(map(key), value)
      assertEquals(map.at(key), value)
      assertEquals(map.below(key), value)
      assertEquals(map.above(key), value)
      assertEquals(map.belowAll, value)
      assertEquals(map.aboveAll, value)
    }
  }

  final private case class Key(value: String)
  private given Order[Key] = Order.by(_.value)

  final private case class Payload(value: String)

  property("constant maps support custom ordered keys without requiring value typeclasses") {
    forAll { (key: String, content: String) =>
      val value = Payload(content)
      val map   = IntervalMap.constant[Key, Payload](value)

      assertEquals(map(Key(key)), value)
      assertEquals(map.at(Key(key)), value)
      assertEquals(map.below(Key(key)), value)
      assertEquals(map.above(Key(key)), value)
      assertEquals(map.belowAll, value)
      assertEquals(map.aboveAll, value)
    }
  }

  property("constant maps preserve None as an ordinary value") {
    forAll { (key: Int) =>
      val map = IntervalMap.constant[Int, Option[String]](None)

      assertEquals(map(key), None)
      assertEquals(map.at(key), None)
      assertEquals(map.below(key), None)
      assertEquals(map.above(key), None)
      assertEquals(map.belowAll, None)
      assertEquals(map.aboveAll, None)
    }
  }

  property("constant maps cover the extreme keys of a bounded domain") {
    forAll(Gen.oneOf(Long.MinValue, Long.MaxValue), arbitrary[String]) { (key, value) =>
      val map = IntervalMap.constant[Long, String](value)

      assertEquals(map(key), value)
      assertEquals(map.at(key), value)
      assertEquals(map.below(key), value)
      assertEquals(map.above(key), value)
    }
  }
}
