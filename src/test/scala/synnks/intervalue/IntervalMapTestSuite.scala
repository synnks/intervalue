package synnks.intervalue

import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

class IntervalMapTestSuite extends ScalaCheckSuite {

  property("constant maps return their value at every key and on both tails") {
    forAll { (key: Int, value: Int) =>
      val map = IntervalMap.constant[Int, Int](value)

      assertEquals(map(key), value)
      assertEquals(map.at(key), value)
      assertEquals(map.belowAll, value)
      assertEquals(map.aboveAll, value)
    }
  }

  final private case class Key(value: String)
  private given Ordering[Key] = Ordering.by(_.value)

  final private case class Payload(value: String)

  property("constant maps support custom ordered keys and values without an algebra") {
    forAll { (key: String, content: String) =>
      val value = Payload(content)
      val map   = IntervalMap.constant[Key, Payload](value)

      assertEquals(map(Key(key)), value)
      assertEquals(map.at(Key(key)), value)
      assertEquals(map.belowAll, value)
      assertEquals(map.aboveAll, value)
    }
  }

  test("constant maps preserve None as an ordinary value") {
    val map = IntervalMap.constant[Int, Option[String]](None)

    assertEquals(map(0), None)
    assertEquals(map.at(0), None)
    assertEquals(map.belowAll, None)
    assertEquals(map.aboveAll, None)
  }

  test("constant maps cover the extreme keys of a bounded domain") {
    val map = IntervalMap.constant[Long, String]("constant")

    List(Long.MinValue, -1L, 0L, 1L, Long.MaxValue).foreach { key =>
      assertEquals(map(key), "constant")
      assertEquals(map.at(key), "constant")
    }
  }
}
