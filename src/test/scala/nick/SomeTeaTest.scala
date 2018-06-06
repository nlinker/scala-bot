package nick

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class SomeTeaTest extends FunSuite with Matchers {

  test("Various lists should be ok") {
    List() shouldBe Nil
    List(1, 2, 3) shouldNot be("empty")
    List("fee", "fie", "foe", "fum") should have length 4
  }

}
