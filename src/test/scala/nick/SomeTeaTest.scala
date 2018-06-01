package nick

import java.util.concurrent.atomic.AtomicReference

import com.lineate.xonix.mind.model._
import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class SomeTeaTest extends FunSuite with Matchers {

  test("Various lists should be ok") {
    List() shouldBe Nil
    List(1, 2, 3) shouldNot be("empty")
    List("fee", "fie", "foe", "fum") should have length 4
  }

}
