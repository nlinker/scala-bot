package nick

import com.lineate.xonix.mind.model.Move._
import nick.Decartes.{P, X, Y}
import nick.SomeTea.{direction, disamb}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class SomeTeaTest extends FunSuite with Matchers {

  test("direction function is ok") {
    val o = P(X(0),Y(0))

    direction(o, P(X(2),  Y(2))) shouldBe disamb(0)

    direction(o, P(X(2),  Y(1))) shouldBe RIGHT
    direction(o, P(X(2),  Y(0))) shouldBe RIGHT
    direction(o, P(X(2), Y(-1))) shouldBe RIGHT

    direction(o, P(X(2), Y(-2))) shouldBe disamb(3)

    direction(o, P(X(1),  Y(2))) shouldBe UP
    direction(o, P(X(0),  Y(2))) shouldBe UP
    direction(o, P(X(-1), Y(2))) shouldBe UP

    direction(o, P(X(-2), Y(2))) shouldBe disamb(1)

    direction(o, P(X(-2),  Y(1))) shouldBe LEFT
    direction(o, P(X(-2),  Y(0))) shouldBe LEFT
    direction(o, P(X(-2), Y(-1))) shouldBe LEFT

    direction(o, P(X(-2), Y(-2))) shouldBe disamb(2)

    direction(o, P(X(-1), Y(-2))) shouldBe DOWN
    direction(o, P(X(0),  Y(-2))) shouldBe DOWN
    direction(o, P(X(1),  Y(-2))) shouldBe DOWN

    direction(o, P(X(0), Y(0))) shouldBe STOP
  }

}

