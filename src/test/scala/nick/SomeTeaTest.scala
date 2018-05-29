package nick

import com.lineate.xonix.mind.model.{Cell, GameStateView, Point}
import javax.swing.plaf.metal.MetalBorders
import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SomeTeaTest extends FunSuite with Matchers {

  test("Various lists should be ok") {
    List() shouldBe Nil
    List(1, 2, 3) shouldNot be ("empty")
    List("fee", "fie", "foe", "fum") should have length 4
  }

  test("game state test") {
    val id = 0
    val head = Point.of(0, 0)
    // ugly
    val cells = Array(
      Array(Cell.border()), Array(Cell.border()), Array(Cell.border()), Array(Cell.border()),
      Array(Cell.border()), Array(Cell.empty()), Array(Cell.empty()), Array(Cell.border()),
      Array(Cell.border()), Array(Cell.empty()), Array(Cell.empty()), Array(Cell.border()),
      Array(Cell.border()), Array(Cell.border()), Array(Cell.border()), Array(Cell.border())
    )
    val gsv = new GameStateView(id, head, cells)
    val bot = new SomeTea
    val path = bot.corner(Point.of(2, 2))
    println(path)
  }

  implicit class CellsConv(s: String) {
    def cells: Array[Array[Cell]] = {
      Array()
    }
  }

}
