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

  //test("game state test") {
  //  val id = 0
  //  val head = Point.of(0, 0)
  //  val gsv =
  //    """
  //      |*.*.*.*.*.*.*.
  //      |*. A . . . .*.
  //      |*. . . . . .*.
  //      |*. . . . . .*.
  //      |*.*.*.*.*.*B*.
  //    """.stripMargin.gsv(0)
  //  val bot = new SomeTea
  //  val path = bot.buildPath(Point.of(0, 0), Point.of(2, 3), horzFirst = true)
  //  println(path)
  //}

  //implicit class CellsConv(s: String) {
  //  def gsv(botId: Int): GameStateView = {
  //    val gs = parseString(s)
  //    val m = gs.getField.getHeight
  //    val n = gs.getField.getWidth
  //    val arr = Array.ofDim[Cell](m, n)
  //    for (i ← 0 until m) {
  //      for (j ← 0 until n) {
  //        arr(i)(j) = gs.getField.getCells.get(Point.of(i, j))
  //      }
  //    }
  //    val head = gs.getHeads.get(botId).getPoint
  //    new GameStateView(botId, head, arr)
  //  }
  //
  //  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))
  //
  //  def parseString(str: String): GameState = {
  //    // detect sizes
  //    val lines = str.lines.filter { it => !it.trim().isEmpty }.toBuffer
  //    val m = lines.size
  //    val n = lines.map(it ⇒ it.length / 2).max
  //    val layer0 = Array.ofDim[Char](m, n)
  //    val layer1 = Array.ofDim[Char](m, n)
  //    for (i ← 0 until m) {
  //      for (j ← 0 until n * 2) {
  //        val c = lines(i)(j)
  //        if (j % 2 == 0)
  //          layer0(i)(j / 2) = c
  //        else
  //          layer1(i)(j / 2) = c
  //      }
  //    }
  //    // playersMap contained by a playerList
  //    val playersMap = mutable.Map[Int, mutable.Buffer[Point]]()
  //    for (i ← 0 until m) {
  //      for (j ← 0 until n) {
  //        val c = layer1(i)(j)
  //        if (c.isLetter && c.isUpper)
  //          playersMap(c - 'A') = mutable.Buffer(Point.of(i, j))
  //      }
  //    }
  //    val maxId = playersMap.keys.max
  //    for (i ← 0 to maxId) {
  //      if (playersMap(i) == null)
  //        playersMap(i) = mutable.Buffer[Point]()
  //    }
  //    // now playersMap is guaranteed to have size == number of players
  //    val cb = mutable.Buffer[(Point, Cell)]()
  //    for (i ← 0 until m) {
  //      for (j ← 0 until n) {
  //        val c = layer0(i)(j)
  //        val cell = if (c == '*') {
  //          Cell.border
  //        } else if (c.isDigit) {
  //          Cell.owned(c - '0')
  //        } else {
  //          Cell.empty()
  //        }
  //        cb += Point.of(i, j) → cell
  //      }
  //    }
  //    val cells = cb.toMap
  //
  //    def bool(x: Boolean) = if (x) 1 else 0
  //
  //    val filledCount = cells.values.map(x ⇒ bool(x != Cell.empty)).sum
  //    // now build player bodies = tails + heads
  //    // head is the last element of the corresponding list
  //    implicit class BoundInt(x: Int) {
  //      def bound(l: Int, u: Int): Int = {
  //        if (x < l) l
  //        else if (x > u) u
  //        else x
  //      }
  //    }
  //
  //    for ((key, body) <- playersMap) {
  //      if (body.nonEmpty) {
  //        // current point, start with head
  //        val cp = new AtomicReference[Option[Point]](body.headOption)
  //        val ct = 'a' + key // the player's tail char
  //        while (cp.get().isDefined) {
  //          // seek for lower letter around until not found
  //          val t = cp.get().get
  //          val nt = neigh.map { it ⇒
  //            val i = (t.getRow + it._1).bound(0, m - 1)
  //            val j = (t.getCol + it._2).bound(0, n - 1)
  //            Point.of(i, j)
  //          }.find { it ⇒
  //            !body.contains(it) && layer1(it.getRow)(it.getCol) == ct
  //          }
  //          if (nt.isDefined)
  //            body.insert(0, nt.get)
  //          cp.set(nt)
  //        }
  //      } else {
  //        //body is empty => do nothing, the player has been killed
  //      }
  //    }
  //
  //    import scala.collection.JavaConverters._
  //
  //    // initialize origins by the number of players
  //    // val np = playersMap.size
  //    // val o = makeOrigins(m, n, np)
  //    // val s = Stats(filledCount, 0, 0)
  //
  //    // mutable.Buffer[Head]()
  //    val f = new Field(m, n, cells.asJava)
  //    val bots = mutable.Buffer[Bot]().asJava
  //    val heads = playersMap.map { case (k, v) ⇒ Head.of(k, k, v.last) }.toBuffer.asJava
  //    GameState.of(f, bots, heads)
  //  }
  //}

}
