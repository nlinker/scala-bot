package nick

import java.lang.Math.abs

import abc.SBot
import com.lineate.xonix.mind.model.Move._
import com.lineate.xonix.mind.model._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import nick.Decartes.{P, X, Y}
import nick.SomeTea._

import scala.collection.JavaConverters._
import scala.collection.mutable

class SomeTea(name: String, random: Random) extends Bot {

  // required by API
  def this() = this("", new Random())

  val version = 1

  // initialized on the first iteration
  var m: Y = Y(0)
  var n: X = X(0)
  var id = 0

  // updated on each iteration
  var iter = 0
  var gs: GameState = _
  var me: Seq[P] = Seq()
  var all: mutable.Buffer[Seq[P]] = mutable.Buffer()
  def meHead: P = me.last
  def cells(p: P): Cell = gs.cells(fromY(p.y))(fromX(p.x))
  def fromX(x: X): Int = x.x
  def fromY(y: Y): Int = m.y - 1 - y.y
  def from(p: P): Point = Point.of(m.y - 1 - p.y.y, p.x.x)
  def to(p: Point): P = P(X(p.getCol), Y(m.y - 1 - p.getRow))

  var curHead: P = P(X(0), Y(0))
  var lastHead: P = P(X(0), Y(0))
  var curMove: Option[Move] = None
  var path: Seq[P] = Seq()

  override def getName: String =
    s"Some[T] v$version " + iter + " " + showMove(curMove)

  override def move(gs: GameState): Move = {
    initStuff(gs)

    // val me = findAll(gs)(id)
    // if we were bitten, then reset the path
    if (distance(lastHead, curHead) > 1) {
      path = Seq()
    }
    // avoid dumb stuff
    if (path.size >= 2 && path.forall(!cells(_).isEmpty)) {
      path = Seq()
    }
    val theMove = if (path.nonEmpty) {
      val newHead = path.head
      path = path.tail
      direction(curHead, newHead)
    } else {
      // generate the new path
      val empties1 = findRandomEmpty(20).sortBy(distance(curHead, _))
      val empties =
        if (empties1.size >= 5) empties1.drop(4)
        else empties1
      if (empties.nonEmpty) {
        val dst = empties.head
        val dp = dst - curHead
        val run = buildPath(curHead, dst, dp.x.x < dp.y.y)
        path = if (run.size >= 2) {
          val q = run.last
          val r = run(run.size - 2)
          // detect where we go and search closest cell so that not to eat self
          val f = direction(r, q) match {
            case Move.RIGHT ⇒ (p: P) ⇒ !cells(p).isEmpty && q.x <= p.x
            case Move.UP    ⇒ (p: P) ⇒ !cells(p).isEmpty && q.y <= p.y
            case Move.LEFT  ⇒ (p: P) ⇒ !cells(p).isEmpty && p.x <= q.x
            case Move.DOWN  ⇒ (p: P) ⇒ !cells(p).isEmpty && p.y <= q.y
            case Move.STOP  ⇒ (p: P) ⇒ !cells(p).isEmpty && p != q
          }
          val fin0 = findClosest(m.y + n.x, dst, f)
          if (fin0.isDefined)
            run ++ buildPath(dst, fin0.get, dp.x.x < dp.y.y)
          else
            run
        } else {
          run
        }
        if (path.isEmpty) {
          STOP
        } else {
          // path has been updated
          val newHead = path.head
          path = path.tail
          direction(curHead, newHead)
        }
      } else {
        STOP
      }
    }
    curMove = Some(theMove)
    theMove
  }

  def initStuff(apiGs: GameState): Unit = {
    if (iter == 0) {
      m = Y(apiGs.cells.length)
      n = X(apiGs.cells.head.length)
      id = apiGs.botId
    }
    iter += 1
    gs = apiGs
    me = apiGs.me.getBody.asScala.map(to)
    all = new mutable.ArrayBuffer[Seq[P]](apiGs.others.size())
    for (i ← 0 until apiGs.others.size()) {
      if (i == id) {
        all += me.tail
      } else {
        all += apiGs.others.get(i).getIt.asScala.map(to)
      }
    }
    lastHead = curHead
    curHead = meHead
  }

  def findRandomEmpty(attempts: Int): Seq[P] = {
    findRandom(attempts, cells(_).isEmpty)
  }

  def findRandom(attempts: Int, predicate: P ⇒ Boolean): Seq[P] = {
    val buf = new ArrayBuffer[P]()
    for (_ ← 0 until attempts) {
      val x = X(random.nextInt(n.x))
      val y = Y(random.nextInt(m.y))
      val p = P(x, y)
      if (predicate(p))
        buf += p
    }
    buf
  }

  def findClosest(limit: Int, o: P, predicate: P ⇒ Boolean): Option[P] = {
    for (r ← 1 to limit) {
      for (k ← 0 until r) {
        val ps = Array(
          P(X(o.x.x - k), Y(o.y.y + r - k)).bound(n, m),
          P(X(o.x.x + k - r), Y(o.y.y - k)).bound(n, m),
          P(X(o.x.x + k), Y(o.y.y - r + k)).bound(n, m),
          P(X(o.x.x - k + r), Y(o.y.y + k)).bound(n, m)
        )
        val opt = ps.find(predicate)
        if (opt.isDefined)
          return opt
      }
    }
    None
  }


}

object SomeTea {

  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))
  val disamb: Array[Move] = Array(Move.RIGHT, Move.LEFT, Move.LEFT, Move.RIGHT)

  def showMove(maybeMove: Option[Move]): String = {
    // ← → ↑ ↓ ↖ ↗ ↘ ↙
    maybeMove match {
      case Some(LEFT)  ⇒ "\uD83E\uDC50"
      case Some(DOWN)  ⇒ "\uD83E\uDC53"
      case Some(RIGHT) ⇒ "\uD83E\uDC52"
      case Some(UP)    ⇒ "\uD83E\uDC51"
      case _           ⇒ ""
    }
  }

  def buildPath(s: P, d: P, horz: Boolean): Seq[P] = {
    if (horz)
      s.y.horz(s.x, d.x) ++ d.x.vert(s.y, d.y) // do ← → then ↑ ↓
    else
      s.x.vert(s.y, d.y) ++ d.y.horz(s.x, d.x) // do ↑ ↓ then ← →
  }

  def distance(p: P, q: P): Int = abs(p.x.x - q.x.x) + abs(p.y.y - q.y.y)

  def direction(src: P, dst: P): Move = {
    // quadrants
    //   2 1
    //   3 4
    // q1, disamb(0) is used when 0 < dx == dy
    // q2, disamb(1) is used when dx < 0, -dx == dy
    // q3, disamb(2) is used when dx < 0, dx == dy
    // q4, disamb(3) is used when 0 < dx == -dy
    direction(src, dst, disamb)
  }

  def direction(src: P, dst: P, disamb: Array[Move]): Move = {
    val dx = dst.x.x - src.x.x
    val dy = dst.y.y - src.y.y
    if (dx == 0 && dy == 0) {
      Move.STOP
    } else if (0 <= dx && 0 <= dy) {
      if (dx > dy) RIGHT
      else if (dx < dy) UP
      else disamb(0) // (dx == dy)
    } else if (dx <= 0 && 0 <= dy) {
      if (-dx > dy) LEFT
      else if (-dx < dy) UP
      else disamb(1) // (dx == dy)
    } else if (dx <= 0 && dy <= 0) {
      if (-dx > -dy) LEFT
      else if (-dx < -dy) DOWN
      else disamb(2) // (dx == dy)
    } else if (0 <= dx && dy <= 0) {
      if (dx > -dy) RIGHT
      else if (dx < -dy) DOWN
      else disamb(3) // (dx == dy)
    } else STOP
  }

  def main(args: Array[String]): Unit = {
    val random = new Random(123)
    val game = new Gameplay
    val bots = mutable.Buffer[Bot](new SomeTea("1", random), new SBot("2", random))
    //val bots = mutable.Buffer[Bot](new SBot("1", random), new SBot("2", random))
    val botNames = bots.map(_.getName).asJava
    val gs = game.createMatch(10, 20, bots.asJava, 100L, 0.9, 0).getGameState
    for (_ ← 0 until 100) {
      for (k ← bots.indices) {
        val cgs = game.getClientGameState(gs, k)
        val move = bots(k).move(cgs)
        game.step(gs, k, move)
        //println("move = " + move + " current game state = \n" + gs.toString)
        println(game.describeGameState(gs, botNames, false, false))
        //Thread.sleep(100)
      }
    }
  }
}
