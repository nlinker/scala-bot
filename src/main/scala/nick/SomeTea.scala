package nick

import com.lineate.xonix.mind.model._
import nick.Utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object Test extends App {
  import nick.Decartes.{X, Y, P}

  val x = X(0)
  val y = Y(2)
  println(Point.of(x.x + 1, y.y + 2))
  println(P(x, y))
  val np = P(x, y) match {
    case P(a, b) ⇒ println(s"decoded ${a.x} and ${b.y}")
  }
}

class SomeTea extends Bot {

  val version = 1
  val random = new Random()
  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))

  val me = new mutable.ArrayBuffer[Point]()
  val all = new mutable.HashMap[Int, Seq[Point]]()

  // initialized on the first iteration
  var randomSeed = 0L
  var m = 0
  var n = 0
  var id = 0

  // updated on each iteration
  var iter = 0
  var field: Array[Array[Cell]] = Array()
  var curHead: Point = Point.of(0, 0)
  var lastHead: Point = Point.of(0, 0)
  var curMove: Option[Move] = None
  var lastMove: Option[Move] = None
  var path: Seq[Point] = Seq()

  def initStuff(gs: GameState): Unit = {
    if (iter == 0) {
      m = gs.cells.length
      n = gs.cells.head.length
      id = gs.botId
      randomSeed = seed(random)
    }
    iter += 1
    field = gs.cells
    lastHead = curHead
    curHead = gs.me.head().get()
  }

  override def getName: String =
    s"Some[T] v$version " + randomSeed + " " + iter + " " + showMove(curMove)

  override def move(gs: GameState): Move = {
    initStuff(gs)

    // val me = findAll(gs)(id)
    // if we were bitten, then reset the path
    if (distance(lastHead, curHead) > 1) {
      path = Seq()
    }

    val theMove = if (path.nonEmpty) {
      val newHead = path.head
      path = path.tail
      direction(curHead, newHead)
    } else {
      // generate the new path
      val dst = findEmpty(field, 20).headOption
      if (dst.isDefined) {
        path = buildPath(curHead, dst.get, random.nextBoolean())
        findClosest(dst.get, borderOrOwned).foreach { border ⇒
          path = path ++ buildPath(dst.get, border, random.nextBoolean())
        }
        val newHead = path.head
        path = path.tail
        direction(curHead, newHead)
      } else {
        Move.STOP
      }
    }
    lastMove = curMove
    curMove = Some(theMove)

    theMove
  }

  def distance(src: Point, dst: Point): Int =
    Math.abs(dst.getRow - src.getRow) + Math.abs(dst.getCol - src.getCol)

  def borderOrOwned(p: Point): Boolean = {
    val ct = field(p.getRow)(p.getCol).getCellType
    ct == CellType.BORDER || ct == CellType.OWNED
  }

  def findClosest(origin: Point, predicate: Point ⇒ Boolean): Option[Point] = {
    val oi = origin.getRow
    val oj = origin.getCol
    for (r ← 1 to (m + n)) {
      for (k ← 0 until r) {
        val ps = Array(
          Point.of(oi - k, oj + r - k),
          Point.of(oi - r + k, oj - k),
          Point.of(oi + k, oj - r + k),
          Point.of(oi + r - k, oj + k)
        )
        val opt = ps.find(predicate)
        if (opt.isDefined)
          return opt
      }
    }
    None
  }

  def showMove(maybeMove: Option[Move]): String = {
    maybeMove match {
      case Some(Move.LEFT)  ⇒ "\uD83E\uDC50"
      case Some(Move.DOWN)  ⇒ "\uD83E\uDC53"
      case Some(Move.RIGHT) ⇒ "\uD83E\uDC52"
      case Some(Move.UP)    ⇒ "\uD83E\uDC51"
      case _                ⇒ ""
    }
  }

  def direction(src: Point, p: Point): Move = {
    val si = src.getRow
    val sj = src.getCol
    val di = p.getRow
    val dj = p.getCol
    if (di == si && dj <= sj) {
      Move.LEFT
    } else if (di == si && dj > sj) {
      Move.RIGHT
    } else if (di < si) {
      Move.UP
    } else {
      Move.DOWN
    }
  }

  // hf - horizontal first
  def buildPath(src: Point, dst: Point, horzFirst: Boolean): Seq[Point] = {
    val ord = implicitly[Ordering[Int]]
    val sj = src.getCol
    val si = src.getRow
    val dj = dst.getCol
    val di = dst.getRow
    // ← → ↑ ↓ ↖ ↗ ↘ ↙
    val ts = if (horzFirst)
      si.h(sj, dj) ++ dj.v(si, di) // do ← → then ↑ ↓
    else
      sj.v(si, di) ++ di.h(sj, dj) // do ↑ ↓ then ← →
    ts.map { case (i, j) ⇒ Point.of(i, j) }
  }

  def findEmpty(field: Array[Array[Cell]], np: Int): Seq[Point] = {
    val buf = new ArrayBuffer[Point]()
    for (k ← 0 until np) {
      val i = random.nextInt(m)
      val j = random.nextInt(n)
      field(i)(j).getCellType match {
        case CellType.EMPTY  ⇒ buf += Point.of(i, j)
        case CellType.BORDER ⇒
        case CellType.OWNED  ⇒
      }
    }
    buf
  }
}

object SomeTea {

  def main(args: Array[String]): Unit = {
    // TODO

  }
}
