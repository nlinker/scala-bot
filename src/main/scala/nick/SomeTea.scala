package nick

import com.lineate.xonix.mind.model._
import nick.Utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

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
      m = gs.field.length
      n = gs.field.head.length
      id = gs.botId
      randomSeed = seed(random)
    }
    iter += 1
    field = gs.field
    lastHead = curHead
    curHead = gs.head
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
      case Some(Move.LEFT) ⇒ "\uD83E\uDC50"
      case Some(Move.DOWN) ⇒ "\uD83E\uDC53"
      case Some(Move.RIGHT) ⇒ "\uD83E\uDC52"
      case Some(Move.UP) ⇒ "\uD83E\uDC51"
      case _ ⇒ ""
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
        case CellType.EMPTY ⇒ buf += Point.of(i, j)
        case CellType.BORDER ⇒
        case CellType.OWNED ⇒
        case CellType.TAIL ⇒
      }
    }
    buf
  }

  def findAll(gs: GameState): Map[Int, Seq[Point]] = {
    val tail = Cell.tail(id)
    val me = mutable.Buffer(gs.head)
    // current point
    var cp = me.headOption
    while (cp.isDefined) {
      // seek for lower letter around until not found
      val p = cp.get
      val po = neigh.map { it ⇒
        val i = (p.getRow + it._1).bound(0, m - 1)
        val j = (p.getCol + it._2).bound(0, n - 1)
        Point.of(i, j)
      }.find { it ⇒
        !me.contains(it) && gs.field(it.getRow)(it.getCol) == tail
      }
      po.foreach { nt ⇒
        me.insert(0, nt)
      }
      cp = po
    }
    Map(id → me)
  }
}

object SomeTea {

  def main(args: Array[String]): Unit = {
    // TODO

  }
}
