package abc

import com.lineate.xonix.mind.model.Move._
import com.lineate.xonix.mind.model._

import scala.collection.mutable
import scala.util.Random
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class SBot(name: String, random: Random) extends Bot {

  // required by API
  def this() = this("", new Random())

  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))
  var m = 0
  var n = 0
  var id = 0

  var iter = 0
  var gs: GameState = _
  var curHead: Point = Point.of(0, 0)
  var lastHead: Point = Point.of(0, 0)
  var curMove: Option[Move] = None
  var lastMove: Option[Move] = None
  var path: Seq[Point] = Seq()
  var me: mutable.Buffer[Point] = mutable.Buffer()

  override def getName: String = s"SBbot!$name"

  override def move(gs: GameState): Move = {
    if (iter == 0) {
      m = gs.cells.length
      n = gs.cells.head.length
      id = gs.botId
    }
    this.gs = gs
    iter += 1
    me = gs.me.getBody.asScala
    lastHead = curHead
    curHead = me.last

    if (distance(lastHead, curHead) > 1) {
      path = Seq()
    }

    val theMove = if (path.nonEmpty) {
      val newHead = path.head
      path = path.tail
      direction(curHead, newHead)
    } else {
      // generate the new path
      val dstOpt = findEmpty(gs.cells, 20).headOption
      if (dstOpt.isDefined) {
        val dst = dstOpt.get
        val testDir = direction(curHead, dst)
        path = buildPath(curHead, dst, testDir == LEFT || testDir == RIGHT)
        val borderOpt = findClosest(dst, borderOrOwnedPartial(curHead, dst))
        if (borderOpt.isDefined) {
          val border = borderOpt.get
          path = path ++ buildPath(dst, border, random.nextBoolean())
        }
        val newHead = path.head
        path = path.tail
        direction(curHead, newHead)

      } else {
        STOP
      }
    }
    lastMove = curMove
    curMove = Some(theMove)
    theMove
  }

  def opposite(move: Move): Move = move match {
    case RIGHT ⇒ LEFT
    case UP    ⇒ DOWN
    case LEFT  ⇒ RIGHT
    case DOWN  ⇒ UP
    case STOP  ⇒ STOP
  }

  def rotate90(move: Move, clockwise: Boolean): Move = move match {
    case RIGHT ⇒ if (clockwise) DOWN else UP
    case UP    ⇒ if (clockwise) RIGHT else LEFT
    case LEFT  ⇒ if (clockwise) UP else DOWN
    case DOWN  ⇒ if (clockwise) LEFT else RIGHT
    case STOP  ⇒ STOP
  }


  def direction(src: Point, p: Point): Move = {
    val si = src.getRow
    val sj = src.getCol
    val di = p.getRow
    val dj = p.getCol
    if (di == si && dj <= sj) {
      LEFT
    } else if (di == si && dj > sj) {
      RIGHT
    } else if (di < si) {
      UP
    } else {
      DOWN
    }
  }

  def distance(src: Point, dst: Point): Int =
    Math.abs(dst.getRow - src.getRow) + Math.abs(dst.getCol - src.getCol)

  def borderOrOwnedPartial(o: Point, a: Point)(p: Point): Boolean = {
    val oi = o.getRow
    val oj = o.getCol
    val ai = a.getRow
    val aj = a.getCol
    def select(x: Point): Boolean = {
      val xi = x.getRow
      val xj = x.getCol
      // 4 3 2
      // 5 9 1
      // 6 7 8
      if (false) false
      else if (oi == ai && oj < aj ) aj <= xj
      else if (ai < oi  && oj < aj ) xi <= ai && aj <= xj
      else if (ai < oi  && oj == aj) xi <= ai
      else if (ai < oi  && aj < oj ) xi <= ai && xj <= aj
      else if (oi == ai && aj < oj ) xj <= aj
      else if (ai > oi  && aj < oj ) ai <= xi && xj <= aj
      else if (ai > oi  && oj == aj) ai <= xi
      else if (ai > oi  && oj < aj ) ai <= xi && aj <= xj
      else if (oi == ai && oj < aj ) aj <= xj
      else ai != xi && aj != xj
    }
    val cell = gs.cells(p.getRow)(p.getCol)
    val selected = select(p)
    (cell.isBorder || cell.isOwned) && selected
  }

  def buildPath(src: Point, dst: Point, horzFirst: Boolean): Seq[Point] = {
    val sj = src.getCol
    val si = src.getRow
    val dj = dst.getCol
    val di = dst.getRow
    val ts = if (horzFirst)
      si.h(sj, dj) ++ dj.v(si, di) // do ← → then ↑ ↓
    else
      sj.v(si, di) ++ di.h(sj, dj) // do ↑ ↓ then ← →
    ts.map { case (i, j) ⇒ Point.of(i, j) }
  }

  def findClosest(src: Point, predicate: Point ⇒ Boolean): Option[Point] = {
    val oi = src.getRow
    val oj = src.getCol
    for (r ← 1 to (m + n)) {
      for (k ← 0 until r) {
        val ps = Array(
          Point.of(oi - k, oj + r - k).bounded,
          Point.of(oi - r + k, oj - k).bounded,
          Point.of(oi + k, oj - r + k).bounded,
          Point.of(oi + r - k, oj + k).bounded
        )
        val opt = ps.find(predicate)
        if (opt.isDefined)
          return opt
      }
    }
    None
  }

  def findEmpty(cells: Array[Array[Cell]], np: Int): Seq[Point] = {
    val buf = new ArrayBuffer[Point]()
    for (k ← 0 until np) {
      val i = random.nextInt(m)
      val j = random.nextInt(n)
      cells(i)(j).getCellType match {
        case CellType.EMPTY  ⇒ buf += Point.of(i, j)
        case CellType.BORDER ⇒
        case CellType.OWNED  ⇒
      }
    }
    // sort them by distance
    buf.sortBy(p ⇒ distance(curHead, p))
  }

  implicit class RichPoint(it: Point) {
    def bounded: Point = {
      val i = it.getRow
      val j = it.getCol
      if (0 <= i && i < m && 0 <= j && j < n) it
      else Point.of(i.bound(0, m - 1), j.bound(0, n - 1))
    }
  }

  implicit class RichInt(it: Int) {
    def bound(l: Int, u: Int): Int = {
      if (it < l) l
      else if (it > u) u
      else it
    }

    def h(a: Int, b: Int): Seq[(Int, Int)] = {
      if (a < b) (a + 1 to b).map((it, _)).toBuffer
      else if (b < a) (b until a).map((it, _)).reverse.toBuffer
      else mutable.Buffer()
    }

    def v(a: Int, b: Int): Seq[(Int, Int)] = {
      if (a < b) (a + 1 to b).map((_, it)).toBuffer
      else if (b < a) (b until a).map((_, it)).reverse.toBuffer
      else mutable.Buffer()
    }
  }
}
