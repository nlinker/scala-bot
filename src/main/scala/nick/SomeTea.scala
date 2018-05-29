package nick

import java.util.concurrent.ConcurrentHashMap

import com.lineate.xonix.mind.model._

import scala.collection.mutable
import scala.util.Random
import Utils._

import scala.collection.mutable.ArrayBuffer

class SomeTea extends Bot {

  val version = 1
  val random = new Random()

  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))
  var head: Point = Point.of(0, 0)
  var oldHead: Option[Point] = None
  var path: Option[Seq[Point]] = None
  var m = 0
  var n = 0
  var id = 0
  var iter = 0
  var shared = ""

  override def getName: String = s"Some[T] v$version " + seed(random) + " " + iter + " " + shared

  override def move(gs: GameStateView): Move = {
    val field = gs.field
    m = field.length
    n = field.head.length
    id = gs.botId
    iter += 1
    if (oldHead.isEmpty) {
      head = gs.head
    } else {
      oldHead = Some(gs.head)
      head = gs.head
    }

    val me = findAll(gs)

    if (path.isDefined) {
      val newHead = path.get.head
      path = Some(path.get.tail)
      return direction(newHead)
    } else {
      // generate the new path
      val empty = findEmpty(gs, 20).headOption
      if (empty.isDefined) {
        path = Some(corner(empty.get))
        val newHead = path.get.head
        path = Some(path.get.tail)
        return direction(newHead)
      }
    }

     Move.STOP
  }

  def direction(p: Point): Move = {
    val hi = head.getRow
    val hj = head.getCol
    val i = p.getRow
    val j = p.getCol
    if (i == hi && j <= hj) {
      Move.LEFT
    } else if (i == hi && j > hj) {
      Move.RIGHT
    } else if (i < hi) {
      Move.UP
    } else {
      Move.DOWN
    }
  }


  // generate corner path
  def corner(p: Point): Seq[Point] = {
    // we have a point, that is not a head, we need to path -> ^
    val hi = head.getRow
    val hj = head.getCol
    val i = p.getRow
    val j = p.getCol
    // find where our head relative to the point p
    val path = if (i <= hi && j <= hj) {
      val horz = (j to hj).reverse.map(Point.of(hi, _))
      val vert = (i to hi).reverse.map(Point.of(_, j))
      horz ++ vert
    } else if (i > hi && j <= hj) {
      val horz = (j to hj).reverse.map(Point.of(hi, _))
      val vert = (hi to i).map(Point.of(_, j))
      horz ++ vert
    } else if (i <= hi && j > hj) {
      val horz = (hj to j).map(Point.of(hi, _))
      val vert = (i to hi).reverse.map(Point.of(_, j))
      horz ++ vert
    } else {
      // (i > hi && j > hj)
      val horz = (hj to j).map(Point.of(hi, _))
      val vert = (i to hi).map(Point.of(_, j))
      horz ++ vert
    }
    path
  }

  def findEmpty(gs: GameStateView, np: Int): Seq[Point] = {
    val buf = new ArrayBuffer[Point]()
    for (k ← 0 until np) {
      val x = random.nextInt(n)
      val y = random.nextInt(m)
      gs.field(y)(x).getCellType match {
        case CellType.EMPTY  ⇒ buf += Point.of(x, y)
        case CellType.BORDER ⇒
        case CellType.OWNED  ⇒
        case CellType.TAIL   ⇒
      }
    }
    buf
  }

  def findAll(gs: GameStateView): Map[Int, Seq[Point]] = {
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


  def main(args: Array[String]): Unit = {
    Shared.obj
    val sh = Shared.underlying
    val x = sh.get()
    println(x)
  }
}
