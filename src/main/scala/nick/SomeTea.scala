package nick

import com.lineate.xonix.mind.model._

import scala.collection.mutable
import scala.util.Random
import Utils._

object SomeTea extends Bot {

  val version = 1
  val random = new Random()

  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))
  var m = 0
  var n = 0
  var id = 0
  var iter = 0
  var shared = ""

  override def getName: String = s"Some(T) v$version " + seed(random) + " " + iter + " " + shared

  override def move(gs: GameStateView): Move = {
    m = gs.field.length
    n = gs.field.head.length
    id = gs.botId
    iter += 1
    if (id == 0) sharedPut(iter.toString)
    else shared = sharedGet()

    val me = findAll(gs)
    Move.STOP
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

}
