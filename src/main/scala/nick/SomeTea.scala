package nick

import java.util.Optional
import java.util.function.BiConsumer

import com.lineate.xonix.mind.domain.MatchDb
import com.lineate.xonix.mind.model._
import com.lineate.xonix.mind.repositories.{BotRepository, MatchRepository}
import com.lineate.xonix.mind.service._
import com.lineate.xonix.mind.service.repository.BotRepositoriesProvider
import nick.Utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class SomeTea extends Bot {

  val version = 1
  val random = new Random(987)
  val neigh = Seq(Pair(0, -1), Pair(-1, 0), Pair(0, 1), Pair(1, 0))

  val me = new mutable.ArrayBuffer[Point]()
  val all = new mutable.HashMap[Int, Seq[Point]]()

  var m = 0
  var n = 0
  var id = 0
  var iter = 0
  var head: Point = Point.of(0, 0)
  var lastHead: Option[Point] = None
  var path: Seq[Point] = Seq()
  var lastMove: Option[Move] = None
  var curMove: Option[Move] = None

  override def getName: String =
    s"Some[T] v$version " + seed(random) + " " + iter + " " + showMove(curMove)

  override def move(gs: GameStateView): Move = {
    initStuff(gs)

    val me = findAll(gs)(id)

    val theMove = if (path.nonEmpty) {
      val newHead = path.head
      path = path.tail
      direction(head, newHead)
    } else {
      // generate the new path
      val dst = findEmpty(gs, 20).headOption
      if (dst.isDefined) {
        path = buildPath(head, dst.get, horzFirst = true)
        val newHead = path.head
        path = path.tail
        direction(head, newHead)
      } else {
        Move.STOP
      }
    }
    lastMove = curMove
    curMove = Some(theMove)

    theMove
  }

  def findClosest(origin: Point, isAccessible: Point ⇒ Boolean): Option[Point] = {
    val oi = origin.getRow
    val oj = origin.getCol
    for (r ← 1 to (m + n)) {
      // diagonal points
      for (k ← 0 until r) {
        val p1 = Point.of(oi - k, oj + r - k)
        val p2 = Point.of(oi - r + k, oj - k)
        val p3 = Point.of(oi + k, oj - r + k)
        val p4 = Point.of(oi + r - k, oj + k)
        if (isAccessible(p1)) return Some(p1)
        else if (isAccessible(p2)) return Some(p2)
        else if (isAccessible(p3)) return Some(p3)
        else if (isAccessible(p4)) return Some(p4)
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

  def initStuff(gs: GameStateView): Unit = {
    if (iter == 0) {
      m = gs.field.length
      n = gs.field.head.length
      id = gs.botId
    } else {
      iter += 1
    }
    head = gs.head
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

  def findEmpty(gs: GameStateView, np: Int): Seq[Point] = {
    val buf = new ArrayBuffer[Point]()
    for (k ← 0 until np) {
      val i = random.nextInt(m)
      val j = random.nextInt(n)
      gs.field(i)(j).getCellType match {
        case CellType.EMPTY  ⇒ buf += Point.of(i, j)
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
}

object SomeTea {

  def main(args: Array[String]): Unit = {

    import scala.collection.JavaConverters._

    lazy val botRepository: BotRepository = null
    lazy val matchRepository: MatchRepository = null

    val bots = Array(new SomeTea: Bot, new SomeTea).toBuffer.asJava

    val botIds = (0 until bots.size()).map(x ⇒ java.lang.Integer.valueOf(x + 10)).asJava
    val botProvider: BotProvider = new BotRepositoriesProvider
    val botService: BotService = new BotServiceImpl(botProvider, botRepository)
    val matchService = new MatchServiceImpl(matchRepository, botService)
    val fieldService = new FieldServiceImpl
    val gsService = new GameStateServiceImpl
    val game = new GamePlayServiceImpl(botService, matchService, fieldService, gsService)
    val field = fieldService.create(8, 10)
    val heads = botService.createHeads(botIds, field)
    val matchDb = {
      val m = new MatchDb()
      m.setId(1)
      m.setDuration(100L)
      m.setPercent(90.0)
      m.setStatus(Status.New)
      m
    }

    val match0: Match = game.createMatch(heads, bots, field, matchDb)
    val logger: BiConsumer[Integer, GameState] = (_, gs) => {
      println(gsService.describeGameState(gs, true))
    }
    game.runMatch(match0, Optional.of(10L), logger)

  }
}
