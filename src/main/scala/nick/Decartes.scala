package nick

import io.estatico.newtype.macros.newsubtype

import scala.collection.mutable

object Decartes {
  // make our own coordinate system, in the name of René Descartes
  // ^ y
  // |
  // |
  // +-------> x
  implicit val ordX: Ordering[X] = Ordering.by(e => e.x)
  implicit val ordY: Ordering[Y] = Ordering.by(e => e.y)

  @newsubtype case class X(x: Int) {
    //def +(o: X) = X(x + o.x)
    //def -(o: X) = X(x - o.x)
    //def *(o: X) = X(x * o.x)
    //def /(o: X) = X(x / o.x)
    def <(o: X): Boolean = x < o.x
    def >(o: X): Boolean = x > o.x
    def <=(o: X): Boolean = x <= o.x
    def >=(o: X): Boolean = x >= o.x

    def vert(y1: Y, y2: Y): Seq[P] = {
      val ord = implicitly[Ordering[Y]]
      if (ord.lt(y1, y2)) (y1.y + 1 to y2.y).map(y ⇒ P(this, Y(y))).toBuffer
      else if (ord.lt(y2, y1)) (y2.y until y1.y).map(y ⇒ P(this, Y(y))).reverse.toBuffer
      else mutable.Buffer()
    }
  }

  @newsubtype case class Y(y: Int) {
    //def +(o: Y) = Y(y + o.y)
    //def -(o: Y) = Y(y - o.y)
    //def *(o: Y) = Y(y * o.y)
    //def /(o: Y) = Y(y / o.y)
    def <(o: Y): Boolean = y < o.y
    def >(o: Y): Boolean = y > o.y
    def <=(o: Y): Boolean = y <= o.y
    def >=(o: Y): Boolean = y >= o.y

    def horz(x1: X, x2: X): Seq[P] = {
      val ord = implicitly[Ordering[X]]
      if (ord.lt(x1, x2)) (x1.x + 1 to x2.x).map(x ⇒ P(X(x), this)).toBuffer
      else if (ord.lt(x2, x1)) (x2.x until x1.x).map(x ⇒ P(X(x), this)).reverse.toBuffer
      else mutable.Buffer()
    }
  }

  case class P(x: X, y: Y) {
    def +(p: P) = P(X(x.x + p.x.x), Y(y.y + p.y.y))
    def -(p: P) = P(X(x.x - p.x.x), Y(y.y - p.y.y))
    def bound(n: X, m: Y): P = {
      if (X(0) <= x && x < n && Y(0) <= y && y < m) this
      else {
        val x1 = if (x < X(0)) X(0) else if (n <= x) X(n.x - 1) else x
        val y1 = if (y < Y(0)) Y(0) else if (m <= y) Y(m.y - 1) else y
        P(x1, y1)
      }
    }
    override def toString: String = s"(${x.x},${y.y})"
  }

}
