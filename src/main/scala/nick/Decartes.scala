package nick

import io.estatico.newtype.macros.newsubtype

import scala.collection.mutable

object Decartes {
  // make our own coordinate system, in the name of René Descartes
  // ^ y
  // |
  // |
  // +-------> x
  implicit val ordX: Ordering[X] = Ordering[X]
  implicit val ordY: Ordering[Y] = Ordering[Y]

  @newsubtype case class X(x: Int) {
    //def +(o: X) = X(x + o.x)
    //def -(o: X) = X(x - o.x)
    //def *(o: X) = X(x * o.x)
    //def /(o: X) = X(x / o.x)
    def <(o: X): Boolean = x < o.x
    def >(o: X): Boolean = x > o.x
    def <=(o: X): Boolean = x <= o.x
    def >=(o: X): Boolean = x >= o.x

    def horz(y1: Y, y2: Y)(implicit ord: Ordering[Y]): Seq[P] = {
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

    def vert(x1: X, x2: X)(implicit ord: Ordering[X]): Seq[P] = {
      if (ord.lt(x1, x2)) (x1.x + 1 to x2.x).map(x ⇒ P(X(x), this)).toBuffer
      else if (ord.lt(x2, x1)) (x2.x until x1.x).map(x ⇒ P(X(x), this)).reverse.toBuffer
      else mutable.Buffer()
    }
  }

  case class P(x: X, y: Y) {
    override def toString: String = s"(${x.x},${y.y})"
  }

}
