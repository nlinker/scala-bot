package nick

import io.estatico.newtype.macros.{newsubtype, newtype}

object Decartes {
  // make our own coordinate system, in the name of René Descartes
  // ^ y
  // |
  // |
  // |
  // +--------------> x
  @newsubtype case class X(x: Int)
  @newsubtype case class Y(y: Int)

  case class P(x: X, y: Y) {
    override def toString: String = s"(${x.x},${y.y})"
  }

}
