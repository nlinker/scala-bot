package nick

import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable
import scala.util.Random

object Utils {

  implicit class RichInt(it: Int) {
    def bound(l: Int, u: Int): Int = {
      if (it < l)  l
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

  def seed(random: Random): Long = {
    val seedF = random.self.getClass.getDeclaredField("seed")
    seedF.setAccessible(true)
    seedF.get(random.self).asInstanceOf[AtomicLong].get()
  }
}


