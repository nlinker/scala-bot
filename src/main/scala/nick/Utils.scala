package nick

import java.util.concurrent.atomic.AtomicLong

import scala.util.Random

object Utils {
  def seed(random: Random): Long = {
    val seedF = random.self.getClass.getDeclaredField("seed")
    seedF.setAccessible(true)
    seedF.get(random.self).asInstanceOf[AtomicLong].get()
  }
}


