package nick

import scala.collection.mutable
import scala.reflect.ClassTag

// shared object for all bots
class Shared[T: ClassTag] {

  def put(obj: T): Unit = {
    val f = classOf[java.beans.Expression].getDeclaredField("unbound")
    f.setAccessible(true)
    f.set(null, obj)
  }

  def get(): Option[T] = {
    val ct = implicitly[ClassTag[T]]
    val f = classOf[java.beans.Expression].getDeclaredField("unbound")
    f.setAccessible(true)
    val v = f.get(null)
    if (ct.runtimeClass.isAssignableFrom(v.getClass))
      Some(v.asInstanceOf[T])
    else
      None
  }
}

object Shared {
  val underlying = new Shared[mutable.Map[Int, String]]()

  def obj: mutable.Map[Int, String] = {
    underlying.get() match {
      case Some(o) ⇒ o
      case None ⇒
        val mm = new mutable.HashMap[Int, String]()
        underlying.put(mm)
        mm
    }
  }
}
