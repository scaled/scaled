//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/scaled/scaled/blob/master/LICENSE

package scaled

object BoxUtil {

  def box (bs :Array[Boolean]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Boolean.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
  def box (bs :Array[Byte]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Byte.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
  def box (bs :Array[Char]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Character.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
  def box (bs :Array[Short]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Short.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
  def box (bs :Array[Int]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Integer.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
  def box (bs :Array[Long]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Long.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
  def box (bs :Array[Float]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Float.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
  def box (bs :Array[Double]) :Array[Any] = {
    val anys = new Array[Any](bs.length)
    var ii = 0 ; while (ii < bs.length) { anys(ii) = java.lang.Double.valueOf(bs(ii)) ; ii += 1 }
    anys
  }
}
