package menthor.util

object CollectionUtils {
  def topNs[T](n: Int, xs: TraversableOnce[T], f : T => Double) = {
    var ss = List[T]()
    var min = Double.MaxValue
    var len = 0
    xs foreach { e =>
      if (len < n || f(e) > min) {
        ss = (e :: ss).sort((e1, e2) => (f(e1) compareTo f(e2)) > 0)
        min = f(ss.head)
        len += 1
      }
      if (len > n) {
        ss = ss.tail
        min = f(ss.head)
        len -= 1
      }
    }
    ss
  }
}