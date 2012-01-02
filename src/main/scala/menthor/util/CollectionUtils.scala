package menthor.util
import scala.collection.parallel.ParIterable

object CollectionUtils {
  def topNs[T](n: Int, xs: TraversableOnce[T], f: T => Double) = {
    var ss = List[T]()
    var min = Double.MaxValue
    var len = 0
    xs foreach { e =>
      if (len < n || f(e) > min) {
        ss = (e :: ss).sort((e1, e2) => (f(e1) compareTo f(e2)) > 0)
        min = f(ss.last)
        len += 1
      }
      if (len > n) {
        ss = ss.init
        min = f(ss.last)
        len -= 1
      }
    }
    ss
  }

  def topNs[T](n: Int, iter: ParIterable[T])(implicit ord: Ordering[T]): ParIterable[T] = {
    def partitionMax(acc: ParIterable[T], it: ParIterable[T]): ParIterable[T] = {
      val max = it.max(ord)
      val (nextElems, rest) = it.partition(ord.gteq(_, max))
      val maxElems = acc ++ nextElems
      if (maxElems.size >= n || rest.isEmpty) maxElems.take(n)
      else partitionMax(maxElems, rest)
    }
    if (iter.isEmpty) iter.take(0)
    else partitionMax(iter.take(0), iter)
  }
}