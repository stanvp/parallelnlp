package menthor.util
import scala.collection.parallel.ParIterable

object CollectionUtils {
  def topNs[T](n: Int, iter: Iterable[T])(implicit ord: Ordering[T]): Iterable[T] = {
    def partitionMax(acc: Iterable[T], it: Iterable[T]): Iterable[T] = {
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