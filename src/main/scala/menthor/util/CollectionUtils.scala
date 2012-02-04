package menthor.util

import scala.collection.parallel.ParIterable

/**
 * Provides utilities methods for working with collections
 * 
 * @author Stanislav Peshterliev
 */
object CollectionUtils {
  
  /**
   * Select top n elements form iterable collection with given ordering
   * 
   * @param n number of elements
   * @param iter iterable collection from which to select the top n elements
   * @param ord ordering of the elements
   * @return a new iterable collection that contains top n elements
   */
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