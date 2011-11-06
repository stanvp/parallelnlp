package menthor.maxent.util

object CollectionUtils {
  def cartesian[A](xs: Iterable[Iterable[A]]) =
    xs.map(x => x.map(y => List(y))).reduceLeft { (x, y) => x.flatMap { a => y.map(a ++ _) } }

  def merge[A](xs: Iterable[Map[A,Int]]): Map[A, Int] = {
    xs.foldLeft[Map[A, Int]](Map()) { (x, y) =>
      x ++ y.map { case (k, v) => k -> (v + x.getOrElse(k, 0)) }
    }
  }
}