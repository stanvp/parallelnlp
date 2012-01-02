package menthor.classifier
package maxent

import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import scalala.tensor.sparse.SparseVector
import scala.collection.mutable.HashMap

class MaxentModelCached[C, S <: Sample](model: MaxentModel[C, S]) extends MaxentModel[C, S](model.classes, model.featureshash, model.parameters) {
  val cache = new HashMap[(C, S), SparseVector[Double]]

  override def encode(cls: C, sample: S): SparseVector[Double] = {
    cache.get((cls, sample)) match {
      case Some(encoding) => encoding
      case None => {
        val encoding = super.encode(cls, sample)
        cache.put((cls, sample), encoding)
        encoding
      }
    }
  }
}