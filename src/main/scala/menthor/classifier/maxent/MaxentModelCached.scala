package menthor.classifier
package maxent

import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import scalala.tensor.sparse.SparseVector
import scala.collection.mutable.HashMap

class MaxentModelCached[C, S <: Sample](
		override val classes: List[C], 
		override val features: List[FeatureFunction[C, S]],
		override val parameters: Vector[Double]
		) extends MaxentModel[C, S](classes, features, parameters) {
  val cache = new HashMap[(C,S),SparseVector[Double]] 
  
  override def encode(cls: C, sample: S): SparseVector[Double] = {
    cache.get((cls, sample)) match {
      case Some(encoding) => encoding
      case None => {
        val encoding = super.encode(cls, sample)
        cache.put((cls,sample), encoding)
        encoding
      }
    }
  }
}