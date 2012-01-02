package menthor.classifier
package maxent

import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import scalala.tensor.sparse.SparseVector

class MaxentModel[C, S <: Sample](
		val classes: List[C], 
		val features: List[FeatureFunction[C, S]],
		val parameters: Vector[Double]
	) {
  def encode(cls: C, sample: S): SparseVector[Double] = SparseVector.tabulate[Double](features.size)(i => features(i)(cls, sample))
}