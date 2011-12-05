package menthor.classifier
package maxent

import scalala.tensor.dense.DenseVector
import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._

class MaxentModel[C, S <: Sample](
		val classes: List[C], 
		val features: List[FeatureFunction[C, S]],
		val parameters: Vector[Double]
	) {
  def encode(cls: C, sample: S): Vector[Double] = DenseVector.tabulate[Double](features.size)(i => features(i)(cls, sample))
  
  def size: Int = features.size
}