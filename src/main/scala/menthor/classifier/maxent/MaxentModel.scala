package menthor.classifier
package maxent

import scalala.tensor.mutable.Vector
import scalala.operators.Implicits._
import scalala.tensor.sparse.SparseVector

class MaxentModel[C, S <: Sample](
  val classes: List[C],
  val featureshash: Map[Int, List[Feature]],
  val parameters: Vector[Double]) {
  
  def encode(cls: C, sample: S): SparseVector[Double] = {
    val encoding = SparseVector.zeros[Double](parameters.size)
    val clsIndex = classes.indexOf(cls) + 1
    
    for((features,i) <- featureshash.values.zipWithIndex) {
    	var value : Double = 0
	    for (feature <- features) {
	    	value += sample.features.getOrElse(feature, 0.0)
	    }
    	encoding(i * clsIndex) = value
    }
    
    encoding
  }
}