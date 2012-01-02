package menthor.classifier
package maxent

import scala.collection.mutable.HashMap

class MaxentFeatureFunction[C, S <: Sample](features: List[Feature], cls: C) extends FeatureFunction[C, S] {
  override def apply(sampleCls: C, sample: S) : Double = {
    var value : Double = 0
    
    for (feature <- features) {
    	value += sample.features.getOrElse(feature, 0.0)
    }
    
    if (cls != sampleCls || value <= 0) {
      0.0
    } else {
      if (sample.total == 0) 0 else value / sample.total
    }
  }
  
  override def toString() = "MaxentFeatureFunction(%s, %s)".format(features, cls)
}