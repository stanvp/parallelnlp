package menthor.classifier
package maxent

class MaxentFeatureFunction[C, S <: Sample](feature: Feature, cls: C) extends FeatureFunction[C, S] {
  override def apply(sampleCls: C, sample: S) : Double = {
    val value = sample.features.getOrElse(feature, 0.0)
    if (cls != sampleCls || value <= 0) {
      0.0
    } else {
      if (sample.total == 0) 0 else value / sample.total
    }
  }
  
  override def toString() = "MaxentFeatureFunction(%s, %s)".format(feature, cls)
}