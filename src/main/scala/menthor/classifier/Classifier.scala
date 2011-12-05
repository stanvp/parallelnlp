package menthor.classifier

import menthor.util.ProbabilityDistribution

abstract class Classifier[C, S <: Sample] {
  def classify(sample: S) : (C,Double) = probClassify(sample).maxBy(_._2)
  def probClassify(sample: S): ProbabilityDistribution[C]
}