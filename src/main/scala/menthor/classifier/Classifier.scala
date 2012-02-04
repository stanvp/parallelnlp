package menthor.classifier

import menthor.util.ProbabilityDistribution

/**
 * Abstract base class for all classifiers
 * 
 * @author Stanislav Peshterliev
 */
abstract class Classifier[C, S <: Sample] {
  /**
   * Given a sample, returns the class with highest probability
   * 
   * @param sample the object that we want to classify
   * @return tuple class, probability
   */
  def classify(sample: S) : (C,Double) = probClassify(sample).maxBy(_._2)
  
  /**
   * Given a sample, returns its class probability distribution
   * 
   * @param sample the object that we want to classify
   * @return sample's class probability distribution
   */
  def probClassify(sample: S): ProbabilityDistribution[C]
}