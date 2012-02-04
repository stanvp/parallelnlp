package menthor.classifier

/**
 * Generic trainer interface with a single method train
 * 
 * @author Stanislav Peshterliev 
 */
trait Trainer[C, S <: Sample] {
  
  /**
   * Trains a classifier
   * 
   * @param classes list of classes used for classification
   * @param samples iterable collection of training samples
   */
  def train(
    classes: List[C],
    samples: Iterable[(C, S)]): Classifier[C, S]
}