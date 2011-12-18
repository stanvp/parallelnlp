package menthor.classifier

trait Trainer[C, S <: Sample] {
  def train(
    classes: List[C],
    samples: Iterable[(C, S)]): Classifier[C, S]
}