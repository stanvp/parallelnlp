package menthor.maxent

abstract class FeatureGenerator[S,C] {
  var classes : List[C] = _
  var features: List[(S, C) => Double] = _
  var C : Double = _
  
  def generate(s: S, c: C) = features.map(f => f(s, c))
  def size() : Int = features.size
  def train(classes: List[C], samples: List[(C,S)])
  def load() { }
}