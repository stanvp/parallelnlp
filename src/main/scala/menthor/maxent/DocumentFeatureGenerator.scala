package menthor.maxent

import util.CollectionUtils._

class DocumentFeatureGenerator(stopWords: Set[String]) extends FeatureGenerator[Document, String] {

  def this() = this(Set())

  def featureFunction(word: String, category: String)(doc: Document, docCategory: String): Double = {
    if (category != docCategory || doc.termFrequency.getOrElse(word, 0) == 0) {
      0.0
    } else {
      1
    }
  }

  override def train(classes: List[String], samples: List[(String, Document)]) {
    this.classes = classes 

    val termFrequency = merge(samples.map(_._2.termFrequency))
    val topTerms = termFrequency.toList.filter(x => !stopWords.contains(x._1) && x._2 > 500).map(_._1)
    val features = cartesian(List(topTerms, classes)).toList.map { case List(w, c) => featureFunction(w, c)(_, _) }
    
    this.C = (for ((c, s) <- samples) yield features.map(f => f(s, c)).sum).max
    this.features = features :+ ((doc: Document, docCategory: String) => C - features.map(f => f(doc, docCategory)).sum)
  }
}