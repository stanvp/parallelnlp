package menthor.documentclassifier

import menthor.maxent.FeatureTrainer
import menthor.util.CollectionUtils._
import menthor.maxent.FeatureFunction
import menthor.maxent.FeatureGenerator

class DocumentFeatureTrainer(stopWords: Set[String]) extends FeatureTrainer[Category, Document] {
  override def train(categories: List[Category], samples: List[(Category, Document)]) : FeatureGenerator[Category, Document] = {
    val termFrequency = merge(samples.map(_._2.termFrequency))
    val topTerms = termFrequency.toList.filter(x => !stopWords.contains(x._1) && x._2 > 700).map(_._1)
    val features = cartesian(List(topTerms, categories)).toList.map { case List(word, category) => new DocumentFeatureFunction(word, category) }
    
    val C = (for ((category, sample) <- samples) yield features.map(f => f(category, sample)).sum).max
    new DocumentFeatureGenerator(categories, features, C)
  }
}