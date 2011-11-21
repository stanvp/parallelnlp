package menthor.documentclassifier

import menthor.util.CollectionUtils._
import menthor.classifier.FeatureTrainer
import menthor.classifier.FeatureGenerator
import scalala.operators.Implicits._

class DocumentFeatureTrainer(stopWords: Set[String]) extends FeatureTrainer[Category, Document] {
  
  override def train(categories: List[Category], samples: List[(Category, Document)]) : FeatureGenerator[Category, Document] = {
    val termFrequency = samples.map(_._2.termFrequency).reduce(_ + _)
    val topTerms = termFrequency.toList.filter(x => !stopWords.contains(x._1)).sortWith{(x, y) => x._2 > y._2}.slice(0,100).map(_._1)
    val features = cartesian(List(topTerms, categories)).toList.map { case List(word, category) => new DocumentFeatureFunction(word, category) }
    
    val C = (for ((category, sample) <- samples) yield features.map(f => f(category, sample)).sum).max
    new DocumentFeatureGenerator(categories, features, C)
  }
}