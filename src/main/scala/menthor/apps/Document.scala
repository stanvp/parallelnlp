package menthor.apps

import menthor.classifier.Sample

case class Document(name: String, categories: List[String], termFrequency: Map[String, Double]) extends Sample {
  val totalTerms = termFrequency.values.sum
  override def toString()  = "Document(%s,%s)".format(name, categories)
  
  override def total: Double = totalTerms
  override def features : Map[String, Double] = termFrequency
}