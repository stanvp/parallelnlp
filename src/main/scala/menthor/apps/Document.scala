package menthor.apps

import menthor.classifier.Sample

case class Document(name: String, categories: List[String], termFrequency: Map[String, Double]) extends Sample {
  override def toString()  = "Document(%s,%s)".format(name, categories)
  
  val total: Double = termFrequency.values.sum
  val features : Map[String, Double] = termFrequency
}