package menthor.apps

import menthor.classifier.Sample
import gnu.trove.map.hash.TIntDoubleHashMap

case class Document(name: String, categories: List[String], termFrequency: TIntDoubleHashMap) extends Sample {
  override def toString()  = "Document(%s,%s)".format(name, categories)
  
  override def hashCode = name.hashCode()
  override def equals(other: Any) = other match {
    case that : String => that == name
    case _ => false
  }
  
  val total: Double = termFrequency.values.sum
  val features : TIntDoubleHashMap = termFrequency
}