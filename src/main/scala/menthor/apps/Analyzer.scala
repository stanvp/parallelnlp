package menthor.apps

import scala.io.Source
import scala.collection.immutable.HashMap
import gnu.trove.map.hash.TObjectIntHashMap
import gnu.trove.map.hash.TIntDoubleHashMap

object Analyzer {

  val stopWords = {
    val source = Source.fromURL(getClass.getResource("/english-stopwords.txt"))
    source.getLines().mkString(",").split(",").toSet
  }

  val index = new TObjectIntHashMap[String]()

  def termFrequency(text: String): TIntDoubleHashMap = {
    val result = new TIntDoubleHashMap

    for (rawTerm <- text.split("(?m)\\s+")) {
      val term = rawTerm.trim.toLowerCase
      
      if (term.matches("^[\\w']+$") && term.size > 2 && !stopWords.contains(term)) {
        var id = index.get(term)
        
        if (id == index.getNoEntryValue()) {
          id = index.size + 1
          index.put(term, id)
        }
        
        var count = result.get(id)
        result.put(id, count + 1)     
      }
    }

    result
  }
}