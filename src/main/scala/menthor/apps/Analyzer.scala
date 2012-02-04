package menthor.apps

import scala.io.Source
import scala.collection.immutable.HashMap
import gnu.trove.map.hash.TObjectIntHashMap
import gnu.trove.map.hash.TIntDoubleHashMap

/**
 * Tokenize, index and removes stop words from text contents
 * 
 * @author Stanislav Peshterliev
 */
object Analyzer {

  /**
   * Set of stop words for english
   */
  val stopWords = {
    val source = Source.fromURL(getClass.getResource("/english-stopwords.txt"))
    source.getLines().mkString(",").split(",").toSet
  }

  /**
   * Term index structure
   */
  val index = new TObjectIntHashMap[String]()
  
  def addToIndex(term: String) : Int = {
	  val id = index.size + 1
	  index.put(term, id)
	  id
  }

  /**
   * Transforms text content to term to term frequency mapping 
   */
  def termFrequency(text: String, extendIndex: Boolean): TIntDoubleHashMap = {
    val result = new TIntDoubleHashMap

    for (rawTerm <- text.split("(?m)\\s+")) {
      val term = rawTerm.trim.toLowerCase

      if (term.matches("^[\\w']+$") && term.size > 2 && !stopWords.contains(term)) {
        var id = index.get(term)

        if (extendIndex && id == index.getNoEntryValue()) {
          addToIndex(term)
        }

        if (id != index.getNoEntryValue()) {
          var count = result.get(id)
          result.put(id, count + 1)
        }
      }
    }

    result
  }
}