package menthor.apps
import scala.io.Source

object Analyzer {
  
  val stopWords = {
    val source = Source.fromURL(getClass.getResource("/english-stopwords.txt"))
    source.getLines().mkString(",").split(",").toSet
  }
  
  def termFrequency(text: String) : Map[String, Double] = {
    text.split("(?m)\\s+").foldLeft[Map[String, Double]](Map()) { (frequency, rawTerm) =>
      val term = rawTerm.trim
      if (!term.matches("^[\\w']+$") || term.size < 3 || stopWords.contains(term)) {
        frequency
      } else {
	      frequency.get(term) match {
	        case Some(occurrences) => frequency + (term -> (occurrences + 1))
	        case None => frequency + (term -> 1)
	      }
      }
    }
  }
}