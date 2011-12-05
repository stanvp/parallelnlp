package menthor.apps

object Analyzer {
  def termFrequency(text: String) : Map[String, Double] = {
    text.split("(?m)\\s+").foldLeft[Map[String, Double]](Map()) { (frequency, rawTerm) =>
      val term = rawTerm.trim
      if (!term.matches("^[\\w']+$")) {
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