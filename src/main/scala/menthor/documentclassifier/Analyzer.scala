package menthor.documentclassifier

object Analyzer {
  def termFrequency(text: String) = {
    text.split("(?m)\\s+").foldLeft[Map[String, Int]](Map()) { (frequency, rawTerm) =>
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