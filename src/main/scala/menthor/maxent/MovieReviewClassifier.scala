package menthor.maxent

import scala.collection.JavaConversions._
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import util.FileUtils._
import util.CollectionUtils._
import scala.util.Random

object MovieReviewClassifier {
  
  val stopWords = {
    val source = Source.fromURL(getClass.getResource("/common-english-words.txt"))
    source.getLines().mkString(",").split(",").toSet
  }
  
  def load(folder: String): HashMap[String, Document] = {
    val collection = new HashMap[String, Document]

    forEachFileIn(new File(folder)) {
      file =>
        val document = new Document(
          file.getName(),
          file.getParentFile().getName(),
          Analyzer.termFrequency(Source.fromFile(file).getLines().mkString("\n")))
        
        collection.put(document.name, document)
    }

    collection
  }

  def main(args: Array[String]) {
	val collection = load("/home/stanvp/workspace/semesterproject/movie_reviews")
	
	val validationSize = (collection.size * 0.3).toInt
	val start = Random.nextInt(collection.size - validationSize)
	val validation = collection.slice(start, start + validationSize)
	
	val train = collection -- validation.map(_._1)
	
	val categories = List("neg", "pos")
	val samples = train.values.map(d => (d.category, d)).toList
	
	val maxent = Maxent.trainSequential(categories, samples, new DocumentFeatureGenerator(stopWords))
	
	var success = 0
	for ((c,d) <- validation) {
	  val r = maxent.classify(d)
	  
	  if (d.category == r._1) {
	    success += 1
	  }
	}
	
	println("Total: " + validation.size)
	println("Success: " + success)
	println("Percent: " + ((success / validation.size.toFloat) * 100 ) + " %")
  }
}