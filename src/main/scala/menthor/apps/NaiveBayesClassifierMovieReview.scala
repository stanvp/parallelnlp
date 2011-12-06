package menthor.apps

import scala.collection.JavaConversions._
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import menthor.util.FileUtils._
import scala.util.Random
import menthor.classifier.naivebayes.NaiveBayesClassifier
import menthor.classifier.naivebayes.NaiveBayesTrainer
import menthor.classifier.naivebayes.NaiveBayesTrainerParallel
import scala.collection.mutable.ListBuffer

object NaiveBayesClassifierMovieReview {
  def load(folder: String): List[Document] = {
    val collection = new ListBuffer[Document]

    forEachFileIn(new File(folder)) {
      file =>
        val document = new Document(
          file.getName(),
          file.getParentFile().getName(),
          Analyzer.termFrequency(Source.fromFile(file).getLines().mkString("\n")))
        
        collection += document
    }

    collection.toList
  }

  def main(args: Array[String]) {
    if (args.size < 2) {
      println("Please specify [traning mode] and [corpus path]")
      exit
    }
    
	val collection = Random.shuffle(load(args(1)))
	
	// split the collection into training and test sets  
	val testSize = (collection.size * 0.1).toInt
	val test = collection.slice(0, testSize - 1)
	
	val train = collection.slice(testSize, collection.size - 1)
	
	val categories = List("neg", "pos")
	val samples = train.map(d => (d.category, d)).toList
	
    var naiveBayes: NaiveBayesClassifier[Category, Document] = null

    if (args.first == "parallel") {
      naiveBayes = NaiveBayesTrainerParallel.train(categories, samples, 5)
    } else if (args.first == "sequential") {
      naiveBayes = NaiveBayesTrainer.train(categories, samples)
    }	
	
	var success = 0
	for (d <- test) {
	  val r = naiveBayes.classify(d)
	  
	  if (d.category == r._1) {
	    success += 1
	  }
	}
	
	println("Total: " + test.size)
	println("Success: " + success)
	println("Percent: " + ((success / test.size.toFloat) * 100 ) + " %")
  }
}