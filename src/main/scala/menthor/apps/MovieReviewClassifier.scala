package menthor.apps

import scala.collection.JavaConversions._
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import menthor.util.FileUtils._
import scala.util.Random
import menthor.classifier.FeatureSelector
import menthor.classifier.naivebayes._
import menthor.classifier.maxent._
import scala.collection.mutable.ListBuffer
import scala.util.logging.ConsoleLogger
import menthor.classifier.featureselector.IGFeatureSelector

object MovieReviewClassifier {
  
  val categories = List("neg", "pos")
  
  def load(folder: String, extendIndex: Boolean): Iterable[Document] = {
    val collection = new ListBuffer[Document]

    forEachFileIn(new File(folder)) {
      file =>
        val document = new Document(
          file.getName(),
          List(file.getParentFile().getName()),
          Analyzer.termFrequency(Source.fromFile(file).getLines().mkString("\n"), extendIndex))
        
        collection += document
    }

    collection.toIterable
  }

  def main(args: Array[String]) {
    if (args.size < 4) {
      println("Please specify [algorithm] [traning mode] [movie review corpus path] [features file]")
      println("algorithm can be: maxent or naivebayes")
      println("traning mode can be: parallel or sequential")
      exit
    }
    
    val features = loadFeatures(args(3))
    
	val collection = Random.shuffle(load(args(2), false))
	
	// split the collection into training and test sets  
	val testSize = (collection.size * 0.1).toInt
	val test = collection.slice(0, testSize - 1)
	
	val train = collection.slice(testSize, collection.size - 1)
		
	val samples = train.view.flatMap(d => 
	  d.categories.map(c => (c, d)) 
	)
	
	val trainer = args.first match {
	  case "maxent" =>
	  	 args(1) match {
	  	   case "parallel" => new MaxentTrainerParallel[Category, Document](3, features) with ConsoleLogger
	  	   case "parallelbatch" => new MaxentTrainerParallelBatch[Category, Document](48, features) with ConsoleLogger
	  	   case "sequential" => new MaxentTrainer[Category, Document](features) with ConsoleLogger
	  	   case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
	  	 }	    
	  case "naivebayes" =>
	  	 args(1) match {
	  	   case "parallel" => new NaiveBayesTrainerParallel[Category, Document](8, features) with ConsoleLogger
	  	   case "sequential" => new NaiveBayesTrainer[Category, Document](features) with ConsoleLogger
	  	   case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
	  	 }
	  case _ => throw new IllegalArgumentException("Illegal algorithm, choose maxent or naivebayes")
	}
	
	val classifier = trainer.train(categories, samples)
	
	println("Evaluation")
	
	var success = 0
	for (d <- test) {
	  val r = classifier.classify(d)
	  
	  if (d.categories.contains(r._1)) {
	    success += 1
	  }
	}
	
	println("Total: " + test.size)
	println("Success: " + success)
	println("Percent: " + ((success / test.size.toFloat) * 100 ) + " %")
  }
}