package menthor.apps

import scala.collection.JavaConversions._
import scala.io.Source
import java.io.File
import scala.collection.mutable.HashMap
import menthor.util.FileUtils._
import scala.util.Random
import menthor.classifier.naivebayes.NaiveBayesTrainer
import menthor.classifier.naivebayes.NaiveBayesTrainerParallel
import menthor.classifier.maxent.MaxentTrainer
import menthor.classifier.maxent.MaxentTrainerParallel

import scala.collection.mutable.ListBuffer
import scala.util.logging.ConsoleLogger

object NewsgroupsClassifier {  
  def load(folder: String): Stream[Document] = {
    fileStream(new File(folder)) {
      file =>
        val document = new Document(
          file.getName(),
          List(file.getParentFile().getName()),
          Analyzer.termFrequency(Source.fromFile(file, "Cp1252").getLines().mkString("\n")))
        
        Some(document)
    }
  }  

  def main(args: Array[String]) {
    if (args.size < 3) {
      println("Please specify [algorithm] [traning mode] [movie review corpus path]")
      println("algorithm can be: maxent or naivebayes")
      println("traning mode can be: parallel or sequential")
      exit
    }
    
	 val train = load(args(2) + "/20news-bydate-train")
	
	val categories = List("neg", "pos")
	val samples = train.view.flatMap(d => 
	  d.categories.map(c => (c, d)) 
	)
	
	val trainer = args.first match {
	  case "maxent" =>
	  	 args(1) match {
	  	   case "parallel" => new MaxentTrainerParallel[Category, Document](3) with ConsoleLogger
	  	   case "sequential" => new MaxentTrainer[Category, Document] with ConsoleLogger
	  	   case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
	  	 }	    
	  case "naivebayes" =>
	  	 args(1) match {
	  	   case "parallel" => new NaiveBayesTrainerParallel[Category, Document](5) with ConsoleLogger
	  	   case "sequential" => new NaiveBayesTrainer[Category, Document] with ConsoleLogger
	  	   case _ => throw new IllegalArgumentException("Illegal traning mode, choose parallel or sequential")
	  	 }
	  case _ => throw new IllegalArgumentException("Illegal algorithm, choose maxent or naivebayes")
	}
	
	val classifier = trainer.train(categories, samples)
	
	val test = load(args(2) + "/20news-bydate-test")
	
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